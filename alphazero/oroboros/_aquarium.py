#!/usr/bin/env python3
"""Oroboros Aquarium: persistent evolving population of game-organisms.

Runs continuously, cycling through:
  1. Evaluate unevaluated bacteria
  2. Mutate and evolve the population
  3. Inject random seeds for diversity
  4. Prune the archive to keep only peak representatives
  5. Save state and sleep

The population persists across runs via aquarium_state.json.
The game library (game_library.json) is updated with the best discoveries.

Run:
  python -m alphazero.oroboros.aquarium
  python -m alphazero.oroboros.aquarium --cycles 50    # run 50 cycles then stop
  python -m alphazero.oroboros.aquarium --forever       # run until killed
"""

from __future__ import annotations
import argparse
import json
import math
import os
import random
import time
from dataclasses import dataclass, field
from pathlib import Path

from .schema import RuleSetV3
from .grammar import Grammar
from .game import Game
from .evaluator import GameMetrics, EvalConfig, evaluate
from .library import merge_into_library, load_library

def _evaluate_worker(ruleset: RuleSetV3, config: EvalConfig) -> tuple[float, str]:
    """Worker function for parallel evaluation (must be top-level for pickling)."""
    try:
        m = evaluate(ruleset, config)
        return m.richness_score(), m.summary()
    except Exception as e:
        return 0.0, f"error: {e}"


STATE_PATH = Path("aquarium_state.json")
LIBRARY_PATH = Path("game_library.json")


@dataclass
class Organism:
    """A game-organism in the aquarium."""
    ruleset_dict: dict
    richness:     float = 0.0
    age:          int = 0
    evaluated:    bool = False
    summary:      str = ""
    # Region key for clustering
    region:       str = ""

    def to_dict(self) -> dict:
        return {
            "ruleset": self.ruleset_dict,
            "richness": self.richness,
            "age": self.age,
            "evaluated": self.evaluated,
            "summary": self.summary,
            "region": self.region,
        }

    @staticmethod
    def from_dict(d: dict) -> "Organism":
        return Organism(
            ruleset_dict=d["ruleset"],
            richness=d.get("richness", 0),
            age=d.get("age", 0),
            evaluated=d.get("evaluated", False),
            summary=d.get("summary", ""),
            region=d.get("region", ""),
        )


def _region_key(rs_dict: dict) -> str:
    """Compute a region key for clustering similar games.
    Games in the same region have the same topology + piece count + rule count."""
    schema = rs_dict.get("schema", {})
    return (f"{schema.get('topology_type', '?')}"
            f"_s{schema.get('topology_size', '?')}"
            f"_t{schema.get('num_piece_types', '?')}"
            f"_r{len(rs_dict.get('rules', []))}")


class Aquarium:
    """Persistent evolving population of game-organisms."""

    def __init__(
        self,
        grammar:        Grammar | None = None,
        eval_config:    EvalConfig | None = None,
        max_population: int = 40,
        max_archive:    int = 100,
        seed_rate:      float = 0.1,
        max_per_region: int = 3,     # keep at most N games per region
        n_workers:      int = 1,     # parallel evaluation workers (1 = sequential)
        state_path:     Path = STATE_PATH,
        verbose:        bool = True,
    ):
        self.grammar    = grammar or Grammar(fixed_players=2)
        self.eval_cfg   = eval_config or EvalConfig()
        self.max_pop    = max_population
        self.max_archive = max_archive
        self.seed_rate  = seed_rate
        self.max_per_region = max_per_region
        self.n_workers  = n_workers
        self.state_path = state_path
        self.verbose    = verbose

        self.population: list[Organism] = []
        self.archive:    list[Organism] = []  # best representatives
        self.cycle       = 0
        self.total_evals = 0

    # ── Persistence ───────────────────────────────────────────────────────────

    def save_state(self):
        data = {
            "cycle": self.cycle,
            "total_evals": self.total_evals,
            "population": [o.to_dict() for o in self.population],
            "archive": [o.to_dict() for o in self.archive],
        }
        with open(self.state_path, "w") as f:
            json.dump(data, f, indent=2, default=str)

    def load_state(self):
        if self.state_path.exists():
            with open(self.state_path) as f:
                data = json.load(f)
            self.cycle = data.get("cycle", 0)
            self.total_evals = data.get("total_evals", 0)
            self.population = [Organism.from_dict(d) for d in data.get("population", [])]
            self.archive = [Organism.from_dict(d) for d in data.get("archive", [])]
            self._log(f"Loaded state: cycle {self.cycle}, "
                      f"pop={len(self.population)}, archive={len(self.archive)}, "
                      f"evals={self.total_evals}")
        else:
            self._log("No previous state found, starting fresh")

    # ── Main loop ─────────────────────────────────────────────────────────────

    def run(self, n_cycles: int = 10):
        """Run n_cycles of evolution. Picks up where it left off."""
        self.load_state()

        # Seed if empty
        if not self.population:
            self._seed_population(self.max_pop // 2)

        for _ in range(n_cycles):
            self.cycle += 1
            self._log(f"\n═══ Cycle {self.cycle} ═══ pop={len(self.population)} "
                      f"archive={len(self.archive)} evals={self.total_evals}")

            # 1. Evaluate unevaluated organisms
            self._evaluate_batch()

            # 2. Age everyone
            for o in self.population:
                o.age += 1

            # 3. Evolve: mutate the fit, cull the weak
            self._evolve()

            # 4. Inject random seeds
            n_seed = max(1, int(len(self.population) * self.seed_rate))
            self._seed_population(n_seed)

            # 5. Update archive (keep peak representatives)
            self._update_archive()

            # 6. Export to game library
            self._export_to_library()

            # 7. Save state
            self.save_state()
            self._report()

        self._log(f"\n{'='*60}")
        self._log(f"Aquarium paused after {n_cycles} cycles. "
                  f"Total: {self.total_evals} evaluations, {len(self.archive)} in archive.")
        self._log(f"Resume with: python -m alphazero.oroboros.aquarium")

    # ── Core operations ───────────────────────────────────────────────────────

    def _evaluate_batch(self):
        """Evaluate all unevaluated organisms. Uses parallel workers if n_workers > 1."""
        unevaluated = [o for o in self.population if not o.evaluated]
        if not unevaluated:
            return

        if self.n_workers > 1 and len(unevaluated) > 1:
            self._evaluate_parallel(unevaluated)
        else:
            self._evaluate_sequential(unevaluated)

    def _evaluate_sequential(self, organisms: list[Organism]):
        for o in organisms:
            self._evaluate_one(o)

    def _evaluate_parallel(self, organisms: list[Organism]):
        """Evaluate organisms in parallel using process pool."""
        import multiprocessing as mp
        try:
            rulesets = [RuleSetV3.from_dict(o.ruleset_dict) for o in organisms]
            args = [(rs, self.eval_cfg) for rs in rulesets]
            with mp.Pool(self.n_workers) as pool:
                results = pool.starmap(_evaluate_worker, args)
            for o, (richness, summary) in zip(organisms, results):
                o.richness = richness
                o.summary = summary
                o.evaluated = True
                o.region = _region_key(o.ruleset_dict)
                self.total_evals += 1
                if self.verbose:
                    self._log(f"  eval {o.region} rich={o.richness:.0f}")
        except Exception as e:
            self._log(f"  Parallel eval failed ({e}), falling back to sequential")
            self._evaluate_sequential(organisms)

    def _evaluate_one(self, o: Organism):
        t0 = time.time()
        try:
            rs = RuleSetV3.from_dict(o.ruleset_dict)
            m = evaluate(rs, self.eval_cfg)
            o.richness = m.richness_score()
            o.summary = m.summary()
            o.evaluated = True
            o.region = _region_key(o.ruleset_dict)
        except Exception as e:
            o.richness = 0
            o.evaluated = True
            o.summary = f"error: {e}"
        self.total_evals += 1
        elapsed = time.time() - t0
        if self.verbose:
            self._log(f"  eval {o.region} rich={o.richness:.0f} ({elapsed:.1f}s)")

    def _evolve(self):
        """Mutate the fit, divide the best, cull the worst."""
        if not self.population:
            return

        # Sort by richness
        self.population.sort(key=lambda o: o.richness, reverse=True)

        # Top 50% mutate (biased exploration)
        n_top = max(1, len(self.population) // 2)
        new_organisms = []
        for o in self.population[:n_top]:
            if len(self.population) + len(new_organisms) >= self.max_pop:
                break
            try:
                rs = RuleSetV3.from_dict(o.ruleset_dict)
                child_rs = self.grammar.mutate(rs)
                new_organisms.append(Organism(
                    ruleset_dict=child_rs.to_dict(),
                    region=_region_key(child_rs.to_dict()),
                ))
            except Exception:
                pass

        # Cull bottom 30%
        n_cull = max(0, len(self.population) - self.max_pop + len(new_organisms))
        if n_cull > 0:
            # Remove from the bottom, but keep at least min viable
            self.population = self.population[:len(self.population) - n_cull]

        self.population.extend(new_organisms)

    def _seed_population(self, n: int):
        """Inject n completely random organisms."""
        for _ in range(n):
            if len(self.population) >= self.max_pop:
                break
            rs = self.grammar.sample()
            self.population.append(Organism(
                ruleset_dict=rs.to_dict(),
                region=_region_key(rs.to_dict()),
            ))

    def _update_archive(self):
        """Keep only the best representative per region.

        For each region (topology+size+types+rules), keep the top N by richness.
        This prevents the archive from being dominated by clusters of similar games.
        """
        # Add all evaluated organisms to candidate pool
        candidates = list(self.archive)
        for o in self.population:
            if o.evaluated and o.richness > 0:
                candidates.append(o)

        # Group by region
        by_region: dict[str, list[Organism]] = {}
        for o in candidates:
            region = o.region or _region_key(o.ruleset_dict)
            by_region.setdefault(region, []).append(o)

        # Keep top N per region
        new_archive = []
        for region, organisms in by_region.items():
            organisms.sort(key=lambda o: o.richness, reverse=True)
            new_archive.extend(organisms[:self.max_per_region])

        # Also keep global top regardless of region
        all_sorted = sorted(candidates, key=lambda o: o.richness, reverse=True)
        for o in all_sorted[:20]:
            if o not in new_archive:
                new_archive.append(o)

        # Cap total archive size
        new_archive.sort(key=lambda o: o.richness, reverse=True)
        self.archive = new_archive[:self.max_archive]

    def _export_to_library(self):
        """Export archive to game_library.json for the website."""
        games = []
        for o in self.archive:
            if o.richness > 0:
                games.append({
                    "richness": o.richness,
                    "summary": o.summary,
                    "ruleset": o.ruleset_dict,
                    "region": o.region,
                    "age": o.age,
                })
        added = merge_into_library(games, LIBRARY_PATH)
        if added > 0:
            self._log(f"  Library: +{added} new games (total: {len(load_library(LIBRARY_PATH))})")

    # ── Reporting ─────────────────────────────────────────────────────────────

    def _report(self):
        if not self.verbose:
            return
        # Region summary
        regions: dict[str, list[float]] = {}
        for o in self.archive:
            regions.setdefault(o.region, []).append(o.richness)

        self._log(f"\n  Archive: {len(self.archive)} games in {len(regions)} regions")
        top_regions = sorted(regions.items(), key=lambda x: max(x[1]), reverse=True)
        for region, scores in top_regions[:8]:
            self._log(f"    {region}: best={max(scores):.0f} count={len(scores)}")

        # Top games
        top = sorted(self.archive, key=lambda o: o.richness, reverse=True)[:5]
        self._log(f"  Top games:")
        for o in top:
            self._log(f"    {o.region} rich={o.richness:.0f} age={o.age}")

    def _log(self, msg: str):
        if self.verbose:
            print(msg)


def main():
    parser = argparse.ArgumentParser(description="Oroboros Aquarium: persistent game evolution")
    parser.add_argument("--cycles", type=int, default=10, help="Cycles to run (default 10)")
    parser.add_argument("--forever", action="store_true", help="Run indefinitely")
    parser.add_argument("--pop", type=int, default=30, help="Max population size")
    parser.add_argument("--archive", type=int, default=100, help="Max archive size")
    parser.add_argument("--seed-rate", type=float, default=0.15, help="Fraction of random seeds per cycle")
    parser.add_argument("--az-iters", type=int, default=2, help="AlphaZero training iterations")
    parser.add_argument("--az-sims", type=int, default=8, help="MCTS simulations per move")
    parser.add_argument("--workers", type=int, default=1, help="Parallel evaluation workers")
    args = parser.parse_args()

    # Auto-detect device
    from .evaluator import _resolve_device
    device = _resolve_device("")
    print(f"Device: {device}")
    if device == "cuda":
        try:
            import torch
            print(f"GPU: {torch.cuda.get_device_name(0)}")
        except Exception:
            pass

    cfg = EvalConfig(
        sim_n_games=25, sim_max_steps=500,
        az_iters=args.az_iters, az_sims=args.az_sims, az_games=2,
        device=device,
        az_max_steps=150, az_min_buf=8, az_eval_games=6,
    )

    grammar = Grammar(fixed_players=2)
    aquarium = Aquarium(
        grammar=grammar, eval_config=cfg,
        max_population=args.pop, max_archive=args.archive,
        seed_rate=args.seed_rate, n_workers=args.workers,
    )

    if args.forever:
        print("Aquarium running forever. Ctrl+C to pause.")
        while True:
            try:
                aquarium.run(n_cycles=5)
                print(f"\n  [pause 5s between batches]")
                time.sleep(5)
            except KeyboardInterrupt:
                print("\nAquarium paused. State saved.")
                aquarium.save_state()
                break
    else:
        aquarium.run(n_cycles=args.cycles)


if __name__ == "__main__":
    main()
