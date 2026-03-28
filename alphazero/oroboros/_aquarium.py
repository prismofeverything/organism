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
from . import lineage as lin
from .game import Game
from .evaluator import GameMetrics, EvalConfig, evaluate
from .library import merge_into_library, load_library

def _evaluate_worker(ruleset: RuleSetV3, config: EvalConfig) -> tuple[float, str]:
    """Worker function for parallel evaluation (must be top-level for pickling)."""
    try:
        from .game import Game
        from dataclasses import replace
        game = Game(ruleset)
        cfg = config
        if game.N > 40:
            cfg = replace(cfg, sim_n_games=15, sim_max_steps=300,
                          az_iters=1, az_games=1, az_eval_games=4)
        m = evaluate(ruleset, cfg)
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
    region:       str = ""
    # Gradient tracking
    parent_richness: float = 0.0
    delta:           float = 0.0
    best_delta:      float = 0.0
    # Lineage tracking
    lineage_id:      int = -1      # unique ID in lineage.json
    parent_lineage_id: int = -1    # parent's lineage_id (-1 for seeds)

    def to_dict(self) -> dict:
        return {
            "ruleset": self.ruleset_dict,
            "richness": self.richness,
            "age": self.age,
            "evaluated": self.evaluated,
            "summary": self.summary,
            "region": self.region,
            "parent_richness": self.parent_richness,
            "delta": self.delta,
            "best_delta": self.best_delta,
            "lineage_id": self.lineage_id,
            "parent_lineage_id": self.parent_lineage_id,
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
            parent_richness=d.get("parent_richness", 0),
            delta=d.get("delta", 0),
            best_delta=d.get("best_delta", 0),
            lineage_id=d.get("lineage_id", -1),
            parent_lineage_id=d.get("parent_lineage_id", -1),
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
        self.archive:    list[Organism] = []
        self.cycle       = 0
        self.total_evals = 0
        self.lineage     = lin.load_lineage()

    # ── Persistence ───────────────────────────────────────────────────────────

    def save_state(self):
        # Update lineage alive status
        lin.mark_all_dead(self.lineage)
        for o in self.population:
            if o.lineage_id >= 0:
                lin.mark_alive(self.lineage, o.lineage_id)
        lin.save_lineage(self.lineage)

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

            # 1. Evolve: mutate the fit, cull the weak (creates unevaluated children)
            self._evolve()

            # 2. Inject random seeds (also unevaluated)
            n_seed = max(1, int(len(self.population) * self.seed_rate))
            self._seed_population(n_seed)

            # 3. Evaluate all unevaluated organisms (children + seeds)
            self._evaluate_batch()

            # 4. Age everyone
            for o in self.population:
                o.age += 1

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

    def _evaluate_one(self, o: Organism, timeout: float = 120.0):
        t0 = time.time()
        try:
            rs = RuleSetV3.from_dict(o.ruleset_dict)
            # Use a shorter sim config for large boards to avoid timeouts
            cfg = self.eval_cfg
            game = Game(rs)
            if game.N > 40:
                # Large board: reduce sim games and skip AZ
                from dataclasses import replace
                cfg = replace(cfg, sim_n_games=15, sim_max_steps=300,
                              az_iters=1, az_games=1, az_eval_games=4)
            m = evaluate(rs, cfg)
            o.richness = m.richness_score()
            o.summary = m.summary()
            o.evaluated = True
            o.region = _region_key(o.ruleset_dict)
            o.delta = o.richness - o.parent_richness
            o.best_delta = max(o.best_delta, o.delta)
            if o.lineage_id >= 0:
                lin.update_richness(self.lineage, o.lineage_id, o.richness)
        except Exception as e:
            o.richness = 0
            o.delta = -o.parent_richness
            o.evaluated = True
            o.summary = f"error: {e}"
        self.total_evals += 1
        elapsed = time.time() - t0
        if self.verbose:
            delta_str = f" Δ={o.delta:+.0f}" if o.parent_richness > 0 else ""
            self._log(f"  eval {o.region} rich={o.richness:.0f}{delta_str} ({elapsed:.1f}s)")

    def _niche_fitness(self, o: Organism) -> float:
        """Fitness that rewards gradient, diversity, and absolute richness.

        Three signals combined:
          1. Richness (altitude): how good is this game absolutely?
          2. Gradient (slope): did this mutation IMPROVE over its parent?
             A Δ=+30 jump from a mediocre parent is more promising than
             sitting at a known peak with Δ=0.
          3. Diversity (loneliness): fewer neighbors in the same region = bonus.
             Explores underrepresented areas of game space.

        This ensures:
          - Known peaks are retained (high richness)
          - Rising slopes are followed (positive delta)
          - Unexplored territory is valued (low region count)
        """
        region = o.region or _region_key(o.ruleset_dict)

        # How many others in the same region?
        same_region = sum(1 for p in self.population
                          if (p.region or _region_key(p.ruleset_dict)) == region)

        # 1. Richness (50% weight): absolute quality
        richness_score = o.richness

        # 2. Gradient (30% weight): reward positive deltas strongly,
        #    and penalize negative deltas mildly
        if o.delta > 0:
            # Positive improvement — scale by relative magnitude
            gradient_score = o.delta * 3.0
        elif o.best_delta > 0:
            # This mutation was bad but the lineage has shown promise
            gradient_score = o.best_delta * 0.5
        else:
            gradient_score = 0.0

        # 3. Diversity (20% weight): lonely regions get a bonus
        diversity_score = 80.0 / max(same_region, 1)

        return richness_score + gradient_score + diversity_score

    def _evolve(self):
        """Adaptive evolution: cull rate adapts to population health.

        - If many children improve over parents → smaller cull (exploitation)
        - If few children improve → larger cull + more seeds (exploration)
        - Young organisms get immunity (min 2 cycles before eligible for cull)
        - Best-in-topology organisms are immune from culling
        """
        if not self.population:
            return

        # Measure population health: what fraction of recent children improved?
        recent_children = [o for o in self.population
                           if o.parent_richness > 0 and o.age <= 3]
        if recent_children:
            improving = sum(1 for o in recent_children if o.delta > 0)
            health = improving / len(recent_children)
        else:
            health = 0.5

        # Adaptive cull rate: healthy population → gentle cull, sick → aggressive
        cull_fraction = 0.15 + 0.15 * (1.0 - health)  # 15-30%
        self._log(f"  health={health:.2f} cull_rate={cull_fraction:.0%}")

        # Sort by niche fitness
        self.population.sort(key=lambda o: self._niche_fitness(o), reverse=True)

        # Protect: young organisms (age < 2) and best-per-topology are immune
        protected = set()
        best_by_topo: dict[str, float] = {}
        for o in self.population:
            topo = o.ruleset_dict.get("schema", {}).get("topology_type", "?")
            if topo not in best_by_topo or o.richness > best_by_topo[topo]:
                best_by_topo[topo] = o.richness
                protected.add(id(o))
            if o.age < 2:
                protected.add(id(o))

        # Cull from the bottom, skipping protected
        n_cull = max(1, int(len(self.population) * cull_fraction))
        new_pop = []
        culled = 0
        for o in self.population:
            if culled >= n_cull or id(o) in protected:
                new_pop.append(o)
            elif len(self.population) - culled <= len(best_by_topo):
                # Don't cull below minimum viable (one per topology)
                new_pop.append(o)
            else:
                culled += 1
        # Reverse because we cull from the sorted bottom
        # Actually we need to cull from the END (lowest fitness)
        new_pop = []
        cull_candidates = list(reversed(self.population))
        culled = 0
        for o in cull_candidates:
            if culled < n_cull and id(o) not in protected:
                culled += 1
            else:
                new_pop.append(o)
        self.population = list(reversed(new_pop))
        self._log(f"  culled {culled} (protected {len(protected)}), pop now {len(self.population)}")

        # Top 50% of survivors produce children
        n_top = max(1, len(self.population) // 2)
        new_organisms = []
        for o in self.population[:n_top]:
            if len(self.population) + len(new_organisms) >= self.max_pop:
                break
            try:
                rs = RuleSetV3.from_dict(o.ruleset_dict)
                # High-richness parents get gentle mutation (preserve what works)
                # Low-richness parents get normal mutation (explore more)
                gentle = o.richness > 50
                child_rs = self.grammar.mutate(rs, gentle=gentle)
                child_dict = child_rs.to_dict()
                child_region = _region_key(child_dict)
                child_lid = lin.record_birth(
                    self.lineage, o.lineage_id, self.cycle, child_dict, region=child_region)
                new_organisms.append(Organism(
                    ruleset_dict=child_dict,
                    region=child_region,
                    parent_richness=o.richness,
                    best_delta=o.best_delta,
                    lineage_id=child_lid,
                    parent_lineage_id=o.lineage_id,
                ))
            except Exception:
                pass

        self.population.extend(new_organisms)
        self._log(f"  +{len(new_organisms)} children, pop now {len(self.population)}")

    def _seed_population(self, n: int):
        """Inject random organisms, biased toward underrepresented topologies."""
        # Count current topology distribution
        topo_counts = {}
        for o in self.population:
            t = o.ruleset_dict.get("schema", {}).get("topology_type", "?")
            topo_counts[t] = topo_counts.get(t, 0) + 1

        all_topos = ["square", "hex", "triangle", "radial", "torus_square", "torus_hex"]
        for _ in range(n):
            if len(self.population) >= self.max_pop:
                break
            # Pick underrepresented topology 50% of the time
            if random.random() < 0.5 and topo_counts:
                min_count = min(topo_counts.get(t, 0) for t in all_topos)
                rare_topos = [t for t in all_topos if topo_counts.get(t, 0) <= min_count + 1]
                target_topo = random.choice(rare_topos)
                # Sample until we get the right topology (up to 10 tries)
                for _ in range(10):
                    rs = self.grammar.sample()
                    if rs.schema.topology_type == target_topo:
                        break
                else:
                    rs = self.grammar.sample()
            else:
                rs = self.grammar.sample()
            rd = rs.to_dict()
            region = _region_key(rd)
            lid = lin.record_birth(self.lineage, -1, self.cycle, rd, region=region)
            self.population.append(Organism(
                ruleset_dict=rd,
                region=region,
                lineage_id=lid,
                parent_lineage_id=-1,
            ))
            t = rd.get("schema", {}).get("topology_type", "?")
            topo_counts[t] = topo_counts.get(t, 0) + 1

    def _update_archive(self):
        """Keep peak representatives per region, ensuring topology diversity.

        Two-level diversity:
          1. Per exact region (topology+size+types+rules): keep top N
          2. Per topology family: ensure every topology has representation
        """
        candidates = list(self.archive)
        for o in self.population:
            if o.evaluated and o.richness > 0:
                candidates.append(o)

        if not candidates:
            return

        # Group by exact region
        by_region: dict[str, list[Organism]] = {}
        for o in candidates:
            region = o.region or _region_key(o.ruleset_dict)
            by_region.setdefault(region, []).append(o)

        # Keep top N per exact region
        new_archive = []
        for region, organisms in by_region.items():
            organisms.sort(key=lambda o: o.richness, reverse=True)
            new_archive.extend(organisms[:self.max_per_region])

        # Ensure topology diversity: at least 5 games per topology family
        by_topo: dict[str, list[Organism]] = {}
        for o in candidates:
            t = o.ruleset_dict.get("schema", {}).get("topology_type", "?")
            by_topo.setdefault(t, []).append(o)

        for topo, organisms in by_topo.items():
            organisms.sort(key=lambda o: o.richness, reverse=True)
            # Add top games from this topology if underrepresented
            current = sum(1 for o in new_archive
                          if o.ruleset_dict.get("schema", {}).get("topology_type") == topo)
            for o in organisms[:max(5, self.max_per_region)]:
                if current >= 5:
                    break
                if o not in new_archive:
                    new_archive.append(o)
                    current += 1

        # Also keep global top 10 regardless
        all_sorted = sorted(candidates, key=lambda o: o.richness, reverse=True)
        for o in all_sorted[:10]:
            if o not in new_archive:
                new_archive.append(o)

        # Cap and sort
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

        # Topology diversity in population
        pop_topos: dict[str, int] = {}
        for o in self.population:
            t = o.ruleset_dict.get("schema", {}).get("topology_type", "?")
            pop_topos[t] = pop_topos.get(t, 0) + 1
        self._log(f"\n  Population diversity: " +
                  " ".join(f"{t}={c}" for t, c in sorted(pop_topos.items())))

        # Archive summary
        regions: dict[str, list[float]] = {}
        arch_topos: dict[str, int] = {}
        for o in self.archive:
            r = o.region or _region_key(o.ruleset_dict)
            regions.setdefault(r, []).append(o.richness)
            t = o.ruleset_dict.get("schema", {}).get("topology_type", "?")
            arch_topos[t] = arch_topos.get(t, 0) + 1

        self._log(f"  Archive: {len(self.archive)} games, {len(regions)} regions")
        self._log(f"  Archive topologies: " +
                  " ".join(f"{t}={c}" for t, c in sorted(arch_topos.items())))

        # Age distribution
        ages = [o.age for o in self.population]
        if ages:
            avg_age = sum(ages) / len(ages)
            max_age = max(ages)
            self._log(f"  Ages: avg={avg_age:.1f} max={max_age}")

        # Top games by richness
        top = sorted(self.archive, key=lambda o: o.richness, reverse=True)[:5]
        self._log(f"  Top by richness:")
        for o in top:
            self._log(f"    {o.region} rich={o.richness:.0f} Δ={o.delta:+.0f} age={o.age}")

        # Top games by gradient (steepest recent improvement)
        rising = sorted([o for o in self.population if o.delta > 0],
                        key=lambda o: o.delta, reverse=True)[:3]
        if rising:
            self._log(f"  Rising (best gradient):")
            for o in rising:
                self._log(f"    {o.region} rich={o.richness:.0f} Δ={o.delta:+.0f} (parent={o.parent_richness:.0f})")

    def _log(self, msg: str):
        if self.verbose:
            print(msg)


def main():
    parser = argparse.ArgumentParser(description="Oroboros Aquarium: persistent game evolution")
    parser.add_argument("--cycles", type=int, default=10, help="Cycles to run (default 10)")
    parser.add_argument("--forever", action="store_true", help="Run indefinitely")
    parser.add_argument("--pop", type=int, default=100, help="Max population size")
    parser.add_argument("--archive", type=int, default=200, help="Max archive size")
    parser.add_argument("--seed-rate", type=float, default=0.15, help="Fraction of random seeds per cycle")
    parser.add_argument("--az-iters", type=int, default=2, help="AlphaZero training iterations")
    parser.add_argument("--az-sims", type=int, default=8, help="MCTS simulations per move")
    parser.add_argument("--workers", type=int, default=4, help="Parallel evaluation workers")
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
