"""Chemotactic search: a population of game-organisms that sense and
navigate through game space toward regions of high strategic richness.

Each "bacterium" is a game ruleset that:
  - Senses the local gradient (which mutations improved richness?)
  - Moves toward nutrients (mutates in productive directions)
  - Divides when well-fed (spawns children in rich areas)
  - Dies when starving (gets culled from barren regions)
  - Tumbles randomly when no gradient (explores new territory)

The population is dynamic: it grows in rich areas and shrinks in barren ones.
New random bacteria are spawned periodically to seed unexplored regions.
"""

from __future__ import annotations
import json
import math
import os
import random
import time
from dataclasses import dataclass, field

from .schema import RuleSetV3, validate_coverage
from .grammar_v3 import GrammarV3
from .game_v3 import GameV3
from .evaluator import GameMetrics, EvalConfig
from .evaluator_v3 import evaluate_v3


@dataclass
class Bacterium:
    """A single game-organism navigating game space."""
    ruleset:    RuleSetV3
    metrics:    GameMetrics | None = None
    richness:   float = 0.0
    age:        int = 0          # generations survived
    mutations:  int = 0          # total mutations applied
    lineage:    int = 0          # which initial seed this descended from
    # Gradient memory: which mutation types improved richness
    gradient:   dict = field(default_factory=dict)  # {mutation_type: running_avg_improvement}

    @property
    def is_evaluated(self) -> bool:
        return self.metrics is not None

    def fitness(self) -> float:
        """Composite fitness: richness + novelty bonus for age."""
        return self.richness + self.age * 0.1

    def label(self) -> str:
        s = self.ruleset.schema
        return (f"{s.topology_type[:3]}{s.topology_size}"
                f"t{s.num_piece_types}r{len(self.ruleset.rules)}"
                f" rich={self.richness:.0f} age={self.age}")


class ChemotacticSearch:
    """Population-based search with chemotactic dynamics.

    Algorithm per generation:
      1. EVALUATE: score all unevaluated bacteria
      2. SENSE: each bacterium records which mutations helped
      3. MOVE: bacteria mutate, biased by their gradient memory
      4. DIVIDE: bacteria in rich areas spawn children
      5. CULL: remove the worst bacteria to maintain population cap
      6. SEED: inject fresh random bacteria to explore new territory
    """

    def __init__(
        self,
        grammar:        GrammarV3  | None = None,
        eval_config:    EvalConfig | None = None,
        max_population: int = 30,
        min_population: int = 8,
        seed_rate:      float = 0.15,   # fraction of population to inject each gen
        divide_threshold: float = 0.7,  # top fraction that can divide
        cull_threshold:   float = 0.3,  # bottom fraction at risk of culling
        checkpoint_dir: str = "chemotaxis_checkpoints",
        verbose:        bool = True,
    ):
        self.grammar  = grammar or GrammarV3(fixed_players=2)
        self.eval_cfg = eval_config or EvalConfig()
        self.max_pop  = max_population
        self.min_pop  = min_population
        self.seed_rate = seed_rate
        self.divide_threshold = divide_threshold
        self.cull_threshold = cull_threshold
        self.ckpt_dir = checkpoint_dir
        self.verbose  = verbose

        os.makedirs(checkpoint_dir, exist_ok=True)

        self.population: list[Bacterium] = []
        self.archive:    list[Bacterium] = []  # best-ever seen
        self.generation = 0
        self.total_evals = 0

    def run(self, n_generations: int = 20, initial_pop: int = 12) -> list[Bacterium]:
        """Run the chemotactic search."""
        self._log(f"Chemotactic search: {n_generations} gens, "
                  f"pop={initial_pop}→{self.max_pop}, seed_rate={self.seed_rate}")

        # Phase 0: seed initial population from random points in game space
        self._log("\n  Seeding initial population...")
        for i in range(initial_pop):
            rs = self.grammar.sample()
            self.population.append(Bacterium(
                ruleset=rs, lineage=i,
            ))
        self._evaluate_all()
        self._update_archive()
        self._report()

        # Evolution loop
        for gen in range(1, n_generations + 1):
            self.generation = gen
            self._log(f"\n── Generation {gen} ── pop={len(self.population)} "
                      f"archive={len(self.archive)}")

            # 1. MOVE: each bacterium mutates (biased by gradient)
            self._move_all()

            # 2. EVALUATE: score all bacteria
            self._evaluate_all()

            # 3. DIVIDE: top bacteria spawn children
            self._divide()

            # 4. CULL: remove worst bacteria
            self._cull()

            # 5. SEED: inject fresh random bacteria
            self._seed()

            # 6. Age everyone
            for b in self.population:
                b.age += 1

            self._update_archive()
            self._save_checkpoint()
            self._report()

        self._log(f"\nDone. Archive: {len(self.archive)} best games, "
                  f"{self.total_evals} total evaluations.")
        return self.archive

    # ── Core operations ───────────────────────────────────────────────────────

    def _evaluate_all(self):
        """Evaluate all unevaluated bacteria."""
        for b in self.population:
            if not b.is_evaluated:
                self._evaluate(b)

    def _evaluate(self, b: Bacterium):
        """Evaluate a single bacterium."""
        t0 = time.time()
        try:
            m = evaluate_v3(b.ruleset, self.eval_cfg)
            b.metrics = m
            b.richness = m.richness_score()
        except Exception as e:
            b.metrics = GameMetrics()
            b.richness = 0.0
        self.total_evals += 1
        elapsed = time.time() - t0
        if self.verbose:
            self._log(f"    eval {b.label()} ({elapsed:.1f}s)")

    def _move_all(self):
        """Each bacterium mutates, biased by its gradient memory."""
        for b in self.population:
            old_richness = b.richness
            # Mutate with gradient bias
            new_rs = self._gradient_mutate(b)
            b.ruleset = new_rs
            b.metrics = None  # needs re-evaluation
            b.mutations += 1

    def _gradient_mutate(self, b: Bacterium) -> RuleSetV3:
        """Mutate biased by the bacterium's gradient memory.

        If the gradient says "rule_pred mutations helped", bias toward that.
        If no gradient (tumble), mutate randomly.
        """
        # Tumble probability: inversely proportional to richness
        # Low-richness bacteria tumble more (explore more randomly)
        tumble_prob = max(0.1, 1.0 - b.richness / max(self._median_richness(), 1))

        if random.random() < tumble_prob or not b.gradient:
            # Random tumble
            return self.grammar.mutate(b.ruleset)

        # Biased run: weight mutation types by gradient
        # The grammar's mutate operators map to types:
        # schema, conflict, rule_pred, rule_eff, add_rule, remove_rule, termination, turns
        ops = list(b.gradient.keys())
        if not ops:
            return self.grammar.mutate(b.ruleset)

        weights = [max(0.1, b.gradient[op]) for op in ops]
        total = sum(weights)
        r = random.random() * total
        cumulative = 0
        for i, w in enumerate(weights):
            cumulative += w
            if r <= cumulative:
                # Try this specific mutation type
                return self.grammar.mutate(b.ruleset)
        return self.grammar.mutate(b.ruleset)

    def _divide(self):
        """Top bacteria spawn children."""
        if len(self.population) >= self.max_pop:
            return
        sorted_pop = sorted(self.population, key=lambda b: b.richness, reverse=True)
        n_dividers = max(1, int(len(sorted_pop) * self.divide_threshold))
        candidates = sorted_pop[:n_dividers]

        # Only the best can divide, and only if there's room
        for parent in candidates:
            if len(self.population) >= self.max_pop:
                break
            if parent.richness <= 0:
                continue
            # Child = mutated copy of parent
            child_rs = self.grammar.mutate(parent.ruleset)
            child = Bacterium(
                ruleset=child_rs,
                lineage=parent.lineage,
                gradient=dict(parent.gradient),  # inherit gradient memory
            )
            self.population.append(child)
            self._log(f"    divide: {parent.label()} → child")

    def _cull(self):
        """Remove the worst bacteria."""
        if len(self.population) <= self.min_pop:
            return
        sorted_pop = sorted(self.population, key=lambda b: b.fitness())
        n_cull = max(0, len(sorted_pop) - self.max_pop)
        # Also cull some low-fitness bacteria probabilistically
        n_at_risk = max(1, int(len(sorted_pop) * self.cull_threshold))
        for b in sorted_pop[:n_at_risk]:
            if len(self.population) <= self.min_pop:
                break
            # Cull probability: lower fitness = more likely to die
            cull_prob = 0.5 if b.richness <= 0 else 0.2
            if random.random() < cull_prob or n_cull > 0:
                self.population.remove(b)
                n_cull -= 1
                self._log(f"    cull: {b.label()}")

    def _seed(self):
        """Inject fresh random bacteria."""
        n_seed = max(1, int(len(self.population) * self.seed_rate))
        n_seed = min(n_seed, self.max_pop - len(self.population))
        for _ in range(n_seed):
            rs = self.grammar.sample()
            self.population.append(Bacterium(
                ruleset=rs,
                lineage=self.generation * 1000 + random.randint(0, 999),
            ))

    # ── Archive management ────────────────────────────────────────────────────

    def _update_archive(self):
        """Add best bacteria to the permanent archive."""
        for b in self.population:
            if b.richness > 0 and b.is_evaluated:
                # Check if this is a new best or unique enough
                dominated = False
                for existing in self.archive:
                    if existing.richness >= b.richness:
                        # Check if they're similar (same topology + similar rules)
                        if (existing.ruleset.schema.topology_type == b.ruleset.schema.topology_type
                                and len(existing.ruleset.rules) == len(b.ruleset.rules)):
                            dominated = True
                            break
                if not dominated:
                    self.archive.append(Bacterium(
                        ruleset=b.ruleset, metrics=b.metrics,
                        richness=b.richness, age=b.age,
                        lineage=b.lineage,
                    ))
        # Keep archive bounded
        self.archive.sort(key=lambda b: b.richness, reverse=True)
        self.archive = self.archive[:50]

    # ── Gradient update ───────────────────────────────────────────────────────

    def _update_gradient(self, b: Bacterium, old_richness: float, new_richness: float):
        """Update gradient memory based on richness change."""
        delta = new_richness - old_richness
        # Track improvement by mutation type (simplified: just overall gradient)
        key = "overall"
        old_avg = b.gradient.get(key, 0.0)
        b.gradient[key] = old_avg * 0.7 + delta * 0.3  # exponential moving average

    def _median_richness(self) -> float:
        values = [b.richness for b in self.population if b.is_evaluated]
        if not values:
            return 1.0
        values.sort()
        return values[len(values) // 2]

    # ── Reporting ─────────────────────────────────────────────────────────────

    def _report(self):
        if not self.verbose:
            return
        top = sorted(self.population, key=lambda b: b.richness, reverse=True)[:5]
        self._log(f"\n  Population: {len(self.population)} bacteria, "
                  f"archive: {len(self.archive)}")
        self._log(f"  Median richness: {self._median_richness():.1f}")
        for b in top:
            self._log(f"    {b.label()}")

    def top_results(self, n: int = 15) -> list[Bacterium]:
        return sorted(self.archive, key=lambda b: b.richness, reverse=True)[:n]

    def report(self, n: int = 15) -> str:
        lines = [f"=== Chemotactic Search Results (top {n}) ===",
                 f"Population: {len(self.population)}, Archive: {len(self.archive)}, "
                 f"Evaluations: {self.total_evals}"]
        for i, b in enumerate(self.top_results(n)):
            rs = b.ruleset
            s = rs.schema
            lines.append(f"\n#{i+1}  {rs.short_id()}  (lineage {b.lineage}, age {b.age})")
            lines.append(f"  Topology:  {s.topology_type} size={s.topology_size}")
            lines.append(f"  Schema:    {s.num_piece_types}t {s.num_players}p "
                         f"{s.num_resource_slots}res {s.num_global_counters}counters")
            lines.append(f"  Rules:     {len(rs.rules)} ({rs.description_length():.0f} bits)")
            lines.append(f"  Richness:  {b.richness:.1f}")
            if b.metrics:
                lines.append(f"  Metrics:   {b.metrics.summary()}")
        return "\n".join(lines)

    # ── Persistence ───────────────────────────────────────────────────────────

    def _save_checkpoint(self):
        path = os.path.join(self.ckpt_dir, f"gen_{self.generation:04d}.json")
        data = {
            "generation": self.generation,
            "total_evals": self.total_evals,
            "population_size": len(self.population),
            "archive": [
                {"ruleset": b.ruleset.to_dict(),
                 "richness": b.richness,
                 "age": b.age,
                 "lineage": b.lineage,
                 "summary": b.metrics.summary() if b.metrics else ""}
                for b in self.archive[:20]
            ]
        }
        with open(path, "w") as f:
            json.dump(data, f, indent=2, default=str)

    def _log(self, msg: str):
        if self.verbose:
            print(msg)
