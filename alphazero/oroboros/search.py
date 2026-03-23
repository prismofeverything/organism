"""Pareto evolutionary search over the ruleset space.

Objectives (all maximised after normalisation):
  O1  depth_score         (AlphaZero Elo gain)          — strategic richness
  O2  complexity_ratio    (log_game_tree / description_length)
  O3  balance_score       (even win distribution)
  O4  interaction_rate    (opponent moves affect your options)
  O5  mechanism_coverage  (all action types used)
  O6  -description_length (prefer simpler rules, so negate)

A result R dominates S when R is ≥ S on ALL objectives and > S on at least one.
The Pareto front is the non-dominated set — each represents a distinct trade-off.

Algorithm: (μ+λ) evolutionary strategy with Pareto archive.
  1. Initialise population by random sampling from Grammar.
  2. Evaluate each individual.
  3. Add all to Pareto archive (keep non-dominated set).
  4. Evolve: select parents from archive, apply mutation/crossover → λ children.
  5. Evaluate children, update archive.
  6. Repeat for n_generations.

Enhancements:
  - Adaptive metric weights: learn which objectives distinguish the Pareto front
  - Guided mutation: track which mutations lead to improvements, bias future mutations
  - Surrogate model: pre-screen candidates using a cheap predictor
  - Local refinement: exhaustive 1-mutation neighborhood search around top rulesets
"""

from __future__ import annotations
import copy
import json
import math
import os
import random
import time
from dataclasses import dataclass, field
from typing import Any, Callable

import numpy as np

from .ruleset import RuleSet
from .grammar import Grammar
from .abstract_game import AbstractGame
from .evaluator import GameMetrics, EvalConfig, evaluate


# ── SearchResult ──────────────────────────────────────────────────────────────

@dataclass
class SearchResult:
    ruleset:  RuleSet
    metrics:  GameMetrics
    gen:      int = 0

    def objectives(self) -> dict[str, float]:
        m = self.metrics
        return {
            "depth":       m.depth_score(),
            "complexity":  m.complexity_ratio(),
            "balance":     m.sim.balance_score,
            "interaction": m.sim.interaction_rate,
            "mechanism":   m.sim.mechanism_coverage,
            "simplicity":  -m.sim.description_length,
        }

    def richness(self, weights: dict[str, float] | None = None) -> float:
        return self.metrics.richness_score(weights)

    def label(self) -> str:
        o = self.objectives()
        return (
            f"{self.ruleset.short_id()}  "
            f"depth={o['depth']:.0f}  "
            f"KR={-o['simplicity']:.1f}b  "
            f"ratio={o['complexity']:.2f}  "
            f"bal={o['balance']:.2f}"
        )


# ── Pareto helpers ────────────────────────────────────────────────────────────

def pareto_dominates(a: SearchResult, b: SearchResult) -> bool:
    """True iff a weakly dominates b on all objectives and strictly on ≥1."""
    oa, ob = a.objectives(), b.objectives()
    at_least_one_better = False
    for k in oa:
        if oa[k] < ob[k]:
            return False
        if oa[k] > ob[k]:
            at_least_one_better = True
    return at_least_one_better


def compute_pareto_front(results: list[SearchResult]) -> list[SearchResult]:
    """Return non-dominated subset of results."""
    front: list[SearchResult] = []
    for r in results:
        if not any(pareto_dominates(o, r) for o in results if o is not r):
            front.append(r)
    return front


# ── Adaptive Weights ──────────────────────────────────────────────────────────

class AdaptiveWeights:
    """Learn which richness-score components distinguish good games.

    Uses Fisher discriminant: objectives where the Pareto front differs most
    from dominated results get higher weight.
    """

    # Map from richness component names to objective keys
    _RICHNESS_TO_OBJ = {
        "elo_gain":                   "depth",
        "log_game_tree":              "complexity",
        "interaction_rate":           "interaction",
        "mechanism_coverage":         "mechanism",
        "mechanism_coverage_trained": "mechanism",  # related signal
        "balance_score":              "balance",
    }

    def __init__(self, learning_rate: float = 0.15, min_weight: float = 0.05):
        self.lr = learning_rate
        self.min_weight = min_weight
        # Start with default weights from GameMetrics
        self.weights = dict(GameMetrics._DEFAULT_WEIGHTS)
        self.history: list[dict[str, float]] = [dict(self.weights)]

    def update(self, archive: list[SearchResult], all_results: list[SearchResult]):
        """Update weights based on what distinguishes the Pareto front."""
        if len(archive) < 3 or len(all_results) < 6:
            return  # not enough data

        dominated = [r for r in all_results if r not in archive]
        if len(dominated) < 3:
            return

        # Compute discriminative power of each objective
        front_objs = [r.objectives() for r in archive]
        dom_objs   = [r.objectives() for r in dominated]
        keys = list(front_objs[0].keys())

        discriminative: dict[str, float] = {}
        for k in keys:
            front_vals = [o[k] for o in front_objs]
            dom_vals   = [o[k] for o in dom_objs]
            mean_f = np.mean(front_vals)
            mean_d = np.mean(dom_vals)
            std_all = np.std(front_vals + dom_vals) + 1e-8
            discriminative[k] = (mean_f - mean_d) / std_all

        # Softmax over discriminative scores → target distribution
        disc_arr = np.array([max(discriminative.get(k, 0), 0) for k in keys])
        if disc_arr.sum() < 1e-10:
            return
        disc_arr = disc_arr / disc_arr.sum()
        target = {k: float(v) for k, v in zip(keys, disc_arr)}

        # Blend into current weights
        for w_key, o_key in self._RICHNESS_TO_OBJ.items():
            if o_key in target:
                old = self.weights.get(w_key, 1.0)
                # Scale target to be in same magnitude as current weights
                total_weight = sum(self.weights.values())
                new_target = target[o_key] * total_weight
                self.weights[w_key] = max(
                    self.min_weight,
                    (1 - self.lr) * old + self.lr * new_target,
                )

        self.history.append(dict(self.weights))

    def to_dict(self) -> dict:
        return {"weights": self.weights, "history": self.history}

    def summary(self) -> str:
        items = sorted(self.weights.items(), key=lambda x: -x[1])
        return " ".join(f"{k}={v:.2f}" for k, v in items)


# ── Mutation Tracker ──────────────────────────────────────────────────────────

@dataclass
class MutationRecord:
    field: str
    old_value: Any
    new_value: Any
    improved_pareto: bool = False
    richness_delta:  float = 0.0


class MutationTracker:
    """Track which mutations lead to improvements, bias future exploration.

    Maintains exponential moving averages of success rate per field and
    per (field, target_value) pair.
    """

    def __init__(self, decay: float = 0.9, smoothing: float = 1.0):
        self.decay = decay
        self.smoothing = smoothing  # Laplace smoothing for sparse fields

        self.records: list[MutationRecord] = []

        # Running scores (higher = more productive)
        self._field_success:   dict[str, float] = {}
        self._field_attempts:  dict[str, float] = {}
        self._value_success:   dict[str, dict[Any, float]] = {}
        self._value_attempts:  dict[str, dict[Any, float]] = {}

    def record(self, rec: MutationRecord):
        self.records.append(rec)
        if not rec.field:
            return

        # Decay old observations
        for k in self._field_success:
            self._field_success[k]  *= self.decay
            self._field_attempts[k] *= self.decay
        for f in self._value_success:
            for v in self._value_success[f]:
                self._value_success[f][v]  *= self.decay
                self._value_attempts[f][v] *= self.decay

        # Record new observation
        signal = 1.0 if rec.improved_pareto else 0.0
        self._field_success.setdefault(rec.field, 0.0)
        self._field_attempts.setdefault(rec.field, 0.0)
        self._field_success[rec.field]  += signal
        self._field_attempts[rec.field] += 1.0

        self._value_success.setdefault(rec.field, {})
        self._value_attempts.setdefault(rec.field, {})
        self._value_success[rec.field].setdefault(rec.new_value, 0.0)
        self._value_attempts[rec.field].setdefault(rec.new_value, 0.0)
        self._value_success[rec.field][rec.new_value]  += signal
        self._value_attempts[rec.field][rec.new_value] += 1.0

    def field_weights(self) -> dict[str, float]:
        """Bias for field selection: success_rate + smoothing."""
        weights: dict[str, float] = {}
        for f in self._field_attempts:
            rate = (self._field_success[f] + self.smoothing) / (
                    self._field_attempts[f] + 2 * self.smoothing)
            weights[f] = rate
        return weights

    def value_weights(self) -> dict[str, dict[Any, float]]:
        """Bias for value selection within each field."""
        result: dict[str, dict[Any, float]] = {}
        for f in self._value_attempts:
            result[f] = {}
            for v in self._value_attempts[f]:
                rate = (self._value_success[f][v] + self.smoothing) / (
                        self._value_attempts[f][v] + 2 * self.smoothing)
                result[f][v] = rate
        return result

    def summary(self) -> str:
        if not self._field_attempts:
            return "(no mutations tracked)"
        fw = self.field_weights()
        ranked = sorted(fw.items(), key=lambda x: -x[1])
        return " ".join(f"{f}={w:.2f}" for f, w in ranked[:6])


# ── Search ────────────────────────────────────────────────────────────────────

class Search:
    """Pareto evolutionary search with adaptive weights, guided mutation,
    surrogate pre-screening, and local refinement."""

    def __init__(
        self,
        grammar:         Grammar      | None = None,
        eval_config:     EvalConfig   | None = None,
        objectives:      list[str]    | None = None,
        checkpoint_dir:  str          = "search_checkpoints",
        n_workers:       int          = 1,
        verbose:         bool         = True,
        use_surrogate:   bool         = True,
        use_adaptive:    bool         = True,
        use_guided:      bool         = True,
        surrogate_oversample: int     = 5,
    ):
        self.grammar    = grammar    or Grammar(num_players=2)
        self.eval_cfg   = eval_config or EvalConfig()
        self.objectives = objectives
        self.ckpt_dir   = checkpoint_dir
        self.n_workers  = n_workers
        self.verbose    = verbose
        self.surrogate_oversample = surrogate_oversample

        os.makedirs(checkpoint_dir, exist_ok=True)

        self.archive:     list[SearchResult] = []
        self.all_results: list[SearchResult] = []
        self.generation = 0

        # Adaptive weights
        self.use_adaptive = use_adaptive
        self.adaptive_weights = AdaptiveWeights() if use_adaptive else None

        # Guided mutation
        self.use_guided = use_guided
        self.mutation_tracker = MutationTracker() if use_guided else None

        # Surrogate model (lazy import to handle missing sklearn gracefully)
        self.use_surrogate = use_surrogate
        self.surrogate = None
        if use_surrogate:
            try:
                from .surrogate import SurrogateModel
                self.surrogate = SurrogateModel()
            except Exception:
                self.use_surrogate = False

    # ── current richness weights ─────────────────────────────────────────────

    @property
    def _richness_weights(self) -> dict[str, float] | None:
        if self.adaptive_weights:
            return self.adaptive_weights.weights
        return None

    # ── main search loop ──────────────────────────────────────────────────────

    def run(
        self,
        n_generations:  int = 20,
        pop_size:       int = 10,
        n_children:     int = 5,
    ) -> list[SearchResult]:
        """Run the evolutionary search.  Returns the final Pareto archive."""
        self._log(f"Starting search: {n_generations} generations, "
                  f"pop_size={pop_size}, children={n_children}")
        if self.adaptive_weights:
            self._log(f"  Adaptive weights: ON")
        if self.mutation_tracker:
            self._log(f"  Guided mutation: ON")
        if self.surrogate:
            self._log(f"  Surrogate pre-screening: ON ({self.surrogate_oversample}x)")

        # ── generation 0: random initialisation ─────────────────────────────
        self._log("Generation 0: sampling initial population …")
        initial_rulesets = [self.grammar.sample() for _ in range(pop_size)]
        self._evaluate_and_archive(initial_rulesets, gen=0)
        self._post_generation()

        # ── evolution ────────────────────────────────────────────────────────
        for gen in range(1, n_generations + 1):
            self.generation = gen
            self._log(f"\nGeneration {gen}: archive={len(self.archive)} …")

            children_with_info = self._generate_children(n_children)
            children = [c for c, *_ in children_with_info]
            self._evaluate_and_archive(children, gen=gen)

            # Record mutation outcomes
            if self.mutation_tracker:
                for i, (_, field, old_val, new_val) in enumerate(children_with_info):
                    if field and i < len(self.all_results):
                        # Check if the corresponding result entered the Pareto front
                        result_idx = len(self.all_results) - len(children) + i
                        if 0 <= result_idx < len(self.all_results):
                            sr = self.all_results[result_idx]
                            in_front = sr in self.archive
                            self.mutation_tracker.record(MutationRecord(
                                field=field, old_value=old_val, new_value=new_val,
                                improved_pareto=in_front,
                            ))

            self._post_generation()

        self._log(f"\nSearch complete.  Final archive: {len(self.archive)} rulesets.")
        return self.archive

    def _post_generation(self):
        """Update adaptive systems and save checkpoint after each generation."""
        if self.adaptive_weights:
            self.adaptive_weights.update(self.archive, self.all_results)
            self._log(f"  Weights: {self.adaptive_weights.summary()}")
        if self.mutation_tracker:
            self._log(f"  Mutation bias: {self.mutation_tracker.summary()}")
        if self.surrogate:
            self._fit_surrogate()
        self._save_checkpoint()
        self._report_archive()

    # ── population management ─────────────────────────────────────────────────

    def _generate_children(self, n: int) -> list[tuple[RuleSet, str, Any, Any]]:
        """Generate n children. Returns list of (ruleset, mutation_field, old_val, new_val).
        For crossover children, field/old/new are empty/None."""
        archive = self.archive or [SearchResult(self.grammar.sample(), GameMetrics())]

        # Get mutation biases
        field_w = self.mutation_tracker.field_weights() if self.mutation_tracker else None
        value_w = self.mutation_tracker.value_weights() if self.mutation_tracker else None

        # Generate candidates (oversample if surrogate is available)
        want = n * self.surrogate_oversample if (self.surrogate and self.surrogate.is_fitted) else n
        candidates: list[tuple[RuleSet, str, Any, Any]] = []

        for _ in range(want):
            op = random.random()
            if op < 0.5 or len(archive) < 2:
                parent = random.choice(archive).ruleset
                if self.mutation_tracker:
                    child, field, old, new = self.grammar.mutate_tracked(
                        parent, field_weights=field_w, value_weights=value_w)
                else:
                    child = self.grammar.mutate(parent)
                    field, old, new = "", None, None
            else:
                p1, p2 = random.sample(archive, 2)
                child = self.grammar.crossover(p1.ruleset, p2.ruleset)
                field, old, new = "", None, None
            candidates.append((child, field, old, new))

        # Surrogate pre-screening
        if self.surrogate and self.surrogate.is_fitted and len(candidates) > n:
            rulesets = [c[0] for c in candidates]
            predictions = self.surrogate.predict_batch(rulesets)
            # Score by predicted richness
            w = self._richness_weights or GameMetrics._DEFAULT_WEIGHTS
            scores = []
            for pred in predictions:
                # Map objective keys to richness weight keys approximately
                s = (pred.get("depth", 0) * w.get("elo_gain", 1.0) +
                     pred.get("complexity", 0) * w.get("log_game_tree", 0.3) +
                     pred.get("interaction", 0) * w.get("interaction_rate", 20.0) +
                     pred.get("mechanism", 0) * w.get("mechanism_coverage", 10.0) +
                     pred.get("balance", 0) * w.get("balance_score", 5.0))
                scores.append(s)

            # Keep top n, plus some random ones for exploration
            n_top = max(1, n - n // 4)
            n_random = n - n_top
            indexed = sorted(enumerate(scores), key=lambda x: -x[1])
            selected_idx = [i for i, _ in indexed[:n_top]]
            remaining = [i for i, _ in indexed[n_top:]]
            if remaining and n_random > 0:
                selected_idx.extend(random.sample(remaining, min(n_random, len(remaining))))
            candidates = [candidates[i] for i in selected_idx[:n]]
            self._log(f"  Surrogate pre-screened: {want} → {len(candidates)} candidates")

        return candidates[:n]

    def _evaluate_and_archive(self, rulesets: list[RuleSet], gen: int):
        """Evaluate rulesets and update Pareto archive."""
        if self.n_workers > 1:
            results = self._evaluate_parallel(rulesets, gen)
        else:
            results = self._evaluate_sequential(rulesets, gen)

        for r in results:
            self.all_results.append(r)
            self._update_archive(r)

            # Feed surrogate
            if self.surrogate:
                self.surrogate.add_observation(r.ruleset, r.objectives())

    def _evaluate_sequential(self, rulesets: list[RuleSet], gen: int) -> list[SearchResult]:
        results: list[SearchResult] = []
        for i, rs in enumerate(rulesets):
            self._log(f"  Evaluating [{i+1}/{len(rulesets)}] {rs.short_id()} …")
            t0 = time.time()
            m  = evaluate(rs, self.eval_cfg)
            elapsed = time.time() - t0
            sr = SearchResult(rs, m, gen=gen)
            results.append(sr)
            self._log(f"    {m.summary()}  ({elapsed:.1f}s)")
        return results

    def _evaluate_parallel(self, rulesets: list[RuleSet], gen: int) -> list[SearchResult]:
        import multiprocessing as mp
        try:
            with mp.Pool(self.n_workers) as pool:
                args = [(rs, self.eval_cfg) for rs in rulesets]
                metrics_list = pool.starmap(evaluate, args)
            return [SearchResult(rs, m, gen=gen) for rs, m in zip(rulesets, metrics_list)]
        except Exception as e:
            self._log(f"  Parallel eval failed ({e}), falling back to sequential")
            return self._evaluate_sequential(rulesets, gen)

    def _update_archive(self, new: SearchResult):
        """Add new result to Pareto archive, removing dominated results."""
        for existing in self.archive:
            if pareto_dominates(existing, new):
                return
        self.archive = [r for r in self.archive if not pareto_dominates(new, r)]
        self.archive.append(new)

    def _fit_surrogate(self):
        if self.surrogate and self.surrogate.n_observations >= self.surrogate.min_samples:
            self.surrogate.fit()
            if self.surrogate.is_fitted:
                self._log(f"  Surrogate fitted ({self.surrogate.n_observations} obs)")

    # ── local refinement ─────────────────────────────────────────────────────

    def refine(self, top_k: int = 5, max_evals: int = 50) -> list[SearchResult]:
        """Exhaustive 1-mutation neighborhood search around top rulesets.

        For each of the top_k rulesets, try every single-field mutation.
        Uses the surrogate to pre-screen if available, evaluating only the
        most promising max_evals candidates.
        """
        self._log(f"\n=== Local refinement: top {top_k}, max {max_evals} evals ===")
        top = self.top_results(top_k)
        if not top:
            return self.archive

        # Generate all single-field neighbors
        neighbors: list[tuple[RuleSet, str, Any, Any]] = []
        seen: set[str] = set()
        for sr in top:
            for fname, domain in self.grammar._FIELDS:
                current = getattr(sr.ruleset, fname)
                for value in domain:
                    if value == current:
                        continue
                    candidate = self.grammar.mutate_to(sr.ruleset, fname, value)
                    if candidate is None:
                        continue
                    key = json.dumps(candidate.to_dict(), sort_keys=True)
                    if key in seen:
                        continue
                    seen.add(key)
                    neighbors.append((candidate, fname, current, value))

        self._log(f"  Generated {len(neighbors)} unique neighbors")

        # Pre-screen with surrogate if available
        if self.surrogate and self.surrogate.is_fitted and len(neighbors) > max_evals:
            rulesets = [n[0] for n in neighbors]
            predictions = self.surrogate.predict_batch(rulesets)
            w = self._richness_weights or GameMetrics._DEFAULT_WEIGHTS
            scores = []
            for pred in predictions:
                s = (pred.get("depth", 0) * w.get("elo_gain", 1.0) +
                     pred.get("complexity", 0) * w.get("log_game_tree", 0.3) +
                     pred.get("interaction", 0) * w.get("interaction_rate", 20.0) +
                     pred.get("mechanism", 0) * w.get("mechanism_coverage", 10.0) +
                     pred.get("balance", 0) * w.get("balance_score", 5.0))
                scores.append(s)
            # Keep top max_evals
            indexed = sorted(enumerate(scores), key=lambda x: -x[1])
            neighbors = [neighbors[i] for i, _ in indexed[:max_evals]]
            self._log(f"  Surrogate pre-screened to {len(neighbors)} candidates")

        # Evaluate
        rulesets = [n[0] for n in neighbors[:max_evals]]
        self.generation += 1
        self._evaluate_and_archive(rulesets, gen=self.generation)

        # Record mutations
        if self.mutation_tracker:
            for i, (_, field, old, new) in enumerate(neighbors[:max_evals]):
                result_idx = len(self.all_results) - len(rulesets) + i
                if 0 <= result_idx < len(self.all_results):
                    sr = self.all_results[result_idx]
                    self.mutation_tracker.record(MutationRecord(
                        field=field, old_value=old, new_value=new,
                        improved_pareto=(sr in self.archive),
                    ))

        self._post_generation()
        self._log(f"  Refinement complete: archive now {len(self.archive)} rulesets")
        return self.archive

    # ── reporting ─────────────────────────────────────────────────────────────

    def _report_archive(self):
        if not self.verbose:
            return
        w = self._richness_weights
        front = sorted(self.archive, key=lambda r: r.richness(w), reverse=True)
        self._log(f"\n  ── Pareto archive ({len(front)} rulesets) ──")
        for r in front[:10]:
            self._log(f"  {r.label()}")
            self._log(f"    {r.metrics.summary()}")

    def top_results(self, n: int = 10) -> list[SearchResult]:
        """Top N results by richness score."""
        w = self._richness_weights
        return sorted(self.archive, key=lambda r: r.richness(w), reverse=True)[:n]

    def report(self, n: int = 10) -> str:
        lines = [f"=== Game Search Results (top {n}) ==="]
        if self.adaptive_weights:
            lines.append(f"Adaptive weights: {self.adaptive_weights.summary()}")
        if self.mutation_tracker:
            lines.append(f"Mutation bias: {self.mutation_tracker.summary()}")
        if self.surrogate and self.surrogate.is_fitted:
            lines.append(f"Surrogate: fitted on {self.surrogate.n_observations} observations")
            imps = self.surrogate.feature_importances()
            if imps:
                top_feats = list(imps.items())[:5]
                lines.append(f"  Top features: {', '.join(f'{k}={v:.3f}' for k, v in top_feats)}")

        for i, r in enumerate(self.top_results(n)):
            o = r.objectives()
            lines.append(f"\n#{i+1}  {r.ruleset.short_id()}  (gen {r.gen})")
            lines.append(f"  Board:     {r.ruleset.board_symmetry}-fold symmetry, "
                         f"{r.ruleset.num_rings} rings")
            lines.append(f"  Elements:  {r.ruleset.num_types} types, "
                         f"{r.ruleset.elements_per_player} per player, "
                         f"{r.ruleset.num_players} players")
            lines.append(f"  Actions:   "
                         f"move={'✓' if r.ruleset.can_move else '✗'} "
                         f"eat={'✓' if r.ruleset.can_eat else '✗'} "
                         f"grow={'✓' if r.ruleset.can_grow else '✗'} "
                         f"capture={'✓' if r.ruleset.can_capture else '✗'} "
                         f"circulate={'✓' if r.ruleset.can_circulate else '✗'}")
            lines.append(f"  Conflict:  {r.ruleset.conflict}")
            lines.append(f"  Win:       {r.ruleset.win_type} ≥ {r.ruleset.win_threshold}")
            lines.append(f"  Metrics:   {r.metrics.summary()}")
            lines.append(f"  Objectives: depth={o['depth']:.0f}  "
                         f"complexity={o['complexity']:.2f}  "
                         f"balance={o['balance']:.2f}  "
                         f"interaction={o['interaction']:.3f}  "
                         f"mechanism={o['mechanism']:.2f}  "
                         f"KR={-o['simplicity']:.1f}b")
        return "\n".join(lines)

    # ── persistence ───────────────────────────────────────────────────────────

    def _save_checkpoint(self):
        w = self._richness_weights
        path = os.path.join(self.ckpt_dir, f"gen_{self.generation:04d}.json")
        data = {
            "generation": self.generation,
            "archive": [
                {"ruleset": r.ruleset.to_dict(),
                 "gen":     r.gen,
                 "richness": r.richness(w),
                 "objectives": r.objectives(),
                 "summary":   r.metrics.summary()}
                for r in self.archive
            ],
        }
        if self.adaptive_weights:
            data["adaptive_weights"] = self.adaptive_weights.to_dict()
        if self.mutation_tracker:
            data["mutation_bias"] = self.mutation_tracker.field_weights()
        with open(path, "w") as f:
            json.dump(data, f, indent=2)

    def load_checkpoint(self, path: str):
        with open(path) as f:
            data = json.load(f)
        self.generation = data["generation"]
        from .metrics import SimMetrics
        from .evaluator import AZMetrics, GameMetrics
        self.archive = []
        for entry in data["archive"]:
            rs = RuleSet.from_dict(entry["ruleset"])
            m  = GameMetrics()
            self.archive.append(SearchResult(rs, m, gen=entry["gen"]))

    def _log(self, msg: str):
        if self.verbose:
            print(msg)
