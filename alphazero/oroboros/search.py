"""Search: evolutionary search over rule-program games."""

from __future__ import annotations
import json
import os
import random
import time
from dataclasses import dataclass

from .schema import RuleSetV3
from .grammar import Grammar
from .evaluator import GameMetrics, EvalConfig, evaluate


@dataclass
class SearchResult:
    ruleset:  RuleSetV3
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

    def richness(self) -> float:
        return self.metrics.richness_score()

    def label(self) -> str:
        o = self.objectives()
        rs = self.ruleset
        return (f"{rs.short_id()}  {rs.schema.topology_type}  "
                f"rules={len(rs.rules)}  "
                f"depth={o['depth']:.0f}  "
                f"bal={o['balance']:.2f}  "
                f"KR={rs.description_length():.0f}b")


def _dominates(a: SearchResult, b: SearchResult) -> bool:
    oa, ob = a.objectives(), b.objectives()
    better = False
    for k in oa:
        if oa[k] < ob[k]:
            return False
        if oa[k] > ob[k]:
            better = True
    return better


class Search:
    def __init__(
        self,
        grammar:        Grammar  | None = None,
        eval_config:    EvalConfig | None = None,
        checkpoint_dir: str        = "search_checkpoints",
        verbose:        bool       = True,
    ):
        self.grammar  = grammar or Grammar(fixed_players=2)
        self.eval_cfg = eval_config or EvalConfig()
        self.ckpt_dir = checkpoint_dir
        self.verbose  = verbose
        os.makedirs(checkpoint_dir, exist_ok=True)

        self.archive:     list[SearchResult] = []
        self.all_results: list[SearchResult] = []
        self.generation = 0

    def run(self, n_generations: int = 20, pop_size: int = 10, n_children: int = 5
            ) -> list[SearchResult]:
        self._log(f"Search (rules as programs): {n_generations} gens, "
                  f"pop={pop_size}, children={n_children}")
        self._log(f"  No predefined actions — rules are (predicate, effect) AST pairs\n")

        self._log("Generation 0: sampling random games …")
        rulesets = [self.grammar.sample() for _ in range(pop_size)]
        self._eval_and_archive(rulesets, gen=0)
        self._save_checkpoint()
        self._report()

        for gen in range(1, n_generations + 1):
            self.generation = gen
            self._log(f"\nGeneration {gen}: archive={len(self.archive)} …")
            children = self._make_children(n_children)
            self._eval_and_archive(children, gen=gen)
            self._save_checkpoint()
            self._report()

        self._log(f"\nDone. Archive: {len(self.archive)} games.")
        return self.archive

    def _make_children(self, n: int) -> list[RuleSetV3]:
        archive = self.archive
        if not archive:
            return [self.grammar.sample() for _ in range(n)]
        children = []
        for _ in range(n):
            if random.random() < 0.5 or len(archive) < 2:
                parent = random.choice(archive).ruleset
                children.append(self.grammar.mutate(parent))
            else:
                p1, p2 = random.sample(archive, 2)
                children.append(self.grammar.crossover(p1.ruleset, p2.ruleset))
        return children

    def _eval_and_archive(self, rulesets: list[RuleSetV3], gen: int):
        for i, rs in enumerate(rulesets):
            self._log(f"  [{i+1}/{len(rulesets)}] {rs.short_id()} "
                      f"{rs.schema.topology_type} rules={len(rs.rules)} …")
            t0 = time.time()
            m = evaluate(rs, self.eval_cfg)
            elapsed = time.time() - t0
            sr = SearchResult(rs, m, gen=gen)
            self.all_results.append(sr)
            self._update_archive(sr)
            self._log(f"    {m.summary()}  ({elapsed:.1f}s)")

    def _update_archive(self, new: SearchResult):
        for existing in self.archive:
            if _dominates(existing, new):
                return
        self.archive = [r for r in self.archive if not _dominates(new, r)]
        self.archive.append(new)

    def _report(self):
        if not self.verbose:
            return
        front = sorted(self.archive, key=lambda r: r.richness(), reverse=True)
        self._log(f"\n  ── Archive ({len(front)}) ──")
        for r in front[:8]:
            self._log(f"  {r.label()}")
            self._log(f"    {r.metrics.summary()}")

    def top_results(self, n: int = 10) -> list[SearchResult]:
        return sorted(self.archive, key=lambda r: r.richness(), reverse=True)[:n]

    def report(self, n: int = 15) -> str:
        lines = [f"=== Search Results — Rules as Programs (top {n}) ==="]
        for i, r in enumerate(self.top_results(n)):
            o = r.objectives()
            rs = r.ruleset
            s = rs.schema
            lines.append(f"\n#{i+1}  {rs.short_id()}  (gen {r.gen})")
            lines.append(f"  Topology:    {s.topology_type} size={s.topology_size}")
            lines.append(f"  Pieces:      {s.num_piece_types} types, {s.num_players}p")
            lines.append(f"  Resources:   {s.num_resource_slots} slots")
            lines.append(f"  Rules:       {len(rs.rules)} (description: {rs.description_length():.0f} bits)")
            lines.append(f"  Turns:       {rs.actions_per_turn}/turn, limit={rs.turn_limit}")
            lines.append(f"  Conflict:    {rs.conflict_table}")
            lines.append(f"  Metrics:     {r.metrics.summary()}")
            lines.append(f"  Depth={o['depth']:.0f}  Complexity={o['complexity']:.2f}  "
                         f"Balance={o['balance']:.2f}  Interaction={o['interaction']:.3f}")
        return "\n".join(lines)

    def _save_checkpoint(self):
        path = os.path.join(self.ckpt_dir, f"gen_{self.generation:04d}.json")
        data = {
            "generation": self.generation,
            "archive": [
                {"ruleset": r.ruleset.to_dict(),
                 "gen": r.gen,
                 "richness": r.richness(),
                 "objectives": r.objectives(),
                 "summary": r.metrics.summary()}
                for r in self.archive
            ]
        }
        with open(path, "w") as f:
            json.dump(data, f, indent=2, default=str)

    def _log(self, msg: str):
        if self.verbose:
            print(msg)
