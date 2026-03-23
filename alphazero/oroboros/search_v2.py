"""Search v2: evolutionary search over the full compositional game space.

Uses GrammarV2 + GameV2 + evaluate_v2 with the same Pareto archive
and adaptive weights machinery from search.py.
"""

from __future__ import annotations
import json
import os
import random
import time
from dataclasses import dataclass, field

from .ruleset_v2 import RuleSetV2
from .grammar_v2 import GrammarV2
from .game_v2 import GameV2
from .evaluator import GameMetrics, EvalConfig
from .evaluator_v2 import evaluate_v2


@dataclass
class SearchResultV2:
    ruleset:  RuleSetV2
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
        return (f"{rs.short_id()}  {rs.board.topology}  "
                f"acts={','.join(rs.action_names)}  "
                f"depth={o['depth']:.0f}  "
                f"bal={o['balance']:.2f}")


def _dominates(a: SearchResultV2, b: SearchResultV2) -> bool:
    oa, ob = a.objectives(), b.objectives()
    better = False
    for k in oa:
        if oa[k] < ob[k]:
            return False
        if oa[k] > ob[k]:
            better = True
    return better


class SearchV2:
    """Pareto search over the compositional game space."""

    def __init__(
        self,
        grammar:        GrammarV2    | None = None,
        eval_config:    EvalConfig   | None = None,
        checkpoint_dir: str          = "search_v2_checkpoints",
        verbose:        bool         = True,
    ):
        self.grammar  = grammar or GrammarV2(fixed_players=2)
        self.eval_cfg = eval_config or EvalConfig()
        self.ckpt_dir = checkpoint_dir
        self.verbose  = verbose
        os.makedirs(checkpoint_dir, exist_ok=True)

        self.archive:     list[SearchResultV2] = []
        self.all_results: list[SearchResultV2] = []
        self.generation = 0

    def run(self, n_generations: int = 20, pop_size: int = 10, n_children: int = 5
            ) -> list[SearchResultV2]:
        self._log(f"Search v2: {n_generations} gens, pop={pop_size}, children={n_children}")
        self._log(f"  Topologies: ring, square, hex, torus, petersen")
        self._log(f"  Actions: move, eat, grow, capture, circulate, place, push, swap, convert, snipe, leap, charge")
        self._log(f"  Win: captures, population, elimination, territory, score_at_limit\n")

        # Gen 0: random
        self._log("Generation 0: sampling …")
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

        self._log(f"\nDone. Archive: {len(self.archive)} rulesets.")
        return self.archive

    def _make_children(self, n: int) -> list[RuleSetV2]:
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

    def _eval_and_archive(self, rulesets: list[RuleSetV2], gen: int):
        for i, rs in enumerate(rulesets):
            self._log(f"  [{i+1}/{len(rulesets)}] {rs.short_id()} {rs.board.topology} "
                      f"acts={','.join(rs.action_names)} …")
            t0 = time.time()
            m = evaluate_v2(rs, self.eval_cfg)
            elapsed = time.time() - t0
            sr = SearchResultV2(rs, m, gen=gen)
            self.all_results.append(sr)
            self._update_archive(sr)
            self._log(f"    {m.summary()}  ({elapsed:.1f}s)")

    def _update_archive(self, new: SearchResultV2):
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

    def top_results(self, n: int = 10) -> list[SearchResultV2]:
        return sorted(self.archive, key=lambda r: r.richness(), reverse=True)[:n]

    def report(self, n: int = 10) -> str:
        lines = [f"=== Search V2 Results (top {n}) ==="]
        for i, r in enumerate(self.top_results(n)):
            o = r.objectives()
            rs = r.ruleset
            lines.append(f"\n#{i+1}  {rs.short_id()}  (gen {r.gen})")
            lines.append(f"  Topology:  {rs.board.topology} size={rs.board.size}")
            lines.append(f"  Pieces:    {rs.pieces.num_types} types, "
                         f"{rs.pieces.elements_per_player}/player, {rs.num_players}p")
            lines.append(f"  Actions:   {', '.join(rs.action_names)}")
            lines.append(f"  Turns:     {rs.turns.structure}")
            lines.append(f"  Conflict:  {rs.interactions.conflict}  "
                         f"trigger={rs.interactions.trigger}")
            lines.append(f"  Win:       {rs.win.condition} ≥ {rs.win.threshold}")
            lines.append(f"  Info:      {rs.info.visibility}"
                         + (f" radius={rs.info.fog_radius}" if rs.info.visibility == "fog" else ""))
            lines.append(f"  Resources: {rs.resources.system}")
            lines.append(f"  Metrics:   {r.metrics.summary()}")
            lines.append(f"  Depth={o['depth']:.0f}  Complexity={o['complexity']:.2f}  "
                         f"Balance={o['balance']:.2f}  Interaction={o['interaction']:.3f}  "
                         f"KR={rs.description_length():.0f}b")
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
            json.dump(data, f, indent=2)

    def _log(self, msg: str):
        if self.verbose:
            print(msg)
