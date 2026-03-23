#!/usr/bin/env python3
"""Run a v2 game search. Execute from the organism project root:

    python -m alphazero.oroboros.run_search

Results go to search_results/ in the project root.
"""

import argparse
import json
import os
import sys

from .grammar_v2 import GrammarV2
from .evaluator import EvalConfig
from .search_v2 import SearchV2
from .topology import BOARD_SIZES


def main():
    parser = argparse.ArgumentParser(description="Search for novel games")
    parser.add_argument("--gens", type=int, default=10, help="Number of generations")
    parser.add_argument("--pop", type=int, default=8, help="Initial population size")
    parser.add_argument("--children", type=int, default=5, help="Children per generation")
    parser.add_argument("--players", type=int, default=2, help="Fixed number of players")
    parser.add_argument("--max-board", type=int, default=5, help="Max board size (caps node count)")
    parser.add_argument("--az-iters", type=int, default=3, help="AlphaZero training iterations")
    parser.add_argument("--az-sims", type=int, default=10, help="MCTS simulations per move")
    parser.add_argument("--outdir", type=str, default="search_results", help="Output directory")
    args = parser.parse_args()

    # Cap board sizes globally so we don't get 100+ node boards
    BOARD_SIZES.clear()
    BOARD_SIZES.extend([3, 4, 5])
    if args.max_board > 5:
        BOARD_SIZES.append(args.max_board)

    grammar = GrammarV2(fixed_players=args.players)
    cfg = EvalConfig(
        sim_n_games=30, sim_max_steps=600,  # enough for multi-action + turn limits
        az_iters=args.az_iters, az_sims=args.az_sims, az_games=3,
        az_max_steps=200, az_min_buf=10, az_eval_games=8,
    )

    search = SearchV2(
        grammar=grammar, eval_config=cfg,
        checkpoint_dir=os.path.join(args.outdir, "checkpoints"),
    )

    archive = search.run(
        n_generations=args.gens,
        pop_size=args.pop,
        n_children=args.children,
    )

    # Write final report
    report = search.report(15)
    print("\n" + report)
    report_path = os.path.join(args.outdir, "report.txt")
    with open(report_path, "w") as f:
        f.write(report)
    print(f"\nReport saved to {report_path}")

    # Write top rulesets as JSON for the universal player
    top_path = os.path.join(args.outdir, "top_games.json")
    top = []
    for i, r in enumerate(search.top_results(20)):
        top.append({
            "rank": i + 1,
            "richness": r.richness(),
            "objectives": r.objectives(),
            "summary": r.metrics.summary(),
            "ruleset": r.ruleset.to_dict(),
        })
    with open(top_path, "w") as f:
        json.dump(top, f, indent=2)
    print(f"Top games saved to {top_path}")


if __name__ == "__main__":
    main()
