#!/usr/bin/env python3
"""Run a game search. Execute from the organism project root:

    python -m alphazero.oroboros.run_search              # v2 (compositional)
    python -m alphazero.oroboros.run_search --v3         # v3 (rules as programs)
    python -m alphazero.oroboros.run_search --chemotaxis # chemotactic population search

Results go to the --outdir directory.
"""

import argparse
import json
import os

from .evaluator import EvalConfig
from .topology import BOARD_SIZES


def main():
    parser = argparse.ArgumentParser(description="Search for novel games")
    parser.add_argument("--v3", action="store_true", help="Use v3 engine (rules as programs)")
    parser.add_argument("--chemotaxis", action="store_true", help="Chemotactic population search (v3)")
    parser.add_argument("--gens", type=int, default=10, help="Number of generations")
    parser.add_argument("--pop", type=int, default=12, help="Initial population size")
    parser.add_argument("--children", type=int, default=5, help="Children per generation")
    parser.add_argument("--players", type=int, default=2, help="Fixed number of players")
    parser.add_argument("--max-board", type=int, default=5, help="Max board size")
    parser.add_argument("--az-iters", type=int, default=3, help="AlphaZero training iterations")
    parser.add_argument("--az-sims", type=int, default=10, help="MCTS simulations per move")
    parser.add_argument("--outdir", type=str, default=None, help="Output directory")
    args = parser.parse_args()

    if args.outdir is None:
        if args.chemotaxis:
            args.outdir = "search_results_chemotaxis"
        elif args.v3:
            args.outdir = "search_results_v3"
        else:
            args.outdir = "search_results"

    os.makedirs(args.outdir, exist_ok=True)

    # Cap board sizes
    BOARD_SIZES.clear()
    BOARD_SIZES.extend([3, 4, 5])
    if args.max_board > 5:
        BOARD_SIZES.append(args.max_board)

    cfg = EvalConfig(
        sim_n_games=30, sim_max_steps=600,
        az_iters=args.az_iters, az_sims=args.az_sims, az_games=3,
        az_max_steps=200, az_min_buf=10, az_eval_games=8,
    )

    if args.chemotaxis:
        from .grammar_v3 import GrammarV3
        from .chemotaxis import ChemotacticSearch

        grammar = GrammarV3(fixed_players=args.players)
        search = ChemotacticSearch(
            grammar=grammar, eval_config=cfg,
            max_population=args.pop * 3,
            min_population=args.pop,
            checkpoint_dir=os.path.join(args.outdir, "checkpoints"),
        )
        archive = search.run(
            n_generations=args.gens,
            initial_pop=args.pop,
        )
    elif args.v3:
        from .grammar_v3 import GrammarV3
        from .search_v3 import SearchV3

        grammar = GrammarV3(fixed_players=args.players)
        search = SearchV3(
            grammar=grammar, eval_config=cfg,
            checkpoint_dir=os.path.join(args.outdir, "checkpoints"),
        )
        archive = search.run(
            n_generations=args.gens,
            pop_size=args.pop,
            n_children=args.children,
        )
    else:
        from .grammar_v2 import GrammarV2
        from .search_v2 import SearchV2

        grammar = GrammarV2(fixed_players=args.players)
        search = SearchV2(
            grammar=grammar, eval_config=cfg,
            checkpoint_dir=os.path.join(args.outdir, "checkpoints"),
        )
        archive = search.run(
            n_generations=args.gens,
            pop_size=args.pop,
            n_children=args.children,
        )

    # Write report
    report = search.report(15)
    print("\n" + report)
    report_path = os.path.join(args.outdir, "report.txt")
    with open(report_path, "w") as f:
        f.write(report)
    print(f"\nReport saved to {report_path}")

    # Write top rulesets as JSON
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
        json.dump(top, f, indent=2, default=str)
    print(f"Top games saved to {top_path}")


if __name__ == "__main__":
    main()
