#!/usr/bin/env python3
"""Run a game search. Execute from the organism project root:

    python -m alphazero.oroboros.run_search              # evolutionary search
    python -m alphazero.oroboros.run_search --chemotaxis # chemotactic population search

Results go to the --outdir directory.
"""

import argparse
import json
import os

from .evaluator import EvalConfig


def main():
    parser = argparse.ArgumentParser(description="Search for novel games")
    parser.add_argument("--chemotaxis", action="store_true", help="Chemotactic population search")
    parser.add_argument("--gens", type=int, default=10, help="Number of generations")
    parser.add_argument("--pop", type=int, default=12, help="Initial population size")
    parser.add_argument("--children", type=int, default=5, help="Children per generation")
    parser.add_argument("--players", type=int, default=2, help="Fixed number of players")
    parser.add_argument("--az-iters", type=int, default=3, help="AlphaZero training iterations")
    parser.add_argument("--az-sims", type=int, default=10, help="MCTS simulations per move")
    parser.add_argument("--outdir", type=str, default=None, help="Output directory")
    args = parser.parse_args()

    if args.outdir is None:
        if args.chemotaxis:
            args.outdir = "search_results_chemotaxis"
        else:
            args.outdir = "search_results"

    os.makedirs(args.outdir, exist_ok=True)

    cfg = EvalConfig(
        sim_n_games=30, sim_max_steps=600,
        az_iters=args.az_iters, az_sims=args.az_sims, az_games=3,
        az_max_steps=200, az_min_buf=10, az_eval_games=8,
    )

    if args.chemotaxis:
        from .grammar import Grammar
        from .chemotaxis import ChemotacticSearch

        grammar = Grammar(fixed_players=args.players)
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
    else:
        from .grammar import Grammar
        from .search import Search

        grammar = Grammar(fixed_players=args.players)
        search = Search(
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
            "richness": r.richness() if callable(r.richness) else r.richness,
            "objectives": r.objectives(),
            "summary": r.metrics.summary(),
            "ruleset": r.ruleset.to_dict(),
        })
    with open(top_path, "w") as f:
        json.dump(top, f, indent=2, default=str)
    print(f"Top games saved to {top_path}")

    # Auto-merge into persistent game library
    from .library import merge_into_library
    added = merge_into_library(top)
    print(f"Library: {added} new games added to game_library.json")


if __name__ == "__main__":
    main()
