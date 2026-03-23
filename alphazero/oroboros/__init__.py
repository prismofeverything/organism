"""Game search: discover strategically deep games via ruleset evolution.

Quick start
-----------
  from alphazero.oroboros import Search, Grammar, EvalConfig

  # Fast search with small boards, 2 players
  grammar = Grammar(num_players=2)
  cfg     = EvalConfig(az_iters=5, az_sims=15, az_games=4)
  search  = Search(grammar=grammar, eval_config=cfg, n_workers=1)
  archive = search.run(n_generations=10, pop_size=8)
  print(search.report())

  # Evaluate a specific ruleset
  from alphazero.oroboros import RuleSet, evaluate
  rs = grammar.heterarchy_minimal()
  metrics = evaluate(rs, cfg)
  print(metrics.summary())
"""

from .ruleset   import RuleSet
from .grammar   import Grammar
from .abstract_game import AbstractGame
from .metrics   import SimMetrics, measure_simulation_metrics
from .evaluator import GameMetrics, AZMetrics, EvalConfig, evaluate
from .search    import (
    Search, SearchResult,
    pareto_dominates, compute_pareto_front,
    AdaptiveWeights, MutationTracker, MutationRecord,
)
from .surrogate import RuleSetEncoder, SurrogateModel

__all__ = [
    "RuleSet", "Grammar", "AbstractGame",
    "SimMetrics", "measure_simulation_metrics",
    "GameMetrics", "AZMetrics", "EvalConfig", "evaluate",
    "Search", "SearchResult", "pareto_dominates", "compute_pareto_front",
    "AdaptiveWeights", "MutationTracker", "MutationRecord",
    "RuleSetEncoder", "SurrogateModel",
]
