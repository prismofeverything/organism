"""Game search: discover strategically deep games via ruleset evolution.

Quick start
-----------
  from alphazero.oroboros import Search, Grammar, EvalConfig

  # Fast search with small boards, 2 players
  grammar = Grammar(fixed_players=2)
  cfg     = EvalConfig(az_iters=5, az_sims=15, az_games=4)
  search  = Search(grammar=grammar, eval_config=cfg)
  archive = search.run(n_generations=10, pop_size=8)
  print(search.report())

  # Evaluate a specific ruleset
  from alphazero.oroboros import evaluate
  rs = grammar.sample()
  metrics = evaluate(rs, cfg)
  print(metrics.summary())
"""

from .schema   import RuleSetV3
from .grammar  import Grammar
from .game     import Game
from .metrics  import SimMetrics, measure_simulation_metrics
from .evaluator import GameMetrics, AZMetrics, EvalConfig, evaluate
from .search   import Search, SearchResult

__all__ = [
    "RuleSetV3", "Grammar", "Game",
    "SimMetrics", "measure_simulation_metrics",
    "GameMetrics", "AZMetrics", "EvalConfig", "evaluate",
    "Search", "SearchResult",
]
