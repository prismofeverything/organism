"""Evaluator for v2 compositional rulesets.

Thin wrapper that constructs GameV2 instead of AbstractGame,
then delegates to the same metrics/AZ pipeline.
"""

from __future__ import annotations
import time

from .ruleset_v2 import RuleSetV2
from .game_v2 import GameV2
from .metrics import SimMetrics, measure_simulation_metrics
from .evaluator import GameMetrics, AZMetrics, EvalConfig, _evaluate_az


def evaluate_v2(ruleset: RuleSetV2, config: EvalConfig | None = None) -> GameMetrics:
    """Full evaluation pipeline for a v2 ruleset."""
    if config is None:
        config = EvalConfig()
    t0 = time.time()

    try:
        game = GameV2(ruleset)
    except Exception as e:
        # Board construction failed — return empty metrics
        return GameMetrics(eval_time_s=time.time() - t0)

    metrics = GameMetrics()

    # Phase 1: simulation metrics
    metrics.sim = measure_simulation_metrics(
        game, n_games=config.sim_n_games, max_steps=config.sim_max_steps,
    )

    if not metrics.sim.is_viable():
        metrics.eval_time_s = time.time() - t0
        return metrics

    # Phase 2: AlphaZero depth metrics
    try:
        metrics.az = _evaluate_az(game, config)
    except Exception:
        metrics.az = AZMetrics()

    metrics.eval_time_s = time.time() - t0
    return metrics
