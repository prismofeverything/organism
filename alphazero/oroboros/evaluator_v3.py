"""Evaluator for v3 rule-program games."""

from __future__ import annotations
import time

from .schema import RuleSetV3
from .game_v3 import GameV3
from .metrics import SimMetrics, measure_simulation_metrics
from .evaluator import GameMetrics, AZMetrics, EvalConfig, _evaluate_az


def evaluate_v3(ruleset: RuleSetV3, config: EvalConfig | None = None) -> GameMetrics:
    if config is None:
        config = EvalConfig()
    t0 = time.time()

    try:
        game = GameV3(ruleset)
    except Exception:
        return GameMetrics(eval_time_s=time.time() - t0)

    metrics = GameMetrics()
    metrics.sim = measure_simulation_metrics(
        game, n_games=config.sim_n_games, max_steps=config.sim_max_steps,
    )

    if not metrics.sim.is_viable():
        metrics.eval_time_s = time.time() - t0
        return metrics

    try:
        metrics.az = _evaluate_az(game, config)
    except Exception:
        metrics.az = AZMetrics()

    metrics.eval_time_s = time.time() - t0
    return metrics
