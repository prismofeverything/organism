"""Evaluator: full evaluation pipeline for a RuleSet.

Phase 1 — simulation metrics (fast, ~1-2 s):
  termination, balance, branching, interaction, mechanism_coverage

Phase 2 — AlphaZero depth metrics (slow, ~30-120 s per ruleset):
  elo_gain          : total Elo improvement over eval_iters training iterations
  elo_slope         : Elo per iteration (higher = faster learning = shallower?)
  elo_curve         : list of Elo estimates per checkpoint
  trained_vs_random : win rate of final trained agent vs random agent
  position_hardness : KL divergence between low- and high-sim MCTS policies
  mechanism_coverage_trained : action type coverage in trained-agent play

These two phases are separated so the search loop can skip phase 2 for
clearly non-viable rulesets.
"""

from __future__ import annotations
import copy
import random
import time
from dataclasses import dataclass, field
from typing import ClassVar

import numpy as np

from .abstract_game import AbstractGame
from .ruleset import RuleSet
from .metrics import SimMetrics, measure_simulation_metrics


@dataclass
class AZMetrics:
    elo_gain:                   float        = 0.0
    elo_slope:                  float        = 0.0
    elo_curve:                  list[float]  = field(default_factory=list)
    trained_vs_random:          float        = 0.0
    position_hardness:          float        = 0.0
    mechanism_coverage_trained: float        = 0.0


@dataclass
class GameMetrics:
    sim:              SimMetrics  = field(default_factory=SimMetrics)
    az:               AZMetrics   = field(default_factory=AZMetrics)
    eval_time_s:      float       = 0.0

    # ── composite scores ────────────────────────────────────────────────────

    def complexity_ratio(self) -> float:
        """log10(game-tree) / description_length — the core signal."""
        dl = max(self.sim.description_length, 1.0)
        return self.sim.log_game_tree / dl

    def depth_score(self) -> float:
        """AlphaZero strategic depth (primary quality signal)."""
        return self.az.elo_gain

    # Default richness weights — used when no adaptive weights are provided
    _DEFAULT_WEIGHTS: ClassVar[dict[str, float]] = {
        "elo_gain":                   1.0,
        "log_game_tree":              0.3,
        "interaction_rate":          20.0,
        "mechanism_coverage":        10.0,
        "mechanism_coverage_trained": 10.0,
        "balance_score":              5.0,
    }

    def _raw_richness_components(self) -> dict[str, float]:
        """Extract the raw values that richness_score weights over."""
        return {
            "elo_gain":                   self.az.elo_gain,
            "log_game_tree":              self.sim.log_game_tree,
            "interaction_rate":           self.sim.interaction_rate,
            "mechanism_coverage":         self.sim.mechanism_coverage,
            "mechanism_coverage_trained": self.az.mechanism_coverage_trained,
            "balance_score":              self.sim.balance_score,
        }

    def richness_score(self, weights: dict[str, float] | None = None) -> float:
        """Combined quality metric for Pareto search.

        Args:
            weights: Optional dict mapping component names to weights.
                     If None, uses default hardcoded weights.
        """
        w = weights or self._DEFAULT_WEIGHTS
        raw = self._raw_richness_components()
        return sum(raw.get(k, 0.0) * w.get(k, 0.0) for k in w)

    def summary(self) -> str:
        s = self.sim
        a = self.az
        return (
            f"term={s.termination_rate:.2f} "
            f"bal={s.balance_score:.2f} "
            f"B={s.branching_factor:.1f} "
            f"len={s.mean_game_length:.0f} "
            f"interact={s.interaction_rate:.2f} "
            f"mech={s.mechanism_coverage:.2f} "
            f"KR={s.description_length:.1f}b "
            f"logGT={s.log_game_tree:.1f} | "
            f"elo={a.elo_gain:.0f} "
            f"tvr={a.trained_vs_random:.2f} "
            f"hard={a.position_hardness:.3f}"
        )


# ── helpers ────────────────────────────────────────────────────────────────────

def _play_one_game(
    game:      AbstractGame,
    agent1,                   # callable(state) → action_idx
    agent2,                   # callable(state) → action_idx
    max_steps: int = 200,
) -> str | None:
    """Play a game between two agents. Returns winner player label or None."""
    state = game.initial_state()
    players = game._players
    for _ in range(max_steps):
        if game.is_terminal(state):
            break
        player = game.current_player(state)
        agent  = agent1 if player == players[0] else agent2
        actions = game.legal_actions(state)
        if not actions:
            break
        idx = agent(state, actions)
        state = actions.get(idx, next(iter(actions.values())))
    return state["winner"]


def _random_agent(state: dict, actions: dict) -> int:
    return random.choice(list(actions.keys()))


def _mcts_agent(game, net, n_sims: int):
    from alphazero.mcts import MCTS
    mcts = MCTS(game, net, num_simulations=n_sims)
    def _agent(state, actions):
        pi = mcts.policy(state, temperature=0.0)
        idx = int(np.argmax(pi))
        if idx in actions:
            return idx
        return next(iter(actions))
    return _agent


def _win_rate(game, agent1, agent2, n_games: int = 10, max_steps: int = 150) -> float:
    """Win rate of agent1 as player "0"."""
    wins = 0
    for _ in range(n_games):
        w = _play_one_game(game, agent1, agent2, max_steps)
        if w == game._players[0]:
            wins += 1
    return wins / n_games


def _elo_delta(win_rate: float) -> float:
    """Elo difference implied by a win rate (logistic model)."""
    wr = max(0.01, min(0.99, win_rate))
    return 400 * (np.log10(wr / (1 - wr)))


def _measure_position_hardness(game, net_low, net_high, n_states: int = 20) -> float:
    """KL divergence between low-sim and high-sim MCTS policies.

    Higher = game positions are harder to evaluate = deeper game.
    """
    from alphazero.mcts import MCTS
    mcts_lo = MCTS(game, net_high, num_simulations=5)
    mcts_hi = MCTS(game, net_high, num_simulations=50)

    kl_sum = 0.0
    n_measured = 0
    state = game.initial_state()
    for _ in range(n_states * 4):   # walk up to 4× to find valid states
        if game.is_terminal(state):
            state = game.initial_state()
            continue
        if n_measured >= n_states:
            break
        try:
            pi_lo = mcts_lo.policy(state, temperature=1.0) + 1e-10
            pi_hi = mcts_hi.policy(state, temperature=1.0) + 1e-10
            pi_lo /= pi_lo.sum()
            pi_hi /= pi_hi.sum()
            kl = float(np.sum(pi_hi * np.log(pi_hi / pi_lo)))
            kl_sum += kl
            n_measured += 1
        except Exception:
            pass
        # Take a random step
        actions = game.legal_actions(state)
        if not actions:
            state = game.initial_state()
            continue
        state = actions[random.choice(list(actions.keys()))]

    return kl_sum / max(n_measured, 1)


def _measure_mechanism_coverage_trained(game, net, n_games: int = 10) -> float:
    """Fraction of available action type slots used by the trained agent."""
    from alphazero.mcts import MCTS
    mcts = MCTS(game, net, num_simulations=10)
    T, N = game.T, game.N
    available: set[int] = set()
    rs = game.rs
    if rs.can_move:     available.add(0)
    if rs.can_eat:      available.add(1)
    if rs.can_grow:
        for t in range(T): available.add(2 + t)
    if rs.can_capture:  available.add(T + 2)
    if rs.can_circulate: available.add(T + 3)

    used: set[int] = set()
    for _ in range(n_games):
        state = game.initial_state()
        for __ in range(150):
            if game.is_terminal(state):
                break
            actions = game.legal_actions(state)
            if not actions:
                break
            pi = mcts.policy(state, temperature=0.0)
            idx = int(np.argmax(pi))
            if idx not in actions:
                idx = next(iter(actions))
            at = idx // (N * N)
            used.add(at)
            state = actions[idx]

    return len(used & available) / max(len(available), 1)


# ── main evaluator ─────────────────────────────────────────────────────────────

@dataclass
class EvalConfig:
    # Simulation phase
    sim_n_games:   int   = 60
    sim_max_steps: int   = 300

    # AlphaZero phase (small network + few sims for speed)
    az_iters:      int   = 5    # training iterations
    az_games:      int   = 4    # self-play games per iter
    az_sims:       int   = 15   # MCTS simulations per move
    az_max_steps:  int   = 150  # max steps per self-play game
    az_min_buf:    int   = 20   # min samples before training
    az_batch:      int   = 32
    az_train_steps: int  = 20
    az_eval_games: int   = 12   # games per Elo estimate
    az_blocks:     int   = 3    # ResNet depth
    az_filters:    int   = 32   # ResNet filters
    device:        str   = "cpu"


def evaluate(ruleset: RuleSet, config: EvalConfig | None = None) -> GameMetrics:
    """Full evaluation pipeline for a ruleset."""
    if config is None:
        config = EvalConfig()
    t0 = time.time()

    game = AbstractGame(ruleset)
    metrics = GameMetrics()

    # ── Phase 1: simulation metrics ──────────────────────────────────────────
    metrics.sim = measure_simulation_metrics(
        game, n_games=config.sim_n_games, max_steps=config.sim_max_steps
    )

    if not metrics.sim.is_viable():
        metrics.eval_time_s = time.time() - t0
        return metrics

    # ── Phase 2: AlphaZero depth metrics ─────────────────────────────────────
    try:
        metrics.az = _evaluate_az(game, config)
    except Exception as e:
        # Non-fatal: return sim metrics even if AZ fails
        metrics.az = AZMetrics()

    metrics.eval_time_s = time.time() - t0
    return metrics


def _evaluate_az(game: AbstractGame, cfg: EvalConfig) -> AZMetrics:
    import torch
    from alphazero.network import AlphaZeroNetwork
    from alphazero.self_play import self_play_game, ReplayBuffer
    from alphazero.train import SelfPlayDataset
    from torch.utils.data import DataLoader
    import torch.nn.functional as F

    net = AlphaZeroNetwork.for_game(
        game, num_res_blocks=cfg.az_blocks, num_filters=cfg.az_filters
    ).to(cfg.device)
    net.eval()

    optimizer = torch.optim.Adam(net.parameters(), lr=1e-3)
    replay = ReplayBuffer(capacity=10_000)

    elo_estimates: list[float] = [0.0]  # Elo of random baseline = 0
    prev_net_sd = copy.deepcopy(net.state_dict())

    for it in range(cfg.az_iters):
        # Self-play
        net.eval()
        for _ in range(cfg.az_games):
            samples = self_play_game(
                game, net,
                num_simulations=cfg.az_sims,
                max_steps=cfg.az_max_steps,
            )
            replay.push(samples)

        # Train
        if len(replay) >= cfg.az_min_buf:
            net.train()
            samples = replay.sample(min(len(replay), cfg.az_batch * cfg.az_train_steps))
            ds = SelfPlayDataset(samples)
            loader = DataLoader(ds, batch_size=cfg.az_batch, shuffle=True)
            for step, (s, pi, v) in enumerate(loader):
                s, pi, v = s.to(cfg.device), pi.to(cfg.device), v.to(cfg.device)
                log_pi, vpred = net(s)
                p_loss = -(pi * log_pi).sum(dim=1).mean()
                v_loss = F.mse_loss(vpred, v)
                loss   = p_loss + v_loss
                optimizer.zero_grad(); loss.backward()
                torch.nn.utils.clip_grad_norm_(net.parameters(), 5.0)
                optimizer.step()
                if step + 1 >= cfg.az_train_steps:
                    break

        # Elo estimate vs previous checkpoint
        net.eval()
        prev_net = AlphaZeroNetwork.for_game(
            game, num_res_blocks=cfg.az_blocks, num_filters=cfg.az_filters
        ).to(cfg.device)
        prev_net.load_state_dict(prev_net_sd)
        prev_net.eval()

        agent_new  = _mcts_agent(game, net,      cfg.az_sims)
        agent_prev = _mcts_agent(game, prev_net, cfg.az_sims)

        wr = _win_rate(game, agent_new, agent_prev, n_games=cfg.az_eval_games)
        delta_elo = _elo_delta(wr)
        elo_estimates.append(elo_estimates[-1] + delta_elo)

        prev_net_sd = copy.deepcopy(net.state_dict())

    # Trained vs random
    agent_trained = _mcts_agent(game, net, cfg.az_sims)
    tvr = _win_rate(game, agent_trained, _random_agent, n_games=cfg.az_eval_games)

    # Position hardness
    hard = _measure_position_hardness(game, net, net, n_states=15)

    # Mechanism coverage of trained agent
    mct = _measure_mechanism_coverage_trained(game, net, n_games=6)

    elo_curve = elo_estimates[1:]  # drop the initial 0.0 baseline
    elo_gain  = max(0.0, elo_estimates[-1])
    elo_slope = elo_gain / max(cfg.az_iters, 1)

    return AZMetrics(
        elo_gain                   = elo_gain,
        elo_slope                  = elo_slope,
        elo_curve                  = elo_curve,
        trained_vs_random          = tvr,
        position_hardness          = hard,
        mechanism_coverage_trained = mct,
    )
