"""Simulation-based game metrics (no AlphaZero required).

All metrics are computed by playing random games.  They are fast and give
an initial filter before the expensive AlphaZero evaluation.

Metric glossary
---------------
termination_rate   : fraction of random games that end within max_steps
mean_game_length   : average steps to termination
balance_score      : 1 - max(|win_rate_i - 1/N|) across players; 1=perfectly balanced
branching_factor   : average number of legal actions per turn
interaction_rate   : fractional change in legal-action count caused by opponent move
capture_rate       : captures per game step (normalised)
mechanism_coverage : fraction of available action types used in random play
description_length : bits to describe the ruleset (lower = simpler)
log_game_tree      : log10 of estimated game-tree size (branching^length)
"""

from __future__ import annotations
import math
import random
from dataclasses import dataclass, field



@dataclass
class SimMetrics:
    termination_rate:   float = 0.0
    mean_game_length:   float = 0.0
    balance_score:      float = 0.0
    branching_factor:   float = 0.0
    interaction_rate:   float = 0.0
    capture_rate:       float = 0.0
    mechanism_coverage: float = 0.0
    description_length: float = 0.0
    log_game_tree:      float = 0.0

    def is_viable(self) -> bool:
        """Minimum quality bar before running expensive AlphaZero evaluation."""
        return (
            self.termination_rate   >= 0.15 and
            self.balance_score      >= 0.20 and
            self.branching_factor   >= 2.0
        )

    def viability_score(self) -> float:
        """Scalar in [0,1] — how close to passing viability."""
        return (
            min(self.termination_rate / 0.25,  1.0) * 0.4 +
            min(self.balance_score    / 0.50,  1.0) * 0.4 +
            min(self.branching_factor / 5.0,   1.0) * 0.2
        )


def _random_action(actions: dict) -> tuple:
    """Pick a random legal action, skipping PASS when possible."""
    keys = list(actions.keys())
    if len(keys) > 1:
        # prefer non-pass actions
        def is_pass(k):
            if isinstance(k, (list, tuple)):
                return k[0] == "pass" or (isinstance(k[0], str) and k[0] == "pass")
            return False
        non_pass = [k for k in keys if not is_pass(k)]
        if non_pass:
            keys = non_pass
    return random.choice(keys)


def _play_random_game(game, max_steps: int) -> tuple[dict, int]:
    """Play one fully random game.  Returns (final_state, n_steps)."""
    state = game.initial_state()
    for step in range(max_steps):
        if game.is_terminal(state):
            return state, step
        actions = game.legal_actions(state)
        if not actions:
            break
        state = actions[_random_action(actions)]
    return state, max_steps


def measure_simulation_metrics(
    game:      AbstractGame,
    n_games:   int = 60,
    max_steps: int = 300,
) -> SimMetrics:
    """Run n_games random games and compute all simulation metrics."""
    rs = game.rs

    terminated    = 0
    total_length  = 0
    win_counts    = {p: 0 for p in game._players}
    branching_sum = 0
    branching_n   = 0
    interaction_deltas: list[float] = []
    captures_sum  = 0
    action_types_used: set[int] = set()
    T = game.T

    for _ in range(n_games):
        state = game.initial_state()
        prev_legal_count: dict[str, int] = {}

        for step in range(max_steps):
            if game.is_terminal(state):
                break

            player = game.current_player(state)
            actions = game.legal_actions(state)
            if not actions:
                break

            # Branching factor
            branching_sum += len(actions)
            branching_n   += 1

            # Interaction rate: compare to after previous opponent move
            if player in prev_legal_count:
                old_n = prev_legal_count[player]
                if old_n > 0:
                    interaction_deltas.append(abs(len(actions) - old_n) / old_n)

            # Mechanism coverage: track action type slots used
            for act_idx in actions:
                if isinstance(act_idx, (list, tuple)):
                    # V3: key is [slot, src, tgt] or ["pass", nil, nil]
                    slot = act_idx[0]
                    if isinstance(slot, int):
                        action_types_used.add(slot)
                elif isinstance(act_idx, int):
                    # V1/V2: key is integer = slot * N * N + src * N + tgt
                    at = act_idx // (game.N * game.N)
                    action_types_used.add(at)

            # Take action
            chosen_idx = _random_action(actions)
            next_state = actions[chosen_idx]

            # Record legal count before advancing
            prev_legal_count[player] = len(actions)

            state = next_state

        final_state, n_steps = state, step + 1

        if game.is_terminal(state):
            terminated += 1
            total_length += n_steps
            rewards = game.rewards(state)
            winner = state.get("winner")
            if winner is not None and winner in win_counts:
                win_counts[winner] += 1

        # Captures (uses state["globals"]["counter_N"])
        if "captures" in state:
            for p in game._players:
                captures_sum += state["captures"].get(p, 0)
        elif "globals" in state:
            for p in game._players:
                p_idx = int(p) if isinstance(p, str) else p
                captures_sum += state["globals"].get(f"counter_{p_idx}", 0)

    n = max(n_games, 1)
    n_term = max(terminated, 1)

    termination_rate = terminated / n

    mean_length = total_length / n_term if terminated > 0 else max_steps

    # Balance: how evenly distributed are wins across players
    ideal = 1.0 / len(game._players)
    win_rates = {p: win_counts[p] / n_term for p in game._players}
    max_dev = max(abs(wr - ideal) for wr in win_rates.values())
    balance = max(0.0, 1.0 - max_dev / max(ideal, 1e-9))

    branching = branching_sum / max(branching_n, 1)

    interaction = float(sum(interaction_deltas) / max(len(interaction_deltas), 1))

    capture_rate = captures_sum / (n * max(mean_length, 1))

    # Mechanism coverage: one slot per rule + pass
    if hasattr(rs, 'rules'):
        n_at = len(rs.rules) + 1  # +1 for PASS
    elif hasattr(rs, 'action_names'):
        n_at = len(rs.action_names)
    elif hasattr(game, '_action_types'):
        n_at = len(game._action_types)
    else:
        n_at = 0
    available_slots: set[int] = set(range(n_at))

    used_available = action_types_used & available_slots
    mechanism_coverage = len(used_available) / max(len(available_slots), 1)

    # Description length
    dl = rs.description_length()

    # Game-tree estimate: branching^depth
    log_gt = mean_length * math.log10(max(branching, 1.01))

    return SimMetrics(
        termination_rate   = termination_rate,
        mean_game_length   = mean_length,
        balance_score      = balance,
        branching_factor   = branching,
        interaction_rate   = interaction,
        capture_rate       = capture_rate,
        mechanism_coverage = mechanism_coverage,
        description_length = dl,
        log_game_tree      = log_gt,
    )
