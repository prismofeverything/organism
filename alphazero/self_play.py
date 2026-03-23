"""Self-play data generation.

Generates (state_tensor, policy, outcome) triples by running full games
with MCTS-guided play.  Results are pushed to a shared replay buffer.

Parallelism strategy:
  - Each self_play_game() call is independent and can be run in a separate
    process or thread.  See worker() for a multiprocessing-friendly wrapper.

Data format per step:
  {
    "state":   np.ndarray (C, H, W)   — encoded state
    "player":  str                    — player to move
    "pi":      np.ndarray (A,)        — MCTS visit-count policy
  }
  After the game ends, each step is augmented with:
    "value":  float   — final reward for that player
"""

from __future__ import annotations
import copy
import multiprocessing as mp
import os
import time
from typing import Any

import numpy as np

from alphazero.games.base import Game
from alphazero.mcts import MCTS


# ── single game ────────────────────────────────────────────────────────────────

def self_play_game(
    game: Game,
    network,
    num_simulations: int = 400,
    temperature_threshold: int = 30,
    players: list[str] | None = None,
    max_steps: int = 400,
) -> list[dict]:
    """Play one full game; return a list of training samples.

    Args:
        temperature_threshold: Use temperature=1 for the first N half-moves
            (to encourage exploration), then temperature=0.
        max_steps: Hard cutoff. Keep this low — Journey rarely terminates with
            random play, and a growing board makes each step progressively slower.
    """
    mcts = MCTS(game, network, num_simulations=num_simulations)
    state = game.initial_state(players)
    samples: list[dict] = []
    step = 0

    while not game.is_terminal(state) and step < max_steps:
        player = game.current_player(state)
        if player is None:
            break

        temp = 1.0 if step < temperature_threshold else 0.0
        pi = mcts.policy(state, temperature=temp)

        samples.append({
            "state": game.encode_state(state, player),
            "player": player,
            "pi": pi.copy(),
        })

        # Sample action from policy
        action_idx = int(np.random.choice(len(pi), p=pi / pi.sum()))
        legal = game.legal_actions(state)
        action = game.index_to_action(action_idx)

        if action not in legal:
            # Policy index doesn't directly map back to a legal action —
            # fall back to the legal action whose index is closest.
            legal_indices = [game.action_to_index(a) for a in legal]
            action_idx = min(legal_indices, key=lambda i: abs(i - action_idx))
            action = game.index_to_action(action_idx)
            if action not in legal:
                # Last resort: pick any legal action
                action = next(iter(legal))

        state = legal[action]
        step += 1

    # Augment samples with final values.
    # If the game didn't finish, score by beacons placed on the cipher so the
    # value head has a non-trivial signal to learn from.
    if game.is_terminal(state):
        rewards = game.rewards(state)
    else:
        rewards = _partial_rewards(state)
    for sample in samples:
        sample["value"] = rewards.get(sample["player"], 0.0)

    return samples


def _shaped_score(state: dict, player: str) -> float:
    """Weighted progress score for one player.

    Weights are deliberately skewed toward cipher beacons (the true objective)
    so the agent isn't nudged to build stations as an end in itself.

      cipher beacons placed : 10
      board beacons placed  :  2  (instrumental — discovered via tower)
      stations activated    :  3  (instrumental — actually using structures)
      stations built        :  1  (weakest hint — structure exists but unused)
    """
    score = 0.0

    # Cipher beacons (primary objective)
    for pos_entry in state.get("cipher", {}).values():
        for color_entry in pos_entry.get("colors", {}).values():
            score += 10 * color_entry.get(player, 0)

    # Board beacons placed by this player
    for tile in state.get("board", {}).values():
        if tile.get("beacon") == player:
            score += 2

    # Stations built and activated
    pstate = state.get("players", {}).get(player, {})
    for info in pstate.get("stations", {}).values():
        score += 1                          # built
        if info.get("level", 1) > 1:
            score += 3                      # activated (level increases on use)

    return score


def _partial_rewards(state: dict) -> dict[str, float]:
    """Reward proxy when the game hits max_steps without terminating.

    Scores each player by _shaped_score, then normalises into [-1, 1].
    """
    players = state["turn_order"]
    scores = {p: _shaped_score(state, p) for p in players}
    values = list(scores.values())
    lo, hi = min(values), max(values)
    spread = max(hi - lo, 1.0)
    return {p: 2.0 * (scores[p] - lo) / spread - 1.0 for p in players}


# ── replay buffer ─────────────────────────────────────────────────────────────

class ReplayBuffer:
    """Fixed-capacity circular buffer of training samples."""

    def __init__(self, capacity: int = 100_000):
        self.capacity = capacity
        self._buffer: list[dict] = []
        self._pos = 0

    def push(self, samples: list[dict]):
        for sample in samples:
            if len(self._buffer) < self.capacity:
                self._buffer.append(sample)
            else:
                self._buffer[self._pos] = sample
            self._pos = (self._pos + 1) % self.capacity

    def sample(self, batch_size: int) -> list[dict]:
        indices = np.random.choice(len(self._buffer), size=min(batch_size, len(self._buffer)), replace=False)
        return [self._buffer[i] for i in indices]

    def __len__(self) -> int:
        return len(self._buffer)


# ── worker (multiprocessing) ──────────────────────────────────────────────────

def worker(
    game: Game,
    network_path: str,
    num_games: int,
    result_queue: mp.Queue,
    num_simulations: int = 400,
):
    """Worker function for a separate process.  Loads network, plays games,
    pushes samples to result_queue.

    Call via multiprocessing.Process(target=worker, args=(...)).
    """
    import torch
    from alphazero.network import AlphaZeroNetwork

    net = AlphaZeroNetwork.for_game(game)
    net.load(network_path)
    net.eval()

    for _ in range(num_games):
        samples = self_play_game(game, net, num_simulations=num_simulations)
        result_queue.put(samples)
