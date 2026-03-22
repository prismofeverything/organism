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
    max_steps: int = 2000,
) -> list[dict]:
    """Play one full game; return a list of training samples.

    Args:
        temperature_threshold: Use temperature=1 for the first N half-moves
            (to encourage exploration), then temperature=0.
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

    # Augment samples with final values
    rewards = game.rewards(state) if game.is_terminal(state) else {p: 0.0 for p in state["turn_order"]}
    for sample in samples:
        sample["value"] = rewards.get(sample["player"], 0.0)

    return samples


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
