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
    control=None,
    truncation: str = "shaped",
    stats: dict | None = None,
    repetition_limit: int = 0,
    observer=None,
) -> list[dict]:
    """Play one full game; return a list of training samples.

    Args:
        temperature_threshold: Use temperature=1 for the first N half-moves
            (to encourage exploration), then temperature=0.
        max_steps: Hard cutoff. Keep this low — Journey rarely terminates with
            random play, and a growing board makes each step progressively slower.
    """
    mcts = MCTS(game, network, num_simulations=num_simulations, control=control)
    state = game.initial_state(players)
    samples: list[dict] = []
    step = 0
    seen = {}
    repetition_key = getattr(game, "repetition_key", None)
    reason = "max_steps"
    if observer:
        observer(state, step)

    while not game.is_terminal(state) and step < max_steps:
        if control:
            control()
        if repetition_limit and repetition_key is not None:
            key = repetition_key(state)
            seen[key] = seen.get(key, 0) + 1
            if seen[key] >= repetition_limit:
                reason = "repetition"
                break
        player = game.current_player(state)
        if player is None:
            reason = "no_player"
            break

        if not game.legal_actions(state):
            reason = "no_legal_actions"
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
            raise RuntimeError("MCTS selected an illegal action")

        state = legal[action]
        step += 1
        if observer:
            observer(state, step, action_idx)

    # Augment samples with final values.
    # If the game didn't finish, score by beacons placed on the cipher so the
    # value head has a non-trivial signal to learn from.
    if game.is_terminal(state):
        rewards = game.rewards(state)
    else:
        rewards = (_partial_rewards(state) if truncation == "shaped"
                   else {p: 0.0 for p in state["turn_order"]})
    if stats is not None:
        stats.update(terminal=game.is_terminal(state), steps=step,
                     winner=state.get("state", {}).get("winner"),
                     termination="win" if game.is_terminal(state) else reason)
    if not game.is_terminal(state) and truncation == "discard":
        return []
    for sample in samples:
        if getattr(network, "value_size", 1) > 1:
            order = state["turn_order"]
            i = order.index(sample["player"])
            sample["value"] = [rewards[p] for p in order[i:] + order[:i]]
        else:
            sample["value"] = rewards.get(sample["player"], 0.0)

    return samples


def _shaped_score(state: dict, player: str) -> float:
    """Weighted progress score for one player.

    Dispatches on game type by inspecting state structure.

    Journey weights:
      cipher beacons placed : 10
      board beacons placed  :  2  (instrumental — discovered via tower)
      stations activated    :  3  (instrumental — actually using structures)
      stations built        :  1  (weakest hint — structure exists but unused)

    Organism weights:
      captures              :  5  (primary objective)
      living organisms      :  2  (organism-victory path)
      total food held       :  0.1 (minor hint — feeding supports growth)
    """
    # ── AbstractGame state (has "elements" at top level) ──
    if "elements" in state and "current_player" in state and "captures" in state:
        score = 0.0
        inner = state
        score += 5 * inner.get("captures", {}).get(player, 0)
        # Elements owned by player count as minor bonus
        score += 0.5 * sum(1 for e in inner.get("elements", {}).values() if e["player"] == player)
        return score

    # ── Organism state (has "elements" key in nested "state") ──
    if "elements" in state.get("state", {}):
        score = 0.0
        inner = state["state"]
        # Captures
        score += 5 * len(inner.get("captures", {}).get(player, []))
        # Living organisms
        elements_by_org: dict = {}
        for el in inner.get("elements", {}).values():
            if el["player"] == player:
                elements_by_org.setdefault(el["organism"], []).append(el)
        for org_elements in elements_by_org.values():
            types = {e["type"] for e in org_elements}
            if len(types) >= 3:
                score += 2
        # Food held
        for el in inner.get("elements", {}).values():
            if el["player"] == player:
                score += 0.1 * el.get("food", 0)
        return score

    # ── Journey state (flat dict with "board", "cipher", etc.) ──
    score = 0.0
    for pos_entry in state.get("cipher", {}).values():
        for color_entry in pos_entry.get("colors", {}).values():
            score += 10 * color_entry.get(player, 0)
    for tile in state.get("board", {}).values():
        if tile.get("beacon") == player:
            score += 2
    pstate = state.get("players", {}).get(player, {})
    for info in pstate.get("stations", {}).values():
        score += 1
        if info.get("level", 1) > 1:
            score += 3
    return score


def _partial_rewards(state: dict) -> dict[str, float]:
    """Reward proxy when the game hits max_steps without terminating.

    Scores each player by _shaped_score, then normalises into [-1, 1].
    """
    players = state["turn_order"]
    scores = {p: _shaped_score(state, p) for p in players}
    values = list(scores.values())
    mean = sum(values) / len(values)
    scale = max(max(abs(v - mean) for v in values), 1.0)
    return {p: (scores[p] - mean) / scale for p in players}


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
