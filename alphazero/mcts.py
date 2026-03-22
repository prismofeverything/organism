"""Monte Carlo Tree Search with multi-player UCB.

Design:
  - AlphaZero-style MCTS: no random rollouts, value estimate comes from the
    neural network.
  - Multi-player: each node stores a per-player value vector.
  - The UCB formula uses the value for *the player to move at that node's
    parent*, matching the standard multi-player MCTS treatment.

Usage:
  mcts = MCTS(game, network, num_simulations=800, c_puct=1.0)
  pi = mcts.policy(state)       # visit-count policy vector (action_space_size,)
  action = mcts.best_action(state)
"""

from __future__ import annotations
import math
from typing import Any

import numpy as np

from alphazero.games.base import Game


class Node:
    __slots__ = [
        "state", "player", "prior", "visit_count",
        "value_sum",   # dict player -> float
        "children",    # action_index -> Node
        "is_expanded",
    ]

    def __init__(self, state: Any, player: str | None, prior: float):
        self.state = state
        self.player = player        # player to move here
        self.prior = prior
        self.visit_count = 0
        self.value_sum: dict[str, float] = {}
        self.children: dict[int, "Node"] = {}
        self.is_expanded = False

    def value(self, player: str) -> float:
        if self.visit_count == 0:
            return 0.0
        return self.value_sum.get(player, 0.0) / self.visit_count

    def ucb_score(self, parent_visit: int, c_puct: float, selecting_player: str) -> float:
        """Upper confidence bound for selecting this child."""
        q = self.value(selecting_player)
        u = c_puct * self.prior * math.sqrt(parent_visit) / (1 + self.visit_count)
        return q + u


class MCTS:
    def __init__(
        self,
        game: Game,
        network,                    # alphazero.network.AlphaZeroNetwork
        num_simulations: int = 800,
        c_puct: float = 1.0,
        dirichlet_alpha: float = 0.3,
        dirichlet_eps: float = 0.25,
    ):
        self.game = game
        self.network = network
        self.num_simulations = num_simulations
        self.c_puct = c_puct
        self.dirichlet_alpha = dirichlet_alpha
        self.dirichlet_eps = dirichlet_eps

    # ── public API ──────────────────────────────────────────────────────────────

    def policy(self, state: Any, temperature: float = 1.0) -> np.ndarray:
        """Run MCTS and return visit-count policy vector."""
        root = Node(state, self.game.current_player(state), prior=1.0)
        self._expand(root)
        self._add_dirichlet_noise(root)

        for _ in range(self.num_simulations):
            self._simulate(root)

        counts = np.zeros(self.game.action_space_size(), dtype=np.float32)
        for action_idx, child in root.children.items():
            counts[action_idx] = child.visit_count

        if temperature == 0:
            best = int(np.argmax(counts))
            pi = np.zeros_like(counts)
            pi[best] = 1.0
            return pi
        counts = counts ** (1.0 / temperature)
        total = counts.sum()
        return counts / total if total > 0 else counts

    def best_action(self, state: Any, temperature: float = 0.0) -> Any:
        pi = self.policy(state, temperature=temperature)
        idx = int(np.argmax(pi))
        return self.game.index_to_action(idx)

    # ── internals ──────────────────────────────────────────────────────────────

    def _simulate(self, root: Node):
        path: list[tuple[Node, int]] = []   # (node, action_idx) pairs traversed
        node = root

        # Selection
        while node.is_expanded and not self.game.is_terminal(node.state):
            action_idx, node = self._select_child(node)
            path.append((node, action_idx))

        # Expansion + evaluation
        if self.game.is_terminal(node.state):
            values = self.game.rewards(node.state)
        else:
            self._expand(node)
            values = self._evaluate(node)

        # Backup
        for ancestor, _ in reversed(path):
            for player, v in values.items():
                ancestor.value_sum[player] = ancestor.value_sum.get(player, 0.0) + v
            ancestor.visit_count += 1

        # Root itself
        for player, v in values.items():
            root.value_sum[player] = root.value_sum.get(player, 0.0) + v
        root.visit_count += 1

    def _select_child(self, node: Node) -> tuple[int, "Node"]:
        selecting_player = node.player
        best_score = -float("inf")
        best_idx = -1
        best_child = None
        for action_idx, child in node.children.items():
            score = child.ucb_score(node.visit_count, self.c_puct, selecting_player or "")
            if score > best_score:
                best_score = score
                best_idx = action_idx
                best_child = child
        return best_idx, best_child

    def _expand(self, node: Node):
        legal = self.game.legal_actions(node.state)
        if not legal:
            node.is_expanded = True
            return

        # Get policy priors from network
        player = self.game.current_player(node.state)
        if player:
            state_tensor = self.game.encode_state(node.state, player)
            import torch
            with torch.no_grad():
                t = torch.FloatTensor(state_tensor).unsqueeze(0)
                log_pi, _ = self.network(t)
                priors = torch.exp(log_pi).squeeze(0).numpy()
        else:
            priors = np.ones(self.game.action_space_size()) / self.game.action_space_size()

        for action, next_state in legal.items():
            action_idx = self.game.action_to_index(action)
            prior = float(priors[action_idx]) if action_idx < len(priors) else 1e-8
            child = Node(next_state, self.game.current_player(next_state), prior)
            node.children[action_idx] = child

        node.is_expanded = True

    def _evaluate(self, node: Node) -> dict[str, float]:
        """Use network value head to estimate outcome."""
        player = self.game.current_player(node.state)
        if player is None:
            return {}
        state_tensor = self.game.encode_state(node.state, player)
        import torch
        with torch.no_grad():
            t = torch.FloatTensor(state_tensor).unsqueeze(0)
            _, value = self.network(t)
            v = float(value.squeeze())
        # For multi-player we have a single scalar from the current player's POV.
        # Distribute: current player gets v, others share -(v / (N-1)).
        players = node.state["turn_order"]
        n = len(players)
        values = {}
        for p in players:
            values[p] = v if p == player else -(v / max(n - 1, 1))
        return values

    def _add_dirichlet_noise(self, root: Node):
        if not root.children:
            return
        n = len(root.children)
        noise = np.random.dirichlet([self.dirichlet_alpha] * n)
        for (action_idx, child), eta in zip(root.children.items(), noise):
            child.prior = (
                (1 - self.dirichlet_eps) * child.prior + self.dirichlet_eps * eta
            )
