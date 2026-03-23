"""Generalized game engine: any topology × any action types × any win condition.

Replaces the organism-specific AbstractGame with a fully generic engine
that evaluates ActionType predicates/effects against any Topology.
"""

from __future__ import annotations
import copy
import random
from collections import deque
from dataclasses import dataclass

import numpy as np

from alphazero.games.base import Game
from .topology import Topology, build_topology
from .ruleset_v2 import RuleSetV2, BoardSpec, PieceSpec, InteractionSpec
from .actions import ActionType, ACTION_BY_NAME


class GameV2(Game):
    """A game defined by a RuleSetV2. Plays on any topology with any action types."""

    def __init__(self, ruleset: RuleSetV2):
        self.rs = ruleset
        self.topo = build_topology(
            ruleset.board.topology, ruleset.board.size, ruleset.board.symmetry,
        )
        self.N = self.topo.N
        self.T = ruleset.pieces.num_types
        self._P = ruleset.num_players
        self._players = [str(i) for i in range(self._P)]
        self._action_types = ruleset.actions
        self._n_at = len(self._action_types) + 1  # +1 for PASS
        self._act_size = self._n_at * self.N * self.N
        self.grid_size = self.topo.grid_size
        self.num_channels = self._P * (self.T + 2) + 2 + self._P

        # Starting positions: spread on outer nodes (highest-degree or last-added)
        self._starting = self._compute_starting()

    def _compute_starting(self) -> dict[str, list[int]]:
        """Assign starting positions for each player."""
        nodes = self.topo.nodes
        N = len(nodes)
        rs = self.rs
        spacing = max(1, N // rs.num_players)
        starting: dict[str, list[int]] = {}
        for p_idx, player in enumerate(self._players):
            pieces = []
            for e_idx in range(rs.pieces.elements_per_player):
                idx = (p_idx * spacing + 1 + e_idx * 2) % N
                # Find an unoccupied node
                taken = {n for ps in starting.values() for n in ps}
                while idx in taken:
                    idx = (idx + 1) % N
                pieces.append(nodes[idx])
            starting[player] = pieces
        return starting

    # ── Game interface ────────────────────────────────────────────────────────────

    @property
    def name(self) -> str:
        return f"v2_{self.rs.short_id()}"

    @property
    def num_players(self) -> int:
        return self._P

    def initial_state(self, players=None) -> dict:
        rs = self.rs
        state = {
            "round": 0,
            "elements": {},
            "free_food": {},
            "captures": {p: 0 for p in self._players},
            "current_player": self._players[0],
            "winner": None,
            "turn_order": list(self._players),
            "actions_remaining": self._actions_per_turn(),
        }
        for player, nodes in self._starting.items():
            for e_idx, node in enumerate(nodes):
                etype = e_idx % self.T
                state["elements"][node] = {
                    "player": player,
                    "type": etype,
                    "food": rs.resources.initial_amount,
                }
        return state

    def _actions_per_turn(self) -> int:
        ts = self.rs.turns
        if ts.structure == "multi":
            return ts.actions_per_turn
        elif ts.structure == "action_points":
            return ts.action_points
        return 1

    def current_player(self, state: dict) -> str | None:
        return None if state["winner"] else state["current_player"]

    def is_terminal(self, state: dict) -> bool:
        return bool(state["winner"])

    def rewards(self, state: dict) -> dict[str, float]:
        w = state["winner"]
        if not w:
            return {p: 0.0 for p in self._players}
        n = len(self._players)
        return {p: (1.0 if p == w else -1.0 / max(n - 1, 1)) for p in self._players}

    def action_space_size(self) -> int:
        return self._act_size

    def action_to_index(self, action) -> int:
        return int(action)

    def index_to_action(self, idx: int) -> int:
        return idx

    # ── Reachability ──────────────────────────────────────────────────────────────

    def _reachable(self, from_node: int, at: ActionType, state: dict) -> list[int]:
        """Nodes reachable from from_node given action type constraints."""
        if at.max_range == 1:
            return self.topo.adjacencies.get(from_node, [])
        # BFS for range > 1
        visited = {from_node}
        frontier = deque([(from_node, 0)])
        result = []
        while frontier:
            node, dist = frontier.popleft()
            if dist > 0:
                result.append(node)
            if dist >= at.max_range:
                continue
            for nb in self.topo.adjacencies.get(node, []):
                if nb not in visited:
                    if at.can_jump or nb not in state["elements"]:
                        visited.add(nb)
                        frontier.append((nb, dist + 1))
                    elif nb in state["elements"] and dist + 1 <= at.max_range:
                        # Can't pass through, but target might be valid
                        visited.add(nb)
                        result.append(nb)
        return result

    # ── Legal actions ─────────────────────────────────────────────────────────────

    def legal_actions(self, state: dict) -> dict[int, dict]:
        if state["winner"]:
            return {}
        player = state["current_player"]
        N = self.N
        actions: dict[int, dict] = {}

        for slot, at in enumerate(self._action_types):
            if at.requires_piece:
                # For each owned piece
                for node, el in list(state["elements"].items()):
                    if el["player"] != player:
                        continue
                    # Actor type filter
                    if at.actor_type_filter >= 0 and el["type"] != at.actor_type_filter:
                        continue
                    # Resource cost check
                    if at.resource_cost > 0 and el.get("food", 0) < at.resource_cost:
                        continue
                    for target in self._reachable(node, at, state):
                        if self._check_target(state, at, player, node, target):
                            idx = slot * N * N + node * N + target
                            if idx not in actions:
                                actions[idx] = self._apply(state, at, node, target)
            else:
                # Placement: no source piece needed (e.g., PLACE action)
                for target in self.topo.nodes:
                    if self._check_target(state, at, player, -1, target):
                        idx = slot * N * N + 0 * N + target  # from=0 (unused)
                        if idx not in actions:
                            actions[idx] = self._apply(state, at, -1, target)

        # PASS always available
        pass_idx = len(self._action_types) * N * N
        actions[pass_idx] = self._advance(copy.deepcopy(state))
        return actions

    def _check_target(self, state: dict, at: ActionType, player: str,
                      from_node: int, target: int) -> bool:
        """Check if the target satisfies the action's target_owner filter."""
        occupant = state["elements"].get(target)
        if at.target_owner == "empty":
            if occupant is not None:
                return False
            # For "collect" effect, check free food exists
            if at.effect == "collect":
                return state.get("free_food", {}).get(target, 0) > 0
            return True
        elif at.target_owner == "enemy":
            if occupant is None or occupant["player"] == player:
                return False
            if at.requires_beats:
                actor_el = state["elements"].get(from_node)
                if actor_el:
                    outcome = self.rs.interactions.outcome_for(
                        self.T, actor_el["type"], occupant["type"])
                    return outcome == "wins"
            return True
        elif at.target_owner == "self":
            return occupant is not None and occupant["player"] == player
        elif at.target_owner == "any":
            return True
        return False

    # ── Action application ────────────────────────────────────────────────────────

    def _apply(self, state: dict, at: ActionType, from_node: int, to_node: int) -> dict:
        state = copy.deepcopy(state)
        player = state["current_player"]

        # Pay resource cost
        if at.resource_cost > 0 and from_node >= 0 and from_node in state["elements"]:
            state["elements"][from_node]["food"] = max(
                0, state["elements"][from_node].get("food", 0) - at.resource_cost)

        # Apply effect
        effect = at.effect
        if effect == "relocate":
            el = state["elements"].pop(from_node, None)
            if el:
                # Pick up free food on arrival
                free = state.get("free_food", {}).pop(to_node, 0)
                el["food"] = el.get("food", 0) + free
                state["elements"][to_node] = el
                state = self._resolve_interactions(state, to_node, player)

        elif effect == "remove_target":
            target_el = state["elements"].pop(to_node, None)
            if target_el:
                dropped = 1 + target_el.get("food", 0)
                state.setdefault("free_food", {})[to_node] = \
                    state.get("free_food", {}).get(to_node, 0) + dropped
                state["captures"][player] = state["captures"].get(player, 0) + 1

        elif effect == "spawn":
            spawn_t = at.spawn_type
            if spawn_t < 0 and from_node >= 0 and from_node in state["elements"]:
                spawn_t = state["elements"][from_node]["type"]
            if spawn_t < 0:
                spawn_t = 0
            state["elements"][to_node] = {"player": player, "type": spawn_t, "food": 0}
            state = self._resolve_interactions(state, to_node, player)

        elif effect == "collect":
            food = state.get("free_food", {}).pop(to_node, 0)
            if from_node >= 0 and from_node in state["elements"]:
                state["elements"][from_node]["food"] = \
                    state["elements"][from_node].get("food", 0) + max(food, 1)

        elif effect == "transfer_resource":
            if to_node in state["elements"] and from_node >= 0:
                state["elements"][to_node]["food"] = \
                    state["elements"][to_node].get("food", 0) + 1

        elif effect == "convert":
            if to_node in state["elements"]:
                state["elements"][to_node]["player"] = player

        elif effect == "push":
            target_el = state["elements"].get(to_node)
            if target_el and from_node >= 0:
                # Push away: find node adjacent to target, opposite from actor
                push_candidates = [
                    n for n in self.topo.adjacencies.get(to_node, [])
                    if n != from_node and n not in state["elements"]
                ]
                if push_candidates:
                    dest = push_candidates[0]
                    el = state["elements"].pop(to_node)
                    state["elements"][dest] = el

        elif effect == "swap":
            if from_node >= 0:
                el_a = state["elements"].pop(from_node, None)
                el_b = state["elements"].pop(to_node, None)
                if el_a:
                    state["elements"][to_node] = el_a
                if el_b:
                    state["elements"][from_node] = el_b

        elif effect == "place":
            state["elements"][to_node] = {"player": player, "type": 0, "food": 0}

        # Check win
        state = self._check_win(state)
        if not state["winner"]:
            state = self._advance(state)
        return state

    def _resolve_interactions(self, state: dict, space: int, active: str) -> dict:
        """Resolve conflicts at a space based on interaction trigger."""
        trigger = self.rs.interactions.trigger
        if trigger == "explicit":
            return state  # only explicit capture actions resolve conflicts
        if space not in state["elements"]:
            return state
        el = state["elements"][space]
        for adj in list(self.topo.adjacencies.get(space, [])):
            if space not in state["elements"]:
                break
            el = state["elements"][space]
            other = state["elements"].get(adj)
            if not other or other["player"] == el["player"]:
                continue
            outcome = self.rs.interactions.outcome_for(self.T, el["type"], other["type"])
            if outcome == "coexist":
                continue
            elif outcome == "mutual":
                for sp in (space, adj):
                    if sp in state["elements"]:
                        d = 1 + state["elements"][sp].get("food", 0)
                        state.setdefault("free_food", {})[sp] = \
                            state.get("free_food", {}).get(sp, 0) + d
                        del state["elements"][sp]
                state["captures"][active] = state["captures"].get(active, 0) + 1
                return state
            elif outcome == "wins":
                d = 1 + other.get("food", 0)
                state.setdefault("free_food", {})[adj] = \
                    state.get("free_food", {}).get(adj, 0) + d
                del state["elements"][adj]
                state["captures"][el["player"]] = state["captures"].get(el["player"], 0) + 1
            elif outcome == "loses":
                d = 1 + el.get("food", 0)
                state.setdefault("free_food", {})[space] = \
                    state.get("free_food", {}).get(space, 0) + d
                del state["elements"][space]
                state["captures"][other["player"]] = state["captures"].get(other["player"], 0) + 1
                return state
        return state

    def _check_win(self, state: dict) -> dict:
        ws = self.rs.win

        # Primary win condition
        if ws.condition == "captures":
            for p in self._players:
                if state["captures"].get(p, 0) >= ws.threshold:
                    state["winner"] = p
                    return state
        elif ws.condition == "population":
            for p in self._players:
                pop = sum(1 for e in state["elements"].values() if e["player"] == p)
                if pop >= ws.threshold:
                    state["winner"] = p
                    return state
        elif ws.condition == "elimination":
            alive = [p for p in self._players
                     if any(e["player"] == p for e in state["elements"].values())]
            if len(alive) == 1:
                state["winner"] = alive[0]
                return state
            if len(alive) == 0:
                state["winner"] = self._players[0]  # everyone dead, arbitrary
                return state
        elif ws.condition == "territory":
            total = max(self.N, 1)
            for p in self._players:
                controlled = sum(1 for e in state["elements"].values() if e["player"] == p)
                if controlled * 100 >= ws.threshold * total:
                    state["winner"] = p
                    return state

        # Turn limit fallback: whoever is ahead wins (applies to ALL conditions)
        if ws.turn_limit > 0 and state["round"] >= ws.turn_limit:
            state["winner"] = self._score_leader(state)
            return state

        return state

    def _score_leader(self, state: dict) -> str:
        """Determine the leading player by a composite score. Ties broken randomly."""
        scores: list[tuple[float, float, str]] = []
        for p in self._players:
            pop = sum(1 for e in state["elements"].values() if e["player"] == p)
            caps = state["captures"].get(p, 0)
            food = sum(e.get("food", 0) for e in state["elements"].values()
                       if e["player"] == p)
            score = caps * 10 + pop * 3 + food
            scores.append((score, random.random(), p))  # random tiebreaker
        scores.sort(reverse=True)
        return scores[0][2]

    def _advance(self, state: dict) -> dict:
        """Advance turn: decrement actions_remaining, advance player when 0."""
        remaining = state.get("actions_remaining", 1) - 1
        if remaining > 0:
            state["actions_remaining"] = remaining
            return state
        # Next player
        idx = self._players.index(state["current_player"])
        nidx = (idx + 1) % len(self._players)
        state["current_player"] = self._players[nidx]
        state["actions_remaining"] = self._actions_per_turn()
        if nidx == 0:
            state["round"] += 1
            # Energy regen
            if self.rs.resources.system == "energy":
                for el in state["elements"].values():
                    el["food"] = el.get("food", 0) + self.rs.resources.regen_rate
        return state

    # ── Encoding ──────────────────────────────────────────────────────────────────

    def encode_state(self, state: dict, player: str) -> np.ndarray:
        T, P, G = self.T, self._P, self.grid_size
        C = self.num_channels
        tensor = np.zeros((C, G, G), dtype=np.float32)

        # Player ordering: current player first
        if player in self._players:
            pi = self._players.index(player)
            ordered = self._players[pi:] + self._players[:pi]
        else:
            ordered = self._players

        for node, el in state["elements"].items():
            rc = self.topo.coords.get(node)
            if rc is None:
                continue
            r, c = rc
            if r >= G or c >= G:
                continue
            # Fog check
            if self.rs.info.visibility == "fog":
                if not self._visible(state, node, player):
                    continue
            opi = ordered.index(el["player"]) if el["player"] in ordered else 0
            base = opi * (T + 2)
            tensor[base, r, c] = 1.0
            tensor[base + 1 + el["type"], r, c] = 1.0
            tensor[base + T + 1, r, c] = min(el.get("food", 0), 10) / 10.0

        ff_ch = P * (T + 2)
        for node, food in state.get("free_food", {}).items():
            rc = self.topo.coords.get(node)
            if rc and rc[0] < G and rc[1] < G:
                tensor[ff_ch, rc[0], rc[1]] = min(food, 5) / 5.0

        curr_ch = ff_ch + 1
        for node, el in state["elements"].items():
            if el["player"] == player:
                rc = self.topo.coords.get(node)
                if rc and rc[0] < G and rc[1] < G:
                    tensor[curr_ch, rc[0], rc[1]] = 1.0

        cap_ch = curr_ch + 1
        thr = max(self.rs.win.threshold, 1)
        for i, p in enumerate(ordered):
            if i < P:
                tensor[cap_ch + i, :, :] = min(state["captures"].get(p, 0) / thr, 1.0)

        return tensor

    def _visible(self, state: dict, node: int, player: str) -> bool:
        """Check if node is within fog radius of any player piece."""
        radius = self.rs.info.fog_radius
        for n, el in state["elements"].items():
            if el["player"] == player:
                if self._node_distance(n, node) <= radius:
                    return True
        return False

    def _node_distance(self, a: int, b: int) -> int:
        """BFS shortest path distance between two nodes."""
        if a == b:
            return 0
        visited = {a}
        frontier = deque([(a, 0)])
        while frontier:
            node, dist = frontier.popleft()
            for nb in self.topo.adjacencies.get(node, []):
                if nb == b:
                    return dist + 1
                if nb not in visited:
                    visited.add(nb)
                    frontier.append((nb, dist + 1))
        return 999

    def decode_action(self, idx: int) -> str:
        N = self.N
        slot = idx // (N * N)
        rem = idx % (N * N)
        from_n, to_n = rem // N, rem % N
        if slot < len(self._action_types):
            aname = self._action_types[slot].name
        else:
            aname = "PASS"
        return f"{aname}({from_n}→{to_n})"
