"""Universal game engine: interprets RuleSetV3 programs.

No hardcoded game logic. Everything is AST evaluation.
"""

from __future__ import annotations
import copy
from collections import deque

import numpy as np

from alphazero.games.base import Game
from .tiling import generate_topology
from .schema import (
    RuleSetV3, StateSchema, Rule, TerminationSpec,
    SRC, TGT, SELF, ENEMY, W_TRIGGERING_PLAYER, W_HIGHEST_COUNTER,
    W_MOST_PIECES, W_LAST_STANDING, W_TERRITORY,
)
from .interpreter import (
    eval_predicate, apply_effect, eval_termination,
    predicate_max_range, _empty_node, _conflict_outcome,
)


class GameV3(Game):
    """A game defined by a RuleSetV3 program."""

    def __init__(self, ruleset: RuleSetV3):
        self.rs = ruleset
        s = ruleset.schema
        self.topo = generate_topology(s.tiling_spec())
        self.N = self.topo.N
        self.T = s.num_piece_types
        self._P = s.num_players
        self._players = list(range(s.num_players))
        self._n_rules = len(ruleset.rules)
        self._act_size = (self._n_rules + 1) * self.N * self.N  # +1 for PASS
        self.grid_size = self.topo.grid_size
        R = s.num_resource_slots
        self.num_channels = s.num_players * (1 + self.T + R) + R + 1 + s.num_global_counters

        # Precompute max range per rule for search optimization
        self._rule_ranges = [predicate_max_range(r.predicate) for r in ruleset.rules]

    # ── Game interface ────────────────────────────────────────────────────────

    @property
    def name(self) -> str:
        return f"v3_{self.rs.short_id()}"

    @property
    def num_players(self) -> int:
        return self._P

    def initial_state(self, players=None) -> dict:
        s = self.rs.schema
        # Initialize all nodes as empty
        nodes = {}
        for node_id in self.topo.nodes:
            nodes[node_id] = _empty_node(s)

        # Place starting pieces
        if self.rs.placement != "empty":
            epp = max(1, min(self.N // (self._P * 2), 4))  # auto-scale
            spacing = max(1, self.N // self._P)
            for p in self._players:
                for e in range(epp):
                    idx = (p * spacing + 1 + e * 2) % self.N
                    # Find unoccupied
                    for attempt in range(self.N):
                        node = self.topo.nodes[(idx + attempt) % self.N]
                        if nodes[node]["owner"] < 0:
                            nodes[node]["owner"] = p
                            nodes[node]["piece_type"] = e % max(self.T, 1)
                            for r in range(s.num_resource_slots):
                                nodes[node][f"resource_{r}"] = 1  # start with 1
                            break

        # Initialize globals
        globals_dict = {
            "round": 0,
            "current_player": 0,
            "actions_remaining": self.rs.actions_per_turn,
        }
        for i in range(s.num_global_counters):
            globals_dict[f"counter_{i}"] = 0

        return {
            "nodes": nodes,
            "globals": globals_dict,
            "winner": None,
            "turn_order": list(self._players),
        }

    def current_player(self, state: dict) -> str | None:
        if state.get("winner") is not None:
            return None
        return str(state["globals"]["current_player"])

    def is_terminal(self, state: dict) -> bool:
        return state.get("winner") is not None

    def rewards(self, state: dict) -> dict[str, float]:
        w = state.get("winner")
        if w is None:
            return {str(p): 0.0 for p in self._players}
        n = len(self._players)
        return {str(p): (1.0 if p == w else -1.0 / max(n - 1, 1))
                for p in self._players}

    def action_space_size(self) -> int:
        return self._act_size

    def action_to_index(self, action) -> int:
        return int(action)

    def index_to_action(self, idx: int) -> int:
        return idx

    # ── Legal actions ─────────────────────────────────────────────────────────

    def legal_actions(self, state: dict) -> dict[int, dict]:
        if state.get("winner") is not None:
            return {}
        player = state["globals"]["current_player"]
        N = self.N
        actions: dict[int, dict] = {}

        for slot, rule in enumerate(self.rs.rules):
            max_range = self._rule_ranges[slot]
            if rule.requires_piece:
                owned = [n for n, nd in state["nodes"].items()
                         if nd.get("owner") == player]
                for src_node in owned:
                    targets = self._nodes_within(src_node, max_range)
                    for tgt_node in targets:
                        if eval_predicate(rule.predicate, state, self.rs.schema,
                                          self.topo, src_node, tgt_node, player,
                                          self.rs.conflict_table):
                            idx = slot * N * N + src_node * N + tgt_node
                            if idx not in actions:
                                new_state = self._apply_rule(state, rule, src_node, tgt_node, player)
                                actions[idx] = new_state
            else:
                for tgt_node in self.topo.nodes:
                    if eval_predicate(rule.predicate, state, self.rs.schema,
                                      self.topo, tgt_node, tgt_node, player,
                                      self.rs.conflict_table):
                        idx = slot * N * N + tgt_node
                        if idx not in actions:
                            new_state = self._apply_rule(state, rule, tgt_node, tgt_node, player)
                            actions[idx] = new_state

        # PASS
        pass_idx = self._n_rules * N * N
        pass_state = self._advance(copy.deepcopy(state))
        actions[pass_idx] = pass_state

        return actions

    def _nodes_within(self, src: int, max_range: int) -> list[int]:
        if max_range <= 1:
            return self.topo.adjacencies.get(src, [])
        visited = {src}
        frontier = deque([(src, 0)])
        result = []
        while frontier:
            node, dist = frontier.popleft()
            if dist > 0:
                result.append(node)
            if dist < max_range:
                for nb in self.topo.adjacencies.get(node, []):
                    if nb not in visited:
                        visited.add(nb)
                        frontier.append((nb, dist + 1))
        return result

    def _apply_rule(self, state: dict, rule: Rule, src: int, tgt: int, player: int) -> dict:
        state = copy.deepcopy(state)
        state = apply_effect(rule.effect, state, self.rs.schema,
                             self.topo, src, tgt, player)
        state = self._resolve_interactions(state, src, tgt, player)
        state = self._check_termination(state, player)
        if state.get("winner") is None:
            state = self._advance(state)
        return state

    # ── Interactions ──────────────────────────────────────────────────────────

    def _resolve_interactions(self, state: dict, src: int, tgt: int, player: int) -> dict:
        for ir in self.rs.interactions:
            if ir.trigger == "adjacency":
                affected = tgt if tgt in state["nodes"] else src
                for adj in self.topo.adjacencies.get(affected, []):
                    nd_a = state["nodes"].get(affected, {})
                    nd_b = state["nodes"].get(adj, {})
                    if (nd_a.get("owner", -1) >= 0 and nd_b.get("owner", -1) >= 0 and
                            nd_a["owner"] != nd_b["owner"]):
                        if eval_predicate(ir.predicate, state, self.rs.schema,
                                          self.topo, affected, adj, player,
                                          self.rs.conflict_table):
                            state = apply_effect(ir.effect, state, self.rs.schema,
                                                 self.topo, affected, adj, player)
        return state

    # ── Termination ───────────────────────────────────────────────────────────

    def _check_termination(self, state: dict, player: int) -> dict:
        for ts in self.rs.terminations:
            if eval_termination(ts.predicate, state, self.rs.schema, player, self.topo):
                winner = self._determine_winner(state, ts, player)
                state["winner"] = winner
                return state
        # Turn limit fallback
        if state["globals"].get("round", 0) >= self.rs.turn_limit:
            state["winner"] = self._score_leader(state)
        return state

    def _determine_winner(self, state: dict, ts: TerminationSpec, player: int) -> int:
        if ts.winner_mode == W_TRIGGERING_PLAYER:
            return player
        elif ts.winner_mode == W_HIGHEST_COUNTER:
            idx = ts.winner_arg
            key = f"counter_{idx}"
            best_p, best_v = 0, -1
            for p in self._players:
                # Counter per player: counter_{player_idx}
                v = state["globals"].get(f"counter_{p}", 0) if idx == p else 0
                # Or: use single counter, highest captures
                v = state["globals"].get(key, 0)
                if v > best_v:
                    best_p, best_v = p, v
            return best_p
        elif ts.winner_mode == W_MOST_PIECES:
            counts = {}
            for nd in state["nodes"].values():
                o = nd.get("owner", -1)
                if o >= 0:
                    counts[o] = counts.get(o, 0) + 1
            return max(self._players, key=lambda p: counts.get(p, 0))
        elif ts.winner_mode == W_TERRITORY:
            # Go-style: territory (enclosed empty regions) + captures
            from .interpreter import _find_group
            scores = {p: state["globals"].get(f"counter_{p}", 0) for p in self._players}
            # Flood-fill empty regions, assign to enclosing player
            visited = set()
            for node_id, nd in state["nodes"].items():
                if nd.get("owner", -1) >= 0 or node_id in visited:
                    continue
                region = set()
                borders = set()
                frontier = deque([node_id])
                while frontier:
                    n = frontier.popleft()
                    if n in region:
                        continue
                    n_nd = state["nodes"].get(n)
                    if n_nd and n_nd.get("owner", -1) >= 0:
                        borders.add(n_nd["owner"])
                        continue
                    region.add(n)
                    visited.add(n)
                    for nb in self.topo.adjacencies.get(n, []):
                        if nb not in region:
                            frontier.append(nb)
                if len(borders) == 1:
                    p = next(iter(borders))
                    scores[p] = scores.get(p, 0) + len(region)
            return max(self._players, key=lambda p: scores.get(p, 0))

        elif ts.winner_mode == W_LAST_STANDING:
            alive = set()
            for nd in state["nodes"].values():
                o = nd.get("owner", -1)
                if o >= 0:
                    alive.add(o)
            return alive.pop() if len(alive) == 1 else self._players[0]
        return player

    def _score_leader(self, state: dict) -> int:
        best_p, best_s = 0, -1
        for p in self._players:
            pieces = sum(1 for nd in state["nodes"].values() if nd.get("owner") == p)
            counter = state["globals"].get(f"counter_{p}", 0)
            score = counter * 10 + pieces * 3
            if score > best_s:
                best_p, best_s = p, score
        return best_p

    # ── Turn advancement ──────────────────────────────────────────────────────

    def _advance(self, state: dict) -> dict:
        g = state["globals"]
        remaining = g.get("actions_remaining", 1) - 1
        if remaining > 0:
            g["actions_remaining"] = remaining
            return state
        # Next player
        cp = g["current_player"]
        nidx = (cp + 1) % self._P
        g["current_player"] = nidx
        g["actions_remaining"] = self.rs.actions_per_turn
        if nidx == 0:
            g["round"] = g.get("round", 0) + 1
        return state

    # ── Encoding ──────────────────────────────────────────────────────────────

    def encode_state(self, state: dict, player: str) -> np.ndarray:
        s = self.rs.schema
        P, T, R = s.num_players, s.num_piece_types, s.num_resource_slots
        G = self.grid_size
        C = self.num_channels
        tensor = np.zeros((C, G, G), dtype=np.float32)
        p_int = int(player) if isinstance(player, str) else player

        # Player ordering: current player first
        ordered = [(p_int + i) % P for i in range(P)]

        for node_id, nd in state["nodes"].items():
            rc = self.topo.coords.get(node_id)
            if rc is None:
                continue
            r, c = rc
            if r >= G or c >= G:
                continue
            owner = nd.get("owner", -1)
            if owner >= 0:
                opi = ordered.index(owner)
                base = opi * (1 + T + R)
                tensor[base, r, c] = 1.0  # presence
                pt = nd.get("piece_type", 0)
                if 0 <= pt < T:
                    tensor[base + 1 + pt, r, c] = 1.0
                for rs_idx in range(R):
                    val = nd.get(f"resource_{rs_idx}", 0)
                    tensor[base + 1 + T + rs_idx, r, c] = min(val, 10) / 10.0

        # Free resources on empty nodes
        res_base = P * (1 + T + R)
        for node_id, nd in state["nodes"].items():
            if nd.get("owner", -1) < 0:
                rc = self.topo.coords.get(node_id)
                if rc:
                    r, c = rc
                    if r < G and c < G:
                        for rs_idx in range(R):
                            tensor[res_base + rs_idx, r, c] = min(
                                nd.get(f"resource_{rs_idx}", 0), 5) / 5.0

        # Valid node mask
        mask_ch = res_base + R
        for node_id in self.topo.nodes:
            rc = self.topo.coords.get(node_id)
            if rc:
                r, c = rc
                if r < G and c < G:
                    tensor[mask_ch, r, c] = 1.0

        # Global counters
        gc_base = mask_ch + 1
        for gi in range(s.num_global_counters):
            val = state["globals"].get(f"counter_{gi}", 0)
            tensor[gc_base + gi, :, :] = min(val, 20) / 20.0

        return tensor

    def decode_action(self, idx: int) -> str:
        N = self.N
        slot = idx // (N * N)
        rem = idx % (N * N)
        src_n, tgt_n = rem // N, rem % N
        if slot < self._n_rules:
            return f"rule{slot}({src_n}→{tgt_n})"
        return f"PASS"
