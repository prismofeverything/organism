"""AbstractGame: generic single-action-per-turn board game.

Implements the Game interface for any RuleSet.  One action per turn keeps
the turn structure simple (like Go/Chess) while the RuleSet parameters
control which mechanics are active and how conflicts resolve.

Action index layout  (N = number of board spaces, T = num_types):
  action_type_slot × N² + from_idx × N + to_idx

action_type_slot:
  0            MOVE
  1            EAT          (if food_enabled)
  2 .. T+1     GROW_type    (if food_enabled; slot = 2 + grow_type)
  T+2          CAPTURE      (direct; if can_capture)
  T+3          CIRCULATE    (if food_enabled)
  T+4          PASS         (always; to_idx = from_idx = 0)
"""

from __future__ import annotations
import copy
import numpy as np
from typing import Any

from alphazero.games.base import Game
from alphazero.games.organism.board import build_board
from .ruleset import RuleSet

Space = tuple[int, int]

# ── action type slots ─────────────────────────────────────────────────────────

_MOVE       = 0
_EAT        = 1
# _GROW_t   = 2 + t
def _CAPTURE(T): return T + 2
def _CIRCULATE(T): return T + 3
def _PASS(T):    return T + 4
def _N_ACTION_TYPES(T): return T + 5


class AbstractGame(Game):
    """A parameterised abstract strategy game for ruleset search."""

    def __init__(self, ruleset: RuleSet):
        self.rs = ruleset
        T = ruleset.num_types
        self.T = T

        # Build ring board
        rings, adjacencies, center, all_spaces = build_board(
            symmetry=ruleset.board_symmetry,
            num_rings=ruleset.num_rings,
            remove_notches=True,
        )
        self.rings       = rings
        self.adjacencies = adjacencies
        self.center      = center
        self.all_spaces  = all_spaces
        N = len(all_spaces)
        self.N = N

        self.space_to_idx = {s: i for i, s in enumerate(all_spaces)}
        self.idx_to_space = all_spaces

        # Action space
        self._at_count = _N_ACTION_TYPES(T)
        self._act_size  = self._at_count * N * N

        # Square encoding grid
        self.grid_size = max(
            ruleset.num_rings,
            (ruleset.num_rings - 1) * ruleset.board_symmetry,
        )

        # Channel layout: P*(T+2) player channels + 2 global + P captures
        P = ruleset.num_players
        self._P = P
        self.num_channels = P * (T + 2) + 2 + P

        # Player labels (strings "0".."P-1")
        self._players = [str(i) for i in range(P)]

        # Starting positions: evenly distributed on the outer ring
        outer = rings[-1]
        n_outer = len(outer)
        self._starting: dict[str, list[Space]] = {}
        spacing = max(1, n_outer // P)
        for p_idx, player in enumerate(self._players):
            base_offset = p_idx * spacing + 1
            spaces: list[Space] = []
            for e in range(ruleset.elements_per_player):
                step = (base_offset + e * 2) % n_outer
                sp: Space = (ruleset.num_rings - 1, step)
                if sp not in adjacencies:
                    # Notched corner — find nearest valid space
                    for delta in range(1, n_outer):
                        sp2: Space = (ruleset.num_rings - 1, (step + delta) % n_outer)
                        if sp2 in adjacencies:
                            sp = sp2
                            break
                spaces.append(sp)
            self._starting[player] = spaces

    # ── Game interface ─────────────────────────────────────────────────────────

    @property
    def name(self) -> str:
        rs = self.rs
        return (f"abstract_s{rs.board_symmetry}r{rs.num_rings}"
                f"t{rs.num_types}p{rs.num_players}")

    @property
    def num_players(self) -> int:
        return self.rs.num_players

    def initial_state(self, players=None) -> dict:
        rs = self.rs
        state: dict = {
            "round":          0,
            "elements":       {},
            "free_food":      {},
            "captures":       {p: 0 for p in self._players},
            "current_player": self._players[0],
            "winner":         None,
            "turn_order":     list(self._players),  # required by MCTS._evaluate
        }
        for p_idx, player in enumerate(self._players):
            for e_idx, space in enumerate(self._starting[player]):
                etype = e_idx % rs.num_types
                state["elements"][space] = {
                    "player": player,
                    "type":   etype,
                    "food":   rs.food_initial,
                }
        return state

    def current_player(self, state: dict) -> str | None:
        return None if state["winner"] else state["current_player"]

    def legal_actions(self, state: dict) -> dict[int, dict]:
        if state["winner"]:
            return {}
        rs    = self.rs
        T     = self.T
        N     = self.N
        player = state["current_player"]
        actions: dict[int, dict] = {}

        for space, el in list(state["elements"].items()):
            if el["player"] != player:
                continue
            fi = self.space_to_idx[space]
            adj = self.adjacencies.get(space, [])

            # ── MOVE ──────────────────────────────────────────────────────
            if rs.can_move:
                for to_sp in adj:
                    if to_sp in state["elements"]:
                        continue
                    # Cannot move to a space adjacent to same-type enemy
                    blocked = any(
                        other["type"] == el["type"] and other["player"] != player
                        for other in (state["elements"].get(a2) for a2 in self.adjacencies.get(to_sp, []))
                        if other
                    )
                    if not blocked:
                        ti = self.space_to_idx[to_sp]
                        idx = _MOVE * N * N + fi * N + ti
                        actions[idx] = self._apply(state, "move", space, to_sp)

            # ── EAT ───────────────────────────────────────────────────────
            if rs.can_eat and rs.food_enabled:
                for to_sp in adj:
                    if state["free_food"].get(to_sp, 0) > 0:
                        ti = self.space_to_idx[to_sp]
                        idx = _EAT * N * N + fi * N + ti
                        if idx not in actions:
                            actions[idx] = self._apply(state, "eat", space, to_sp)

            # ── GROW ──────────────────────────────────────────────────────
            if rs.can_grow and rs.food_enabled and el["food"] > 0:
                for grow_t in range(T):
                    for to_sp in adj:
                        if to_sp not in state["elements"]:
                            ti  = self.space_to_idx[to_sp]
                            idx = (2 + grow_t) * N * N + fi * N + ti
                            if idx not in actions:
                                actions[idx] = self._apply(state, "grow", space, to_sp,
                                                            grow_type=grow_t)

            # ── CAPTURE (direct) ──────────────────────────────────────────
            if rs.can_capture:
                for to_sp in adj:
                    other = state["elements"].get(to_sp)
                    if other and other["player"] != player:
                        res = rs.outcome_for(el["type"], other["type"])
                        if res == 1:
                            ti  = self.space_to_idx[to_sp]
                            idx = _CAPTURE(T) * N * N + fi * N + ti
                            if idx not in actions:
                                actions[idx] = self._apply(state, "capture", space, to_sp)

            # ── CIRCULATE ─────────────────────────────────────────────────
            if rs.can_circulate and rs.food_enabled and el["food"] > 0:
                for to_sp in adj:
                    other = state["elements"].get(to_sp)
                    if other and other["player"] == player and other["food"] < 20:
                        ti  = self.space_to_idx[to_sp]
                        idx = _CIRCULATE(T) * N * N + fi * N + ti
                        if idx not in actions:
                            actions[idx] = self._apply(state, "circulate", space, to_sp)

        # ── PASS (always available) ────────────────────────────────────────
        pass_idx = _PASS(T) * N * N
        actions[pass_idx] = self._advance_turn(copy.deepcopy(state))

        return actions

    # ── action application ────────────────────────────────────────────────────

    def _apply(self, state: dict, action: str, from_sp: Space, to_sp: Space, **kw) -> dict:
        state = copy.deepcopy(state)

        if action == "move":
            el = state["elements"].pop(from_sp)
            el["space"] = to_sp
            free = state["free_food"].pop(to_sp, 0)
            el["food"] += free
            state["elements"][to_sp] = el
            state = self._resolve_at(state, to_sp, state["current_player"])

        elif action == "eat":
            food = state["free_food"].pop(to_sp, 0)
            state["elements"][from_sp]["food"] += max(food, 1)

        elif action == "grow":
            grow_t = kw["grow_type"]
            state["elements"][from_sp]["food"] -= 1
            new_el = {"player": state["current_player"], "type": grow_t, "food": 0}
            state["elements"][to_sp] = new_el
            state = self._resolve_at(state, to_sp, state["current_player"])

        elif action == "capture":
            other = state["elements"].pop(to_sp, None)
            if other:
                dropped = 1 + other.get("food", 0)
                state["free_food"][to_sp] = state["free_food"].get(to_sp, 0) + dropped
                state["captures"][state["current_player"]] = (
                    state["captures"].get(state["current_player"], 0) + 1
                )

        elif action == "circulate":
            state["elements"][from_sp]["food"] -= 1
            state["elements"][to_sp]["food"]   += 1

        state = self._check_win(state)
        if not state["winner"]:
            state = self._advance_turn(state)
        return state

    def _resolve_at(self, state: dict, space: Space, active: str) -> dict:
        """Resolve conflicts for the element at space with adjacent enemies."""
        rs = self.rs
        if space not in state["elements"]:
            return state
        el = state["elements"][space]

        for adj in list(self.adjacencies.get(space, [])):
            if space not in state["elements"]:
                break
            el = state["elements"][space]
            other = state["elements"].get(adj)
            if not other or other["player"] == el["player"]:
                continue

            raw = rs.conflict_outcome(el["type"], other["type"])
            if raw == 0:
                continue

            # Mutual destruction
            if raw == 3:
                for sp in (space, adj):
                    if sp in state["elements"]:
                        dropped = 1 + state["elements"][sp].get("food", 0)
                        state["free_food"][sp] = state["free_food"].get(sp, 0) + dropped
                        del state["elements"][sp]
                state["captures"][active] = state["captures"].get(active, 0) + 1
                return state

            # One side wins
            res = rs.outcome_for(el["type"], other["type"])
            if res == 1:      # el wins
                dropped = 1 + other.get("food", 0)
                state["free_food"][adj] = state["free_food"].get(adj, 0) + dropped
                del state["elements"][adj]
                state["captures"][el["player"]] = state["captures"].get(el["player"], 0) + 1
            elif res == -1:   # other wins
                dropped = 1 + el.get("food", 0)
                state["free_food"][space] = state["free_food"].get(space, 0) + dropped
                del state["elements"][space]
                state["captures"][other["player"]] = (
                    state["captures"].get(other["player"], 0) + 1
                )
                return state  # el destroyed, stop

        return state

    def _check_win(self, state: dict) -> dict:
        rs = self.rs
        if rs.win_type == "captures":
            for p in self._players:
                if state["captures"].get(p, 0) >= rs.win_threshold:
                    state["winner"] = p
                    return state
        elif rs.win_type == "population":
            for p in self._players:
                pop = sum(1 for e in state["elements"].values() if e["player"] == p)
                if pop >= rs.win_threshold:
                    state["winner"] = p
                    return state
        return state

    def _advance_turn(self, state: dict) -> dict:
        idx  = self._players.index(state["current_player"])
        nidx = (idx + 1) % len(self._players)
        state["current_player"] = self._players[nidx]
        if nidx == 0:
            state["round"] += 1
        return state

    # ── terminal / rewards ─────────────────────────────────────────────────────

    def is_terminal(self, state: dict) -> bool:
        return bool(state["winner"])

    def rewards(self, state: dict) -> dict[str, float]:
        w = state["winner"]
        if not w:
            return {p: 0.0 for p in self._players}
        n = len(self._players)
        return {p: (1.0 if p == w else -1.0 / max(n - 1, 1)) for p in self._players}

    # ── encoding ───────────────────────────────────────────────────────────────

    def encode_state(self, state: dict, player: str) -> np.ndarray:
        T  = self.T
        P  = self._P
        G  = self.grid_size
        C  = self.num_channels
        tensor = np.zeros((C, G, G), dtype=np.float32)

        # Player ordering: current player first
        if player in self._players:
            pi  = self._players.index(player)
            ordered = self._players[pi:] + self._players[:pi]
        else:
            ordered = self._players

        for space, el in state["elements"].items():
            ri, st = space
            if ri >= G or st >= G or space not in self.adjacencies:
                continue
            opi  = ordered.index(el["player"]) if el["player"] in ordered else 0
            base = opi * (T + 2)
            tensor[base,              ri, st] = 1.0
            tensor[base + 1 + el["type"], ri, st] = 1.0
            tensor[base + T + 1,      ri, st] = min(el["food"], 10) / 10.0

        ff_ch   = P * (T + 2)
        curr_ch = ff_ch + 1
        cap_ch  = curr_ch + 1

        for space, food in state["free_food"].items():
            ri, st = space
            if ri < G and st < G and space in self.adjacencies:
                tensor[ff_ch, ri, st] = min(food, 5) / 5.0

        for space, el in state["elements"].items():
            ri, st = space
            if el["player"] == player and ri < G and st < G and space in self.adjacencies:
                tensor[curr_ch, ri, st] = 1.0

        thr = max(self.rs.win_threshold, 1)
        for i, p in enumerate(ordered):
            if i < P:
                tensor[cap_ch + i, :, :] = min(state["captures"].get(p, 0) / thr, 1.0)

        return tensor

    def action_space_size(self) -> int:
        return self._act_size

    def action_to_index(self, action) -> int:
        return int(action)

    def index_to_action(self, idx: int) -> int:
        return idx

    # ── decode action for reporting ────────────────────────────────────────────

    def decode_action(self, idx: int) -> str:
        T, N = self.T, self.N
        at = idx // (N * N)
        rem = idx % (N * N)
        fi, ti = rem // N, rem % N
        from_sp = self.idx_to_space[fi] if fi < N else "?"
        to_sp   = self.idx_to_space[ti] if ti < N else "?"
        names = ["MOVE", "EAT"] + [f"GROW_{t}" for t in range(T)] + ["CAPTURE", "CIRCULATE", "PASS"]
        aname = names[at] if at < len(names) else f"AT{at}"
        return f"{aname}({from_sp}→{to_sp})"
