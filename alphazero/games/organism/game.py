"""Organism interface with seat-relative multiplayer values.

Defaults to seven rings and three-organism victory. Player counts below five
use sixfold symmetry; five players use fivefold symmetry. Starts follow the
Clojure board setup formula.
"""

from __future__ import annotations
import numpy as np

from alphazero.games.base import Game
import alphazero.games.organism.board as bd
import alphazero.games.organism.state as gs
import alphazero.games.organism.choices as ch
import alphazero.games.organism.encoding as enc


# Default 5-player starting configuration (ring 6 = index 6, symmetry 5)
DEFAULT_PLAYER_INFO = [
    ("orb",   {"starting_spaces": [(6, 2), (6, 3), (6, 4)]}),
    ("mass",  {"starting_spaces": [(6, 8), (6, 9), (6, 10)]}),
    ("brone", {"starting_spaces": [(6, 14), (6, 15), (6, 16)]}),
    ("laam",  {"starting_spaces": [(6, 20), (6, 21), (6, 22)]}),
    ("stuk",  {"starting_spaces": [(6, 26), (6, 27), (6, 28)]}),
]


class OrganismGame(Game):
    """AlphaZero wrapper for the Organism board game."""

    def __init__(
        self,
        num_players: int = 5,
        symmetry: int | None = None,
        num_rings: int = 7,
        organism_victory: int = 3,
        remove_notches: bool = True,
        player_info: list | None = None,
    ):
        if not 2 <= num_players <= 5:
            raise ValueError("Supported player counts: 2 through 5")
        symmetry = symmetry or (6 if num_players < 5 else 5)
        self.value_size = num_players
        self._num_players = num_players
        self.symmetry = symmetry
        self.num_rings = num_rings
        self.organism_victory = organism_victory

        # Build board
        rings, adjacencies, center, all_spaces = bd.build_board(
            symmetry=symmetry,
            num_rings=num_rings,
            remove_notches=remove_notches,
        )
        self.rings = rings
        self.adjacencies = adjacencies
        self.center = center
        self.all_spaces = all_spaces

        # Space ↔ index mapping
        self.space_to_idx, self.idx_to_space = enc.build_space_index(all_spaces)
        N = len(all_spaces)

        # Patch choices module with board size
        ch._N = N

        # Player info
        if player_info is None:
            import math
            total = (num_rings - 1) * symmetry
            offset = math.ceil((num_rings - 4) / 2) + (num_players == 4)
            self._player_info = [(name, {"starting_spaces": [
                (num_rings - 1, math.ceil(i * total / num_players + j + offset) % total)
                for j in range(3)]})
                for i, (name, _) in enumerate(DEFAULT_PLAYER_INFO[:num_players])]
        else:
            self._player_info = player_info

        if len(self._player_info) != num_players or any(
            space not in adjacencies for _, info in self._player_info
            for space in info["starting_spaces"]
        ):
            raise ValueError("Player setup does not fit this board")

        self._turn_order = [p for p, _ in self._player_info]

        # Tensor / action dimensions
        self._num_channels = enc.total_channels(num_players)
        self._action_size = enc.action_space_size(N)

        # Build a template game dict (without state — state is set per game)
        self._template: dict = {
            "rings": rings,
            "adjacencies": adjacencies,
            "center": center,
            "capture_limit": 5,
            "players": {p: {**info, "capture_limit": 5} for p, info in self._player_info},
            "turn_order": self._turn_order,
            "organism_victory": organism_victory,
            "space_to_idx": self.space_to_idx,
            "idx_to_space": self.idx_to_space,
        }

    # ── Game interface ──────────────────────────────────────────────────────────

    @property
    def name(self) -> str:
        return f"organism_{self._num_players}p"

    @property
    def num_players(self) -> int:
        return self._num_players

    def initial_state(self, players: list[str] | None = None) -> dict:
        import copy
        game = copy.deepcopy(self._template)
        if players:
            # Re-key player info using supplied names
            old_order = self._turn_order
            new_order = players[:self._num_players]
            name_map = {old: new for old, new in zip(old_order, new_order)}
            game["players"] = {
                name_map.get(p, p): info
                for p, info in game["players"].items()
            }
            game["turn_order"] = [name_map.get(p, p) for p in old_order]
        game["state"] = gs.initial_state(game["turn_order"])
        return game

    def current_player(self, state: dict) -> str | None:
        winner = state["state"].get("winner")
        if winner:
            return None
        return state["state"]["player_turn"]["player"]

    def legal_actions(self, state: dict) -> dict[int, dict]:
        """Returns {action_idx: next_state} for all legal actions.

        Automatic single-step transitions (resolve_conflicts, check_integrity,
        start_next_turn) are advanced through transparently until real player
        choices are available.
        """
        ch._N = len(self.all_spaces)
        state = self._advance_automatic(state)
        phase, choices = ch.find_state(state)
        return {k: self._advance_automatic(v) for k, v in choices.items() if k >= 0}

    def is_terminal(self, state: dict) -> bool:
        return state["state"].get("winner") is not None

    def rewards(self, state: dict) -> dict[str, float]:
        """Return per-player rewards in [-1, 1] after terminal state."""
        winner = state["state"].get("winner")
        if not winner:
            return {p: 0.0 for p in state["turn_order"]}
        return {
            p: (1.0 if p == winner else -1.0 / (self._num_players - 1))
            for p in state["turn_order"]
        }

    def repetition_key(self, state: dict):
        """Exact decision state, excluding the round counter (unused by rules).

        Food, captures, player, pending actions and partial payments all matter.
        Matching only occupied spaces would mistake preparation for a loop.
        """
        def freeze(value):
            if isinstance(value, dict):
                return ("dict", frozenset((freeze(k), freeze(v)) for k, v in value.items()))
            if isinstance(value, (list, tuple)):
                return (type(value).__name__, tuple(map(freeze, value)))
            if isinstance(value, set):
                return ("set", frozenset(map(freeze, value)))
            return (type(value).__name__, value)
        return freeze({k: v for k, v in state["state"].items() if k != "round"})

    def encode_state(self, state: dict, player: str) -> np.ndarray:
        return enc.encode_state(state, player)

    def action_space_size(self) -> int:
        return self._action_size

    def action_to_index(self, action) -> int:
        # Actions are already integer indices
        return int(action)

    def index_to_action(self, idx: int) -> int:
        return idx

    def _advance_automatic(self, state: dict) -> dict:
        """Advance through any automatic (single-choice) transitions."""
        ch._N = len(self.all_spaces)
        for _ in range(100):  # safety limit
            advance = state["state"]["player_turn"].get("advance")
            winner = gs.victory(state) if advance != "resolve_conflicts" else None
            if winner:
                import copy
                state = copy.deepcopy(state)
                state["state"]["winner"] = winner
                return state
            phase, choices = ch.find_state(state, automatic_only=True)
            if phase == "game_over":
                break
            # Automatic if all keys are negative sentinels
            if choices and all(k < 0 for k in choices):
                state = next(iter(choices.values()))
            else:
                break
        return state
