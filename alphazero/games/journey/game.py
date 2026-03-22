"""JourneyGame — Game subclass wiring together state, choices, and encoding."""

from __future__ import annotations
from typing import Any

import numpy as np

from alphazero.games.base import Game
from . import state as gs
from . import choices as ch
from . import encoding as enc


class JourneyGame(Game):
    """AlphaZero-compatible wrapper for Journey."""

    DEFAULT_PLAYERS = ["alice", "bob", "carol", "dave", "eve"]

    def __init__(self, num_players: int = 5):
        self._num_players = num_players

    # ── identity ───────────────────────────────────────────────────────────────

    @property
    def name(self) -> str:
        return "journey"

    @property
    def num_players(self) -> int:
        return self._num_players

    # ── state ──────────────────────────────────────────────────────────────────

    def initial_state(self, players: list[str] | None = None) -> dict:
        if players is None:
            players = self.DEFAULT_PLAYERS[: self._num_players]
        return gs.initial_state(players)

    def current_player(self, state: dict) -> str | None:
        return gs.current_player(state)

    # ── moves ──────────────────────────────────────────────────────────────────

    def legal_actions(self, state: dict) -> dict[Any, Any]:
        _, actions = ch.find_state(state)
        return actions

    def is_terminal(self, state: dict) -> bool:
        return bool(state.get("game_over"))

    def rewards(self, state: dict) -> dict[str, float]:
        """Normalise beacon scores into [-1, 1] by comparing to max."""
        game_over = state.get("game_over") or {}
        scores = game_over.get("scores", {})
        if not scores:
            # Loss or no scores
            return {p: -1.0 for p in state["turn_order"]}
        max_score = max(scores.values())
        min_score = min(scores.values())
        spread = max(max_score - min_score, 1)
        result = {}
        for player in state["turn_order"]:
            raw = scores.get(player, 0)
            # Scale so winner = +1, last place = -1
            result[player] = 2.0 * (raw - min_score) / spread - 1.0
        return result

    # ── encoding ───────────────────────────────────────────────────────────────

    def encode_state(self, state: dict, player: str) -> np.ndarray:
        return enc.encode_state(state, player)

    def action_space_size(self) -> int:
        return enc.ACTION_SPACE_SIZE

    def action_to_index(self, action: Any) -> int:
        return enc.action_to_index(action)

    def index_to_action(self, index: int) -> Any:
        return enc.index_to_action(index)
