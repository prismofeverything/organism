"""Abstract Game protocol for AlphaZero self-play.

All games must subclass Game and implement the six abstract methods.
The design mirrors the Clojure `choice/find-state` architecture: states are
immutable dicts and legal moves are returned as {action_key -> next_state}.
"""

from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Any

import numpy as np


class Game(ABC):
    """Protocol that every game must satisfy to plug into the MCTS/training pipeline."""

    # ── identity ───────────────────────────────────────────────────────────────

    @property
    @abstractmethod
    def name(self) -> str:
        """Short identifier used for checkpointing, e.g. 'journey'."""

    @property
    @abstractmethod
    def num_players(self) -> int:
        """Number of simultaneous players (≥1)."""

    # ── state ──────────────────────────────────────────────────────────────────

    @abstractmethod
    def initial_state(self, players: list[str]) -> Any:
        """Return a fresh game state for the given player list."""

    @abstractmethod
    def current_player(self, state: Any) -> str | None:
        """Return the player whose turn it is, or None if game over."""

    # ── moves ──────────────────────────────────────────────────────────────────

    @abstractmethod
    def legal_actions(self, state: Any) -> dict[Any, Any]:
        """Return {action_key -> next_state} for all legal moves from state.

        This is the Clojure choice/find-state equivalent.  Action keys must be
        hashable and serialisable (see action_to_index / index_to_action).
        """

    @abstractmethod
    def is_terminal(self, state: Any) -> bool:
        """True when no more moves can be made (game over)."""

    @abstractmethod
    def rewards(self, state: Any) -> dict[str, float]:
        """Return {player -> reward} for a *terminal* state.

        Convention: reward in [-1, 1].  For a win/loss game this is typically
        +1 for winners and -1 for losers (draws → 0).  For a pure-score game
        normalise scores into this range.
        """

    # ── encoding ───────────────────────────────────────────────────────────────

    @abstractmethod
    def encode_state(self, state: Any, player: str) -> np.ndarray:
        """Encode state as a float32 numpy array from the given player's POV.

        Shape: (C, H, W) for spatial games or (D,) for flat encoding.
        The network architecture must match whichever shape you choose.
        """

    @abstractmethod
    def action_space_size(self) -> int:
        """Total number of distinct action indices the policy head must output."""

    @abstractmethod
    def action_to_index(self, action: Any) -> int:
        """Map an action key (as returned by legal_actions) to an integer index."""

    @abstractmethod
    def index_to_action(self, index: int) -> Any:
        """Inverse of action_to_index."""

    # ── helpers (may override) ──────────────────────────────────────────────────

    def legal_action_mask(self, state: Any) -> np.ndarray:
        """Boolean mask of shape (action_space_size,): True where action is legal."""
        mask = np.zeros(self.action_space_size(), dtype=bool)
        for action in self.legal_actions(state):
            mask[self.action_to_index(action)] = True
        return mask
