"""State → tensor encoding and action ↔ index mapping for Journey.

Spatial encoding (C × H × W) where H = W = GRID_SIZE (19).

Channels (C = 6 + N + 5 + 3 + 1 = 15 + N for N players):
  0-5:   tile color one-hot (sun, silver, green, blue, purple, void)
  6:     ark position
  7:     heading token position
  8:     beacon present (any player)
  9:     station present (any player)
  10:    agent player's sundivers count (normalised)
  11..11+N-1: all players' sundiver counts (normalised), in turn order

Action index space layout (total ACTION_SPACE_SIZE = 512):
  0-360:    positional actions (grid row*GRID_SIZE + col) for launch/fly/matrix etc.
  361-366:  wrap positional actions (same grid, 2nd copy, wrap flag)
             — encoded as base_index + 361 for positions 0-5 (the 6 flank wraps)
  367-372:  scalar choices 0-5 (bonus counts, etc.)
  373-382:  type/keyword actions (done, launch, fly, convert, activate, draw, land, ...)
  383-511:  reserved / conversion keys
"""

from __future__ import annotations
import numpy as np
from . import hex as hx
from . import state as gs

TILE_COLOR_INDEX = {c: i for i, c in enumerate(gs.TILE_COLORS)}

# ── grid ───────────────────────────────────────────────────────────────────────

GRID_SIZE = hx.GRID_SIZE  # 19
N_SPATIAL_CHANNELS = 12   # before player-specific channels; see below

def _num_channels(num_players: int) -> int:
    return N_SPATIAL_CHANNELS + num_players


def encode_state(state: dict, perspective_player: str) -> np.ndarray:
    """Return float32 array of shape (C, GRID_SIZE, GRID_SIZE)."""
    num_players = len(state["turn_order"])
    C = _num_channels(num_players)
    tensor = np.zeros((C, GRID_SIZE, GRID_SIZE), dtype=np.float32)

    board = state["board"]
    ark = state["ark"]
    heading = state["heading_token"]

    for pos, tile in board.items():
        rc = hx.axial_to_grid(pos)
        if rc is None:
            continue
        row, col = rc

        # tile color
        cidx = TILE_COLOR_INDEX.get(tile["color"])
        if cidx is not None:
            tensor[cidx, row, col] = 1.0

        # ark & heading
        if pos == ark:
            tensor[6, row, col] = 1.0
        if pos == heading:
            tensor[7, row, col] = 1.0

        # beacon / station
        if tile.get("beacon"):
            tensor[8, row, col] = 1.0
        if tile.get("station"):
            tensor[9, row, col] = 1.0

        # sundivers — perspective player in channel 10
        sundiver_map = tile.get("sundivers", {})
        perspective_count = sundiver_map.get(perspective_player, 0)
        tensor[10, row, col] = min(perspective_count / 8.0, 1.0)

        # all players starting at channel 11
        for i, player in enumerate(state["turn_order"]):
            count = sundiver_map.get(player, 0)
            tensor[11 + i, row, col] = min(count / 8.0, 1.0)

    # Habitat count for perspective player (broadcast across a corner)
    habitat = gs.get_in(state, ["players", perspective_player, "habitat", "sundivers"]) or 0
    tensor[N_SPATIAL_CHANNELS - 1, 0, 0] = min(habitat / 8.0, 1.0)

    return tensor


# ── action encoding ────────────────────────────────────────────────────────────

# Block boundaries
_POS_BASE = 0               # 0-360: positional (GRID_SIZE*GRID_SIZE = 361)
_WRAP_BASE = 361            # 361-721: wrap positional
_SCALAR_BASE = 722          # 722-741: scalar 0-19
_KEYWORD_BASE = 742         # keyword/type actions
ACTION_SPACE_SIZE = 800

_KEYWORDS = [
    "done", "launch", "fly", "convert", "activate", "draw",
    "land", "continue", "move", "skip", "join",
    "none", "left", "right",
    "foundry", "matrix", "tower", "gate", "beacon",
]
_KEYWORD_INDEX = {k: i for i, k in enumerate(_KEYWORDS)}

# Conversion keys get a special encoding block
_CONV_BASE = _KEYWORD_BASE + len(_KEYWORDS)


def action_to_index(action) -> int:
    """Map any action key returned by choices.find_state to an integer."""
    if action is None:
        return 0

    # Positional tuple (q, r)
    if isinstance(action, tuple) and len(action) == 2 and isinstance(action[0], int):
        rc = hx.axial_to_grid(action)
        if rc:
            row, col = rc
            return _POS_BASE + row * GRID_SIZE + col
        return 0  # out of range — map to 0

    # Wrap: ("wrap", (q, r))
    if isinstance(action, tuple) and action[0] == "wrap":
        inner = action[1]
        rc = hx.axial_to_grid(inner)
        if rc:
            row, col = rc
            return _WRAP_BASE + row * GRID_SIZE + col
        return 0

    # Land: ("land", pos)
    if isinstance(action, tuple) and action[0] == "land":
        return index_of_keyword("land")

    # Activate: ("activate", pos) — encode as activate keyword
    if isinstance(action, tuple) and action[0] == "activate":
        return index_of_keyword("activate")

    # Conversion key: ("foundry" | "matrix", (pos, pos, ...))
    if isinstance(action, tuple) and isinstance(action[0], str):
        kw = action[0]
        if kw in _KEYWORD_INDEX:
            return index_of_keyword(kw)
        return _KEYWORD_BASE  # fallback

    # Scalar integer
    if isinstance(action, int) and 0 <= action < 20:
        return _SCALAR_BASE + action

    # Keyword string
    if isinstance(action, str):
        return index_of_keyword(action)

    return 0


def index_to_action(index: int) -> object:
    """Inverse mapping (approximate — for MCTS child selection)."""
    if _POS_BASE <= index < _WRAP_BASE:
        flat = index - _POS_BASE
        row, col = divmod(flat, GRID_SIZE)
        return hx.grid_to_axial(row, col)
    if _WRAP_BASE <= index < _SCALAR_BASE:
        flat = index - _WRAP_BASE
        row, col = divmod(flat, GRID_SIZE)
        return ("wrap", hx.grid_to_axial(row, col))
    if _SCALAR_BASE <= index < _KEYWORD_BASE:
        return index - _SCALAR_BASE
    if _KEYWORD_BASE <= index < _KEYWORD_BASE + len(_KEYWORDS):
        return _KEYWORDS[index - _KEYWORD_BASE]
    return None


def index_of_keyword(kw: str) -> int:
    idx = _KEYWORD_INDEX.get(kw)
    if idx is not None:
        return _KEYWORD_BASE + idx
    return _KEYWORD_BASE  # fallback
