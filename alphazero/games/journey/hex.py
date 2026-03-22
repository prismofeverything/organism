"""Hex-grid math — Python port of journey/game.cljc hex utilities.

Axial coordinates [q, r].  All arithmetic mirrors the Clojure implementation
so that encoded states and MCTS simulations produce identical results.
"""

from __future__ import annotations
from typing import Iterator

# Six neighbour directions (same order as Clojure hex-directions)
HEX_DIRECTIONS: list[tuple[int, int]] = [
    (1, 0), (1, -1), (0, -1), (-1, 0), (-1, 1), (0, 1)
]

DEFAULT_WRAPPING_RADIUS = 9


def add(a: tuple[int, int], b: tuple[int, int]) -> tuple[int, int]:
    return (a[0] + b[0], a[1] + b[1])


def subtract(a: tuple[int, int], b: tuple[int, int]) -> tuple[int, int]:
    """b - a  (mirrors Clojure subtract-hex convention: [q2-q1, r2-r1])."""
    return (b[0] - a[0], b[1] - a[1])


def distance(a: tuple[int, int], b: tuple[int, int]) -> int:
    dq = b[0] - a[0]
    dr = b[1] - a[1]
    return (abs(dq) + abs(dr) + abs(dq + dr)) // 2


def neighbors(pos: tuple[int, int]) -> list[tuple[int, int]]:
    q, r = pos
    return [(q + dq, r + dr) for dq, dr in HEX_DIRECTIONS]


def direction_index(d: tuple[int, int]) -> int | None:
    try:
        return HEX_DIRECTIONS.index(d)
    except ValueError:
        return None


def rotate_cw(d: tuple[int, int]) -> tuple[int, int]:
    """Rotate direction 60° clockwise."""
    idx = direction_index(d)
    if idx is None:
        raise ValueError(f"Not a unit hex direction: {d}")
    return HEX_DIRECTIONS[(idx + 1) % 6]


def rotate_ccw(d: tuple[int, int]) -> tuple[int, int]:
    """Rotate direction 60° counter-clockwise."""
    idx = direction_index(d)
    if idx is None:
        raise ValueError(f"Not a unit hex direction: {d}")
    return HEX_DIRECTIONS[(idx - 1) % 6]


def heading_direction(ark: tuple[int, int], heading_token: tuple[int, int]) -> tuple[int, int]:
    """Unit direction from ark toward heading_token."""
    return subtract(ark, heading_token)


def adjacent_direction(from_pos: tuple[int, int], to_pos: tuple[int, int]) -> int | None:
    """Direction index from from_pos toward to_pos (must be adjacent). None if not a valid direction."""
    d = subtract(from_pos, to_pos)
    return direction_index(d)


def on_ray(origin: tuple[int, int], direction: tuple[int, int], pos: tuple[int, int]) -> bool:
    """True if pos lies on the ray starting at origin going in direction."""
    dq = pos[0] - origin[0]
    dr = pos[1] - origin[1]
    ddq, ddr = direction
    if ddq == 0 and ddr == 0:
        return False
    if ddq != 0:
        if dq % ddq != 0:
            return False
        t = dq // ddq
        return t > 0 and dr == t * ddr
    else:
        if dr % ddr != 0:
            return False
        t = dr // ddr
        return t > 0 and dq == t * ddq


def wrap_target(
    board_positions: set[tuple[int, int]],
    wrapping_radius: int,
    origin: tuple[int, int],
    direction: tuple[int, int],
) -> tuple[int, int]:
    """Find the wrap-around landing position when flying off the edge of known space."""
    # Walk backward from origin along -direction until we leave the board radius.
    # The wrap target is the first position on the opposite side.
    q, r = origin
    dq, dr = direction
    # Find furthest board tile on the opposite ray
    opposite = (-dq, -dr)
    candidates = [
        p for p in board_positions
        if on_ray(origin, opposite, p)
    ]
    if candidates:
        # Use the furthest one as the anchor
        furthest = max(candidates, key=lambda p: distance(origin, p))
        return add(furthest, direction)
    # Fallback: wrap by diameter
    return add(origin, (dq * 2 * wrapping_radius, dr * 2 * wrapping_radius))


# ── Grid encoding helpers ──────────────────────────────────────────────────────

# We use a fixed (2R+1) × (2R+1) offset grid for tensor encoding.
# Axial (q,r) → array index (row, col) = (r + R, q + R).
ENCODING_RADIUS = DEFAULT_WRAPPING_RADIUS
GRID_SIZE = 2 * ENCODING_RADIUS + 1  # 19


def axial_to_grid(pos: tuple[int, int]) -> tuple[int, int] | None:
    """Map axial (q,r) to (row, col) in the fixed encoding grid.

    Returns None if out of bounds.
    """
    q, r = pos
    row = r + ENCODING_RADIUS
    col = q + ENCODING_RADIUS
    if 0 <= row < GRID_SIZE and 0 <= col < GRID_SIZE:
        return (row, col)
    return None


def grid_to_axial(row: int, col: int) -> tuple[int, int]:
    """Inverse of axial_to_grid."""
    return (col - ENCODING_RADIUS, row - ENCODING_RADIUS)


def all_grid_positions() -> Iterator[tuple[int, int]]:
    """Yield all axial positions covered by the encoding grid."""
    for row in range(GRID_SIZE):
        for col in range(GRID_SIZE):
            yield grid_to_axial(row, col)
