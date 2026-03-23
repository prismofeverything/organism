"""Organism board: ring-based grid adjacency math.

Spaces are (ring_idx, step) tuples. Ring 0 = center (1 space),
ring k has k * symmetry spaces, numbered 0 .. k*symmetry-1.

This ports the relevant subset of organism.game (Clojure) board logic.
"""

from __future__ import annotations

Space = tuple[int, int]


def build_rings(symmetry: int, num_rings: int) -> list[list[Space]]:
    """Build ring lists. Index 0 = center (1 space), index k = k*symmetry spaces."""
    rings: list[list[Space]] = [[(0, 0)]]
    for level in range(1, num_rings):
        rings.append([(level, step) for step in range(level * symmetry)])
    return rings


def space_adjacencies(rings: list[list[Space]], space: Space) -> list[Space]:
    """Compute adjacent spaces for a non-center space.

    Mirrors Clojure space-adjacencies (same-ring ± 1, inner, outer).
    """
    ring_idx, step = space
    level = ring_idx

    same_count = len(rings[level])
    neighbors: list[Space] = [
        (ring_idx, (step - 1) % same_count),
        (ring_idx, (step + 1) % same_count),
    ]

    # Inner ring
    along = step % level   # position within wedge (0 = on axis)
    cycle = step // level  # which axis
    inner_count = len(rings[level - 1])
    inner_ratio = (level - 1) * cycle

    if along == 0:  # on axis → one inner neighbor
        neighbors.append((level - 1, inner_ratio % inner_count))
    else:           # off axis → two inner neighbors
        inner_space = inner_ratio + along - 1
        neighbors.append((level - 1, inner_space % inner_count))
        neighbors.append((level - 1, (inner_space + 1) % inner_count))

    # Outer ring (if not outermost)
    if level < len(rings) - 1:
        outer_count = len(rings[level + 1])
        outer_ratio = (level + 1) * cycle
        outer_along = along + outer_ratio
        if along == 0:  # on axis → three outer neighbors
            for d in (-1, 0, 1):
                neighbors.append((level + 1, (outer_ratio + d) % outer_count))
        else:           # off axis → two outer neighbors
            neighbors.append((level + 1, outer_along % outer_count))
            neighbors.append((level + 1, (outer_along + 1) % outer_count))

    return neighbors


def find_adjacencies(rings: list[list[Space]]) -> dict[Space, list[Space]]:
    """Build the full adjacency map for all spaces."""
    adjacencies: dict[Space, list[Space]] = {}

    # Center (ring 0) is adjacent to all ring-1 spaces
    center: Space = (0, 0)
    adjacencies[center] = list(rings[1]) if len(rings) > 1 else []

    # All other rings
    for level in range(1, len(rings)):
        for space in rings[level]:
            adjacencies[space] = space_adjacencies(rings, space)

    return adjacencies


def find_corners(rings: list[list[Space]], symmetry: int) -> list[Space]:
    """Corner spaces of the outermost ring (one per axis)."""
    outer = rings[-1]
    total = len(outer)
    jump = total // symmetry
    return [outer[i * jump] for i in range(symmetry)]


def remove_space(adjacencies: dict[Space, list[Space]], space: Space) -> dict[Space, list[Space]]:
    """Remove a space and all references to it from adjacencies."""
    return {
        k: [s for s in v if s != space]
        for k, v in adjacencies.items()
        if k != space
    }


def corner_notches(
    adjacencies: dict[Space, list[Space]],
    rings: list[list[Space]],
    symmetry: int,
) -> dict[Space, list[Space]]:
    """Remove corner notch spaces from the adjacency map."""
    for corner in find_corners(rings, symmetry):
        adjacencies = remove_space(adjacencies, corner)
    return adjacencies


def build_board(
    symmetry: int = 5,
    num_rings: int = 7,
    remove_notches: bool = True,
) -> tuple[list[list[Space]], dict[Space, list[Space]], Space, list[Space]]:
    """Build and return (rings, adjacencies, center, all_spaces).

    all_spaces is ordered: center first, then ring-1, ring-2, ..., ring-(num_rings-1),
    each ring's spaces in step order. Corner notches are excluded when remove_notches=True.
    """
    rings = build_rings(symmetry, num_rings)
    adjacencies = find_adjacencies(rings)
    center: Space = (0, 0)

    if remove_notches:
        adjacencies = corner_notches(adjacencies, rings, symmetry)

    # Preserve insertion order: center, then outer rings
    all_spaces = list(adjacencies.keys())
    return rings, adjacencies, center, all_spaces
