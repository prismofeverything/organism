"""Board topologies: ring, square grid, hex grid, torus, named graphs.

Every topology maps nodes (integer IDs) into a spatial (row, col) grid
for neural net encoding, plus an adjacency dict for game logic.
"""

from __future__ import annotations
from dataclasses import dataclass, field

@dataclass
class Topology:
    """Universal board representation."""
    name:        str
    nodes:       list[int]                    # ordered node IDs (0..N-1)
    adjacencies: dict[int, list[int]]         # node → [neighbor nodes]
    coords:      dict[int, tuple[int, int]]   # node → (row, col) for encoding
    grid_size:   int                          # H = W for the square encoding tensor
    labels:      dict[int, str] = field(default_factory=dict)  # optional display labels

    @property
    def N(self) -> int:
        return len(self.nodes)


# ── Ring board ────────────────────────────────────────────────────────────────────

def build_ring(symmetry: int, num_rings: int, remove_notches: bool = True) -> Topology:
    """Ring board matching the Organism board geometry."""
    from alphazero.games.organism.board import build_board as _build_board
    rings, adj_map, center, all_spaces = _build_board(
        symmetry=symmetry, num_rings=num_rings, remove_notches=remove_notches,
    )
    # Map (ring, step) spaces to integer IDs
    space_list = list(all_spaces)
    space_to_id = {s: i for i, s in enumerate(space_list)}
    nodes = list(range(len(space_list)))
    adjacencies = {}
    for s, neighbors in adj_map.items():
        adjacencies[space_to_id[s]] = [space_to_id[n] for n in neighbors if n in space_to_id]
    # Coords: ring index → row, step → col
    grid_size = max(num_rings, (num_rings - 1) * symmetry) if num_rings > 1 else 1
    coords = {}
    for s, idx in space_to_id.items():
        r, step = s
        coords[idx] = (r, step % grid_size)
    labels = {space_to_id[s]: f"{s[0]}:{s[1]}" for s in space_list}
    return Topology(
        name=f"ring_s{symmetry}r{num_rings}",
        nodes=nodes, adjacencies=adjacencies,
        coords=coords, grid_size=grid_size, labels=labels,
    )


# ── Square grid ───────────────────────────────────────────────────────────────────

def build_square(size: int, wrap: bool = False) -> Topology:
    """Square grid, 4-connected. Optional toroidal wrapping."""
    nodes = list(range(size * size))
    adjacencies: dict[int, list[int]] = {}
    coords: dict[int, tuple[int, int]] = {}
    for r in range(size):
        for c in range(size):
            idx = r * size + c
            coords[idx] = (r, c)
            neighbors = []
            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                nr, nc = r + dr, c + dc
                if wrap:
                    nr, nc = nr % size, nc % size
                if 0 <= nr < size and 0 <= nc < size:
                    neighbors.append(nr * size + nc)
            adjacencies[idx] = neighbors
    return Topology(
        name=f"square_{size}{'_torus' if wrap else ''}",
        nodes=nodes, adjacencies=adjacencies,
        coords=coords, grid_size=size,
    )


# ── Hex grid ──────────────────────────────────────────────────────────────────────

def build_hex(radius: int, wrap: bool = False) -> Topology:
    """Hex grid in offset coordinates, 6-connected.

    radius=3 gives a hex board with 37 cells (radius includes center).
    Uses offset-even-row layout for square embedding.
    """
    # Generate hex cells in axial coordinates, then map to offset
    cells: list[tuple[int, int]] = []  # (q, r) axial
    for q in range(-radius + 1, radius):
        for r in range(-radius + 1, radius):
            if abs(q + r) < radius:
                cells.append((q, r))
    cells.sort()

    cell_to_id = {c: i for i, c in enumerate(cells)}
    nodes = list(range(len(cells)))

    # Axial hex neighbors
    hex_dirs = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, -1), (-1, 1)]
    adjacencies: dict[int, list[int]] = {}
    for (q, r), idx in cell_to_id.items():
        neighbors = []
        for dq, dr in hex_dirs:
            nq, nr = q + dq, r + dr
            if (nq, nr) in cell_to_id:
                neighbors.append(cell_to_id[(nq, nr)])
        adjacencies[idx] = neighbors

    # Map to offset coordinates for square grid embedding
    # Offset: col = q + radius - 1, row = r + radius - 1
    grid_size = 2 * radius - 1
    coords = {}
    for (q, r), idx in cell_to_id.items():
        coords[idx] = (r + radius - 1, q + radius - 1)

    labels = {cell_to_id[c]: f"{c[0]},{c[1]}" for c in cells}
    return Topology(
        name=f"hex_r{radius}{'_torus' if wrap else ''}",
        nodes=nodes, adjacencies=adjacencies,
        coords=coords, grid_size=grid_size, labels=labels,
    )


# ── Named graphs ──────────────────────────────────────────────────────────────────

def build_petersen() -> Topology:
    """Petersen graph: 10 nodes, 3-regular, high symmetry, no short cycles."""
    import math
    # Outer pentagon 0-4, inner pentagram 5-9
    adj: dict[int, list[int]] = {i: [] for i in range(10)}
    # Outer cycle
    for i in range(5):
        adj[i].append((i + 1) % 5)
        adj[(i + 1) % 5].append(i)
    # Inner pentagram
    for i in range(5):
        adj[5 + i].append(5 + (i + 2) % 5)
        adj[5 + (i + 2) % 5].append(5 + i)
    # Spokes
    for i in range(5):
        adj[i].append(5 + i)
        adj[5 + i].append(i)
    # Deduplicate
    adj = {k: sorted(set(v)) for k, v in adj.items()}
    # Layout: outer ring + inner ring
    coords = {}
    for i in range(5):
        a = i * 2 * math.pi / 5 - math.pi / 2
        coords[i] = (int(3 + 3 * math.sin(a)), int(3 + 3 * math.cos(a)))
        coords[5 + i] = (int(3 + 1.5 * math.sin(a)), int(3 + 1.5 * math.cos(a)))
    return Topology(
        name="petersen", nodes=list(range(10)), adjacencies=adj,
        coords=coords, grid_size=7,
    )


# ── Factory ───────────────────────────────────────────────────────────────────────

TOPOLOGIES = ["ring", "square", "hex", "torus_square", "torus_hex", "petersen"]
BOARD_SIZES = [3, 4, 5, 6, 7, 8]
SYMMETRIES = [3, 4, 5, 6]

def build_topology(topo_type: str, size: int = 4, symmetry: int = 4) -> Topology:
    if topo_type == "ring":
        return build_ring(symmetry, size)
    elif topo_type == "square":
        return build_square(size)
    elif topo_type == "hex":
        return build_hex(size)
    elif topo_type == "torus_square":
        return build_square(size, wrap=True)
    elif topo_type == "torus_hex":
        return build_hex(size, wrap=True)
    elif topo_type == "petersen":
        return build_petersen()
    else:
        raise ValueError(f"Unknown topology: {topo_type}")
