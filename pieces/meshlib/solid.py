"""
Stage 5 — solid: close the lifted top surface into a watertight manifold.

  top surface : graph z = H(x,y) over R  (triangles T)
  side wall   : vertical curtain along the boundary loop(s), z = H|bdry down to 0,
                subdivided so vertical edges stay ~target edge length
  bottom      : flat copy of R at z = 0 (reversed winding)

When the boundary height is ~0 (e.g. a pure parabola with no wall) the top and
bottom rims coincide and merge, giving a clean rim with no wall.
"""
from __future__ import annotations
from collections import defaultdict
import numpy as np


def boundary_loops(T):
    """Ordered vertex-index loops of the mesh boundary (directed by triangle
    winding: outer CCW, holes CW)."""
    count = defaultdict(int)
    for a, b, c in T:
        for i, j in ((a, b), (b, c), (c, a)):
            count[(min(i, j), max(i, j))] += 1
    nxt = {}
    for a, b, c in T:
        for i, j in ((a, b), (b, c), (c, a)):
            if count[(min(i, j), max(i, j))] == 1:
                nxt[i] = j
    loops, seen = [], set()
    for start in list(nxt):
        if start in seen:
            continue
        loop, cur = [], start
        while cur in nxt and cur not in seen:
            loop.append(cur); seen.add(cur); cur = nxt[cur]
        if len(loop) >= 3:
            loops.append(loop)
    return loops


def build_solid(V2, T, H, edge=1.0):
    """Return (vertices Nx3, faces Mx3) of the closed manifold."""
    n = len(V2)
    V = [(*V2[i], H[i]) for i in range(n)]          # 0..n-1  top
    V += [(*V2[i], 0.0) for i in range(n)]          # n..2n-1 bottom
    F = [[a, b, c] for a, b, c in T]                # top
    F += [[a + n, c + n, b + n] for a, b, c in T]   # bottom (reversed)

    for loop in boundary_loops(T):
        m = len(loop)
        wall_z = float(np.median([H[i] for i in loop]))
        nz = max(1, int(round(wall_z / edge))) if wall_z > 1e-6 else 0
        if nz == 0:
            continue                                # rim merges, no wall
        # rings top(z=wall)=loop, intermediate (new), bottom(z=0)=loop+n
        rings = [list(loop)]
        for k in range(1, nz):
            z = wall_z * (1 - k / nz)
            ring = []
            for vi in loop:
                ring.append(len(V)); V.append((*V2[vi], z))
            rings.append(ring)
        rings.append([vi + n for vi in loop])
        for r in range(len(rings) - 1):
            A, B = rings[r], rings[r + 1]
            for k in range(m):
                i0, i1 = A[k], A[(k + 1) % m]
                j0, j1 = B[k], B[(k + 1) % m]
                F.append([i0, i1, j1])
                F.append([i0, j1, j0])
    return np.array(V, float), np.array(F, int)
