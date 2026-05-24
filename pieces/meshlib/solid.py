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


def build_solid_split(Vt, Tt, Ht, Vb, Tb, edge=1.0):
    """Close a solid whose TOP and BOTTOM are INDEPENDENT 2D meshes that share the
    same rim-loop positions. The top is lifted (z=Ht), the bottom is flat (z=0),
    and a vertical wall stitches the two rims. Use this when the top needs an
    anisotropic (collar) triangulation to lift cleanly while the flat bottom needs
    an isotropic one."""
    from scipy.spatial import cKDTree
    Vt = np.asarray(Vt, float); Vb = np.asarray(Vb, float)
    nt = len(Vt)
    V = [(Vt[i, 0], Vt[i, 1], Ht[i]) for i in range(nt)]
    F = [[a, b, c] for a, b, c in Tt]
    off = len(V)
    V += [(Vb[i, 0], Vb[i, 1], 0.0) for i in range(len(Vb))]
    F += [[a + off, c + off, b + off] for a, b, c in Tb]      # bottom reversed

    lt = boundary_loops(Tt)[0]
    lb = boundary_loops(Tb)[0]
    order = cKDTree(Vb[lb]).query(Vt[lt])[1]                  # align bottom rim to top rim
    lb_al = [lb[o] for o in order]
    m = len(lt)
    wall_z = float(np.median([Ht[i] for i in lt]))
    nz = max(1, int(round(wall_z / edge)))
    rings = [[lt[k] for k in range(m)]]
    for s in range(1, nz):
        z = wall_z * (1 - s / nz); ring = []
        for k in range(m):
            ring.append(len(V)); V.append((Vt[lt[k], 0], Vt[lt[k], 1], z))
        rings.append(ring)
    rings.append([lb_al[k] + off for k in range(m)])
    for r in range(len(rings) - 1):
        A, B = rings[r], rings[r + 1]
        for k in range(m):
            i0, i1 = A[k], A[(k + 1) % m]
            j0, j1 = B[k], B[(k + 1) % m]
            if j0 == j1:                              # rims differ in count here:
                F.append([i0, i1, j0])               # collapse the quad to a tri
            elif i0 == i1:
                F.append([i0, j1, j0])
            else:
                F.append([i0, i1, j1]); F.append([i0, j1, j0])
    return np.array(V, float), np.array(F, int)


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
