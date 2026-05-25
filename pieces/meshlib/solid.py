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


def _stitch_rings(V, F, A, B, m):
    """Loft two equal-length rings, choosing the SHORTER 3D diagonal per quad (so a
    quad where the radius jumps between adjacent columns -- e.g. a star tip at the
    bottom fillet -- doesn't get a thin triangle from a fixed diagonal)."""
    for k in range(m):
        i0, i1 = A[k], A[(k + 1) % m]
        j0, j1 = B[k], B[(k + 1) % m]
        if j0 == j1:                                 # rings differ in count -> collapse
            F.append([i0, i1, j0]); continue
        if i0 == i1:
            F.append([i0, j1, j0]); continue
        a, b, c, d = V[i0], V[i1], V[j0], V[j1]
        d1 = (a[0]-d[0])**2 + (a[1]-d[1])**2 + (a[2]-d[2])**2     # diag i0-j1
        d2 = (b[0]-c[0])**2 + (b[1]-c[1])**2 + (b[2]-c[2])**2     # diag i1-j0
        if d1 <= d2:
            F.append([i0, i1, j1]); F.append([i0, j1, j0])
        else:
            F.append([i0, i1, j0]); F.append([i1, j1, j0])


def _relax_base(V, Tb, off, rim_set, iters=20):
    """Laplacian-relax the flat base interior (z=0) in (x,y), pinning the inset rim,
    so the radial rim inset spreads over the base instead of slivering one ring.
    The base mesh is symmetric and the smoothing is per-vertex, so it stays symmetric."""
    from collections import defaultdict
    nb = defaultdict(set)
    for a, b, c in Tb:
        for i, j in ((a, b), (b, c), (c, a)):
            nb[off + i].add(off + j); nb[off + j].add(off + i)
    interior = [v for v in nb if v not in rim_set]
    for _ in range(iters):
        upd = {}
        for v in interior:
            P = [V[j] for j in nb[v]]
            upd[v] = (sum(p[0] for p in P) / len(P), sum(p[1] for p in P) / len(P), 0.0)
        for k, p in upd.items():
            V[k] = p


def inset_ring(P, d):
    """Radially shrink a ring toward the origin by `d` mm (each point's radius
    -> radius - d). Used to inset the flat base under a rounded bottom rim."""
    P = np.asarray(P, float)
    r = np.hypot(P[:, 0], P[:, 1])
    return P * np.clip(1.0 - d / np.maximum(r, 1e-9), 0.0, 1.0)[:, None]


def offset_loop_inward(P, d):
    """Inset a CCW loop by `d` mm along its inward MITER normal (perpendicular to
    the local edges), not radially — so concave valleys inset by the same `d` as
    convex tips instead of crowding toward the centre (which slivered the base)."""
    P = np.asarray(P, float)
    e = np.roll(P, -1, axis=0) - P
    e = e / np.maximum(np.hypot(e[:, 0], e[:, 1]), 1e-9)[:, None]
    inw = np.stack([-e[:, 1], e[:, 0]], axis=1)          # left normal = interior (CCW)
    vn = inw + np.roll(inw, 1, axis=0)                   # miter = mean of adjacent edge normals
    vn = vn / np.maximum(np.hypot(vn[:, 0], vn[:, 1]), 1e-9)[:, None]
    return P + vn * d


def build_solid_split(Vt, Tt, Ht, Vb, Tb, edge=1.0, fillet_r=0.0):
    """Close a solid whose TOP and BOTTOM are INDEPENDENT 2D meshes that share the
    same rim-loop positions. The top is lifted (z=Ht), the bottom is flat (z=0),
    and a wall stitches the two rims. Use this when the top needs an anisotropic
    (collar) triangulation to lift cleanly while the flat bottom needs an isotropic
    one.

    fillet_r > 0 rounds the bottom rim: the wall runs vertical down to z=fillet_r,
    then a quarter-round curls inward to the (radially inset) flat base at z=0 — a
    G1, sculptable bottom edge instead of a hard 90deg crease. The base mesh `Vb`
    must already be inset by fillet_r so its rim sits under the curl."""
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
    m = len(lt)
    if fillet_r > 0 and len(lb) == m:
        # base is already inset (clean remesh, no crowding); match the two equal loops
        # by the best cyclic shift/flip -> a bijection (watertight), no nearest-collision.
        at = np.arctan2(Vt[lt][:, 1], Vt[lt][:, 0])
        ab = np.arctan2(Vb[lb][:, 1], Vb[lb][:, 0])
        def _cost(s, rev):
            o = ab[::-1] if rev else ab
            return float(np.abs((at - np.roll(o, -s) + np.pi) % (2 * np.pi) - np.pi).sum())
        _, s, rev = min((_cost(s, r), s, r) for r in (False, True) for s in range(m))
        lbb = lb[::-1] if rev else lb
        lb_al = [lbb[(k + s) % m] for k in range(m)]
    else:                                                    # rims coincide -> nearest
        order = cKDTree(Vb[lb]).query(Vt[lt])[1]
        lb_al = [lb[o] for o in order]
    wall_z = float(np.median([Ht[i] for i in lt]))
    rf = float(min(fillet_r, wall_z))
    Rt = Vt[lt]                                             # full top rim (m,2), ordered loop
    Rb = Vb[lb_al]                                          # inset base rim (m,2), already inset
    rings = [[lt[k] for k in range(m)]]                      # top rim, z=wall_z
    nz_wall = max(1, int(round((wall_z - rf) / edge)))       # vertical: wall_z -> rf
    for s in range(1, nz_wall + 1):
        z = wall_z - (wall_z - rf) * s / nz_wall; ring = []
        for k in range(m):
            ring.append(len(V)); V.append((Rt[k, 0], Rt[k, 1], z))
        rings.append(ring)
    if rf > 1e-6:                                            # quarter-round: rf -> 0
        nz_fil = max(1, int(round(rf * (np.pi / 2) / edge)))
        for s in range(1, nz_fil):
            t = s / nz_fil; z = rf * (1 - np.sin(t * np.pi / 2)); w = np.cos(t * np.pi / 2)
            ring = []
            for k in range(m):
                x = Rb[k, 0] + (Rt[k, 0] - Rb[k, 0]) * w
                y = Rb[k, 1] + (Rt[k, 1] - Rb[k, 1]) * w
                ring.append(len(V)); V.append((x, y, z))
            rings.append(ring)
    rings.append([lb_al[k] + off for k in range(m)])         # base rim, z=0 (inset)
    for r in range(len(rings) - 1):
        _stitch_rings(V, F, rings[r], rings[r + 1], m)
    return np.array(V, float), np.array(F, int)


def build_solid(V2, T, H, edge=1.0, fillet_r=0.0):
    """Return (vertices Nx3, faces Mx3) of the closed manifold. fillet_r > 0 rounds
    the bottom rim (vertical wall -> quarter-round -> radially inset flat base), same
    as build_solid_split; the boundary height must be >= fillet_r for the curl to fit
    (give the piece a small wall_frac). A following isotropic remesh evens the base."""
    n = len(V2)
    V = [(*V2[i], H[i]) for i in range(n)]          # 0..n-1  top
    V += [(*V2[i], 0.0) for i in range(n)]          # n..2n-1 bottom
    F = [[a, b, c] for a, b, c in T]                # top
    F += [[a + n, c + n, b + n] for a, b, c in T]   # bottom (reversed)

    for loop in boundary_loops(T):
        m = len(loop)
        wall_z = float(np.median([H[i] for i in loop]))
        if wall_z <= 1e-6:
            continue                                # rim merges, no wall (no room to fillet)
        rf = float(min(fillet_r, wall_z))
        Rt = V2[loop]                               # full top rim (m,2), ordered loop
        if rf > 1e-6:                               # inset bottom rim (radial) + relax base
            rim_set = {loop[k] + n for k in range(m)}
            for k in range(m):
                vi = loop[k] + n; x, y = V2[loop[k]]; r = np.hypot(x, y)
                f = max(0.0, 1.0 - rf / max(r, 1e-9)); V[vi] = (x * f, y * f, 0.0)
            _relax_base(V, T, n, rim_set)
        Rb = np.array([V[loop[k] + n][:2] for k in range(m)])
        rings = [list(loop)]                        # top rim, z=wall_z
        nz_wall = max(1, int(round((wall_z - rf) / edge)))
        for s in range(1, nz_wall + 1):
            z = wall_z - (wall_z - rf) * s / nz_wall; ring = []
            for k in range(m):
                ring.append(len(V)); V.append((Rt[k, 0], Rt[k, 1], z))
            rings.append(ring)
        if rf > 1e-6:                               # quarter-round: rf -> 0
            nz_fil = max(1, int(round(rf * (np.pi / 2) / edge)))
            for s in range(1, nz_fil):
                t = s / nz_fil; z = rf * (1 - np.sin(t * np.pi / 2)); w = np.cos(t * np.pi / 2)
                ring = []
                for k in range(m):
                    x = Rb[k, 0] + (Rt[k, 0] - Rb[k, 0]) * w
                    y = Rb[k, 1] + (Rt[k, 1] - Rb[k, 1]) * w
                    ring.append(len(V)); V.append((x, y, z))
                rings.append(ring)
        rings.append([vi + n for vi in loop])       # base rim, z=0 (inset)
        for r in range(len(rings) - 1):
            _stitch_rings(V, F, rings[r], rings[r + 1], m)
    return np.array(V, float), np.array(F, int)
