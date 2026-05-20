"""
Graph-domain isotropic remesh with a FROZEN boundary.

Make a 2D triangulation near-equilateral *in 3D* on the height-field graph
surface z = H(x,y), while leaving its boundary loop byte-for-byte unchanged.
That boundary is a symmetry wedge's two matched radial cuts (+ the outline arc),
so after replicate+weld the result is exactly N-fold.

Why this is subtle: on the steep wall->dome shoulder a 3D-equilateral triangle
projects to a *thin* 2D triangle, so 2D-Delaunay (max 2D angle) yields 3D
slivers. We therefore (a) place a FIXED number of interior points — count from
the 3D surface area for an equilateral-lattice density — and relax them with a
3D-AREA-WEIGHTED Lloyd/CVT iteration (points migrate to equalise 3D area, i.e.
get denser where the lift is steep), then (b) fix connectivity with 3D edge
flips (raise the minimum 3D angle). Fixed count + Lloyd is stable (no
split/collapse oscillation). Because the surface is a graph, projection is just
re-evaluating H(x,y): no BVH, single-valued (moldable) for free.

The boundary (first len(Vb) verts + segments) never moves, so replicate+weld is
exact -> the assembled mesh is exactly N-fold.
"""
from __future__ import annotations
from collections import defaultdict
import numpy as np
import triangle as tr
from matplotlib.path import Path as MplPath

_RNG = np.random.default_rng(0)


def _cdt(V, seg):
    out = tr.triangulate({"vertices": np.asarray(V, float), "segments": seg}, "pYY")
    return out["vertices"], out["triangles"]


def _unique_edges(T):
    E = np.sort(np.vstack([T[:, [0, 1]], T[:, [1, 2]], T[:, [2, 0]]]), axis=1)
    return np.unique(E, axis=0)


def edge_len_3d(V, E, Hfn):
    P = np.column_stack([V, Hfn(V)])
    return np.linalg.norm(P[E[:, 0]] - P[E[:, 1]], axis=1)


def tri_area_3d(V, T, Hfn):
    P = np.column_stack([V, Hfn(V)])
    return 0.5 * np.linalg.norm(np.cross(P[T[:, 1]] - P[T[:, 0]],
                                         P[T[:, 2]] - P[T[:, 0]]), axis=1)


def min_angles_3d(V, T, Hfn):
    P = np.column_stack([V, Hfn(V)])[T]
    out = []
    for i in range(3):
        a = P[:, (i + 1) % 3] - P[:, i]
        b = P[:, (i + 2) % 3] - P[:, i]
        cos = np.einsum("ij,ij->i", a, b) / (
            np.linalg.norm(a, axis=1) * np.linalg.norm(b, axis=1) + 1e-12)
        out.append(np.degrees(np.arccos(np.clip(cos, -1, 1))))
    return np.min(np.array(out), axis=0)


# ----------------------------------------------------------- 3D Delaunay flips
def _ccw(p, q, r):
    return (q[0] - p[0]) * (r[1] - p[1]) - (q[1] - p[1]) * (r[0] - p[0])


def _order_ccw(V, a, b, c):
    return [a, b, c] if _ccw(V[a], V[b], V[c]) >= 0 else [a, c, b]


def _tri_min_angle_3d(p, q, r):
    m = np.pi
    for u, v, w in ((p, q, r), (q, r, p), (r, p, q)):
        e1, e2 = v - u, w - u
        c = np.dot(e1, e2) / (np.linalg.norm(e1) * np.linalg.norm(e2) + 1e-12)
        m = min(m, np.arccos(np.clip(c, -1, 1)))
    return m


def flip_to_3d(V, T, m, Hfn, max_passes=12):
    """Lawson flips that raise the minimum 3D angle. Boundary edges (both ends
    frozen) never flip, so the cuts stay intact. Monotone -> terminates."""
    P = np.column_stack([V, Hfn(V)])
    T = [list(t) for t in T]
    for _ in range(max_passes):
        ef = defaultdict(list)
        for fi, (a, b, c) in enumerate(T):
            for x, y in ((a, b), (b, c), (c, a)):
                ef[(min(x, y), max(x, y))].append(fi)
        dirty, flipped = set(), False
        for (x, y), fs in ef.items():
            if len(fs) != 2 or (x < m and y < m):
                continue
            f0, f1 = fs
            if f0 in dirty or f1 in dirty:
                continue
            o0 = [v for v in T[f0] if v != x and v != y]
            o1 = [v for v in T[f1] if v != x and v != y]
            if len(o0) != 1 or len(o1) != 1:
                continue
            c0, d0 = o0[0], o1[0]
            if _ccw(V[x], V[y], V[c0]) * _ccw(V[x], V[y], V[d0]) >= 0:
                continue
            if _ccw(V[c0], V[d0], V[x]) * _ccw(V[c0], V[d0], V[y]) >= 0:
                continue
            before = min(_tri_min_angle_3d(P[x], P[y], P[c0]),
                         _tri_min_angle_3d(P[x], P[y], P[d0]))
            after = min(_tri_min_angle_3d(P[c0], P[d0], P[x]),
                        _tri_min_angle_3d(P[c0], P[d0], P[y]))
            if after > before + 1e-7:
                T[f0] = _order_ccw(V, c0, d0, x)
                T[f1] = _order_ccw(V, c0, d0, y)
                dirty.add(f0); dirty.add(f1); flipped = True
        if not flipped:
            break
    return np.array(T)


# ----------------------------------------------------------- CVT point relaxation
def _grad_mag(Hfn, P, eps=0.04):
    hx = (Hfn(P + [eps, 0]) - Hfn(P - [eps, 0])) / (2 * eps)
    hy = (Hfn(P + [0, eps]) - Hfn(P - [0, eps])) / (2 * eps)
    return np.hypot(hx, hy)


def _rho(Hfn, P):
    """Graph area element sqrt(1+|grad H|^2): target point density is prop. to it."""
    return np.sqrt(1.0 + _grad_mag(Hfn, np.atleast_2d(P)) ** 2)


def _sample_under_rho(path, Hfn, n, lo, hi):
    if n <= 0:
        return np.empty((0, 2))
    rmax = _rho(Hfn, _RNG.uniform(lo, hi, size=(3000, 2))).max() * 1.15
    out = []
    while len(out) < n:
        cand = _RNG.uniform(lo, hi, size=(4000, 2))
        cand = cand[path.contains_points(cand)]
        acc = cand[_RNG.uniform(0, rmax, size=len(cand)) < _rho(Hfn, cand)]
        out.extend(map(tuple, acc))
    return np.array(out[:n])


def _lloyd_step(V, T, m, path, Hfn, damp=1.0):
    """Move each interior point to the 3D-area-weighted mean of its incident
    triangles' centroids (CVT under the graph metric); boundary pinned."""
    A = tri_area_3d(V, T, Hfn)
    c2d = V[T].mean(1)
    num = np.zeros((len(V), 2)); den = np.zeros(len(V))
    for ti, (a, b, c) in enumerate(T):
        for v in (a, b, c):
            num[v] += A[ti] * c2d[ti]; den[v] += A[ti]
    Vn = V.copy()
    for i in range(m, len(V)):
        if den[i] > 0:
            cand = V[i] + damp * (num[i] / den[i] - V[i])
            if path.contains_point(cand):
                Vn[i] = cand
    return Vn


def remesh_graph(Vb, seg, Hfn, target, lloyd=28, polish=8, verbose=False):
    """Remesh the wedge interior to ~`target` 3D edge length on z=H(x,y).

    Vb,seg : ORDERED frozen boundary loop (cuts + arc + center) and segments.
    Returns (V, T): V = [Vb ; interior]; first len(Vb) verts + seg preserved.
    """
    Vb = np.asarray(Vb, float)
    m = len(Vb)
    path = MplPath(np.vstack([Vb, Vb[:1]]))
    lo, hi = Vb.min(0), Vb.max(0)

    # interior point count from the 3D surface area (equilateral-lattice density:
    # one vertex per (sqrt(3)/2) target^2 of area), minus the boundary's share.
    _, T0 = _cdt(Vb, seg)
    area3d = tri_area_3d(Vb, T0, Hfn).sum()
    n_total = area3d / ((np.sqrt(3) / 2) * target ** 2)
    n_in = max(0, int(round(n_total - 0.65 * m)))
    Vi = _sample_under_rho(path, Hfn, n_in, lo, hi)

    def assemble():
        return np.vstack([Vb, Vi]) if len(Vi) else Vb.copy()

    for it in range(lloyd):
        V = assemble()
        _, T = _cdt(V, seg)
        T = flip_to_3d(V, T, m, Hfn)
        damp = 1.0 if it < lloyd - 6 else 0.5
        V = _lloyd_step(V, T, m, path, Hfn, damp)
        Vi = V[m:]
        if verbose and it % 4 == 0:
            ang = min_angles_3d(V, T, Hfn)
            L = edge_len_3d(V, _unique_edges(T), Hfn)
            print(f"    lloyd {it:2d}: {len(V):5d}v {len(T):5d}t  min3Dang {ang.min():4.1f}"
                  f"  maxedge {L.max():4.2f}  p95/med {np.percentile(L,95)/np.median(L):4.2f}")

    for _ in range(polish):                       # settle: flips + light smoothing
        V = assemble()
        _, T = _cdt(V, seg)
        T = flip_to_3d(V, T, m, Hfn)
        V = _lloyd_step(V, T, m, path, Hfn, 0.3)
        Vi = V[m:]

    V = assemble()
    _, T = _cdt(V, seg)
    T = flip_to_3d(V, T, m, Hfn)
    return V, T
