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


def _frozen_mask(n, m_or_mask):
    """Accept either an int m (first-m-are-frozen) or a boolean mask."""
    if np.isscalar(m_or_mask):
        f = np.zeros(n, bool); f[:int(m_or_mask)] = True; return f
    return np.asarray(m_or_mask, bool)


def flip_to_3d(V, T, m, Hfn, max_passes=12):
    """Lawson flips that raise the minimum 3D angle. Frozen-frozen edges (cuts /
    rim) never flip, so they stay intact. Monotone -> terminates. `m` is an int
    (first m frozen) or a boolean frozen-vertex mask."""
    frozen = _frozen_mask(len(V), m)
    P = np.column_stack([V, Hfn(V)])
    T = [list(t) for t in T]
    for _ in range(max_passes):
        ef = defaultdict(list)
        for fi, (a, b, c) in enumerate(T):
            for x, y in ((a, b), (b, c), (c, a)):
                ef[(min(x, y), max(x, y))].append(fi)
        dirty, flipped = set(), False
        for (x, y), fs in ef.items():
            if len(fs) != 2 or (frozen[x] and frozen[y]):
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


def _lloyd_step(V, T, m, path, Hfn, damp=0.6):
    """Move each interior point toward the 3D-area-weighted mean of its incident
    triangles' centroids (CVT under the graph metric); boundary pinned, points
    kept inside the wedge. Vectorised."""
    A = tri_area_3d(V, T, Hfn)
    c2d = V[T].mean(1)
    num = np.zeros((len(V), 2)); den = np.zeros(len(V))
    for k in range(3):
        np.add.at(num, T[:, k], A[:, None] * c2d)
        np.add.at(den, T[:, k], A)
    tgt = num / np.maximum(den, 1e-12)[:, None]
    out = V.copy()
    cand = V[m:] + damp * (tgt[m:] - V[m:])
    inside = path.contains_points(cand)
    out[m:][inside] = cand[inside]
    return out


def _cleanup_near(Vb, Vi, target, frac):
    """Drop interior points closer than frac*target to a boundary point or to a
    nearer-indexed interior point — removes the slivers a free CVT leaves hard
    against the frozen boundary."""
    from scipy.spatial import cKDTree
    if not len(Vi):
        return Vi
    Vi = Vi[cKDTree(Vb).query(Vi)[0] > frac * target]
    if len(Vi) > 1:                                # thin out interior near-dupes
        keep = np.ones(len(Vi), bool)
        tree = cKDTree(Vi)
        for i, j in tree.query_pairs(frac * target):
            if keep[i] and keep[j]:
                keep[j] = False
        Vi = Vi[keep]
    return Vi


def worst_tris(V, T, m, Hfn, n=8):
    """Diagnostic: the n worst triangles by min 3D angle, with where they sit."""
    ang = min_angles_3d(V, T, Hfn)
    P = np.column_stack([V, Hfn(V)])
    order = np.argsort(ang)[:n]
    rows = []
    for ti in order:
        tri = T[ti]; cen = P[tri].mean(0)
        onb = int((tri < m).sum())
        ctr = int((tri == 0).any())               # touches the center vertex (idx 0)
        rows.append(f"      ang {ang[ti]:5.1f}  r={np.hypot(cen[0],cen[1]):5.1f} "
                    f"z={cen[2]:5.1f}  bnd_verts={onb}/3  center={ctr}")
    return "\n".join(rows)


def remesh_graph(Vb, seg, Hfn, target, lloyd=18, polish=8, flip_every=6, verbose=False):
    """Remesh the wedge interior to ~`target` 3D edge length on z=H(x,y).

    Vb,seg : ORDERED frozen boundary loop (cuts + arc + center) and segments.
    Returns (V, T): V = [Vb ; interior]; first len(Vb) verts + seg preserved.
    """
    Vb = np.asarray(Vb, float)
    m = len(Vb)
    path = MplPath(np.vstack([Vb, Vb[:1]]))
    lo, hi = Vb.min(0), Vb.max(0)

    # interior count from the 3D surface area (one vertex per (sqrt(3)/2)*t^2)
    _, T0 = _cdt(Vb, seg)
    area3d = tri_area_3d(Vb, T0, Hfn).sum()
    n_in = max(0, int(round(area3d / ((np.sqrt(3) / 2) * target ** 2) - 0.5 * m)))
    Vi = _sample_under_rho(path, Hfn, n_in, lo, hi)

    def step(damp, do_flip):
        nonlocal Vi
        V = np.vstack([Vb, Vi]) if len(Vi) else Vb.copy()
        _, T = _cdt(V, seg)
        if do_flip:
            T = flip_to_3d(V, T, m, Hfn)
        V = _lloyd_step(V, T, m, path, Hfn, damp)
        Vi = V[m:]
        return V, T

    for it in range(lloyd):                        # distribute (cheap: flip rarely)
        V, T = step(0.6, do_flip=(it % flip_every == flip_every - 1))
        if verbose and it % 4 == 0:
            ang = min_angles_3d(V, T, Hfn)
            L = edge_len_3d(V, _unique_edges(T), Hfn)
            print(f"    lloyd {it:2d}: {len(V):5d}v  min3Dang {ang.min():4.1f}"
                  f"  maxedge {L.max():4.2f}  p95/med {np.percentile(L,95)/np.median(L):4.2f}")

    Vi = _cleanup_near(Vb, Vi, target, 0.35)       # thin only true near-dupes
    for _ in range(polish):                        # settle connectivity + spacing
        step(0.3, do_flip=True)
    Vi = _cleanup_near(Vb, Vi, target, 0.3)        # drop interior that polish
    # nudged hard against the frozen boundary (would sliver after weld)

    V = np.vstack([Vb, Vi]) if len(Vi) else Vb.copy()
    _, T = _cdt(V, seg)
    T = flip_to_3d(V, T, m, Hfn)
    return V, T


# ------------------------------------------------ explicit cleanup (no re-CDT)
def _split_tri(tri, x, y, mi):
    """Split triangle `tri` (CCW) across edge (x,y) at new vertex mi -> two tris,
    preserving winding."""
    for i in range(3):
        u, v, w = tri[i], tri[(i + 1) % 3], tri[(i + 2) % 3]
        if {u, v} == {x, y}:
            return [[u, mi, w], [mi, v, w]]
    return None


def split_long_edges(V, T, frozen, Hfn, thresh):
    """Explicit 1->2 split of every long, non-frozen edge at its midpoint (no
    re-triangulation, so a structured collar survives). Returns (V, T, frozen)."""
    V = V.tolist(); T = [list(t) for t in T]
    P3 = np.column_stack([np.array(V), Hfn(np.array(V))])
    ef = defaultdict(list)
    for ti, (a, b, c) in enumerate(T):
        for x, y in ((a, b), (b, c), (c, a)):
            ef[(min(x, y), max(x, y))].append(ti)
    cand = []
    for (x, y), ts in ef.items():
        if (frozen[x] and frozen[y]) or len(ts) != 2:
            continue
        L = np.linalg.norm(P3[x] - P3[y])
        if L > thresh:
            cand.append((L, x, y, ts[0], ts[1]))
    cand.sort(reverse=True)
    dead = set(); add = []; frozen = list(frozen)
    for L, x, y, t0, t1 in cand:
        if t0 in dead or t1 in dead:
            continue
        mi = len(V)
        V.append([0.5 * (V[x][0] + V[y][0]), 0.5 * (V[x][1] + V[y][1])])
        frozen.append(False)
        s0 = _split_tri(T[t0], x, y, mi); s1 = _split_tri(T[t1], x, y, mi)
        if s0 is None or s1 is None:
            V.pop(); frozen.pop(); continue
        dead.add(t0); dead.add(t1); add += s0 + s1
    T = [t for i, t in enumerate(T) if i not in dead] + add
    return np.array(V, float), np.array(T, int), np.array(frozen, bool)


def _smooth_masked(V, T, frozen, path, Hfn, step=0.4):
    P = np.column_stack([V, Hfn(V)])
    nbr = defaultdict(list)
    for a, b, c in T:
        nbr[a] += [b, c]; nbr[b] += [a, c]; nbr[c] += [a, b]
    Vn = V.copy()
    for i in range(len(V)):
        if frozen[i] or not nbr[i]:
            continue
        nb = list(set(nbr[i]))
        w = np.linalg.norm(P[nb] - P[i], axis=1) + 1e-9
        cand = V[i] + step * ((V[nb] * w[:, None]).sum(0) / w.sum() - V[i])
        if path.contains_point(cand):
            Vn[i] = cand
    return Vn


def cleanup_wedge(V, T, frozen, Hfn, max_edge, passes=6):
    """Bounded long-edge split + 3D flip + smoothing on an assembled wedge
    (collar + cap), with rim/cuts frozen. Caps the max 3D edge that the CVT cap
    and the concave-notch collar strips leave behind, without re-triangulating
    (so the structured collar is preserved). Returns (V, T, frozen)."""
    from solid import boundary_loops
    loop = boundary_loops([list(t) for t in T])[0]
    path = MplPath(np.vstack([V[loop], V[loop][:1]]))
    frozen = np.asarray(frozen, bool)
    for _ in range(passes):
        V, T, frozen = split_long_edges(V, T, frozen, Hfn, max_edge)
        T = flip_to_3d(V, T, frozen, Hfn)
        V = _smooth_masked(V, T, frozen, path, Hfn)
    T = flip_to_3d(V, T, frozen, Hfn)
    return V, T, frozen
