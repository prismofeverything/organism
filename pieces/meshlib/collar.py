"""
Structured ring mesh from level sets of the height field.

The hard part of a height-field mesh is the wall->dome shoulder, where the dome
leaves the vertical wall with a near-vertical tangent (|grad H| -> inf). There a
constrained-Delaunay of the 2D domain produces slivers, because a 3D-good
triangle there projects to a degenerate 2D one. The fix is to STOP letting the
triangulator choose connectivity in that band: lay triangles along LEVEL SETS of
H instead.

Rings sit at H = wall + k*target (vertical spacing = target). Each ring is a
curve from cut A to cut B, resampled to ~target along its length, and consecutive
rings are LOFTED (zipped by the shorter diagonal). Because we control the
connectivity, a strip that is thin in the 2D domain (steep band) becomes a row of
near-equilateral triangles in 3D. Every ring crosses the two cuts at the same
radius (H is N-fold symmetric), so after replicate+weld the mesh is exactly
N-fold. Near the apex the innermost ring fans to the center (an exempt pole).

`build_wedge_rings` returns one wedge as a fully structured onion; the build can
instead use the collar only for the steep band and a CVT cap above it.
"""
from __future__ import annotations
import numpy as np

from symmetry import _radial_outline, _r_at


def _resample_open(P, target):
    """Resample an OPEN polyline to ~target spacing, keeping both endpoints."""
    seg = np.hypot(*np.diff(P, axis=0).T)
    s = np.concatenate([[0], np.cumsum(seg)])
    tot = s[-1]
    n = max(2, int(round(tot / target)))
    si = np.linspace(0, tot, n + 1)
    return np.column_stack([np.interp(si, s, P[:, 0]), np.interp(si, s, P[:, 1])])


def _ring_at_height(Hfn, th, r_hi, zt, iters=42):
    """Radius along each ray `th` (out to r_hi) where H == zt, by bisection.
    H decreases from the center (high) to the rim (= wall), so it is monotone
    along a ray for these star-convex domes."""
    lo = np.zeros_like(th); hi = r_hi.copy()
    for _ in range(iters):
        mid = 0.5 * (lo + hi)
        Hm = Hfn(np.column_stack([mid * np.cos(th), mid * np.sin(th)]))
        higher = Hm > zt
        lo = np.where(higher, mid, lo)
        hi = np.where(higher, hi, mid)
    return 0.5 * (lo + hi)


def _loft(V, A, B, Hfn):
    """Zip outer ring A and inner ring B (both ordered A_alpha..A_beta) into
    triangles, advancing whichever diagonal is shorter in 3D."""
    P = np.column_stack([V, Hfn(V)])
    i = j = 0; out = []
    while i < len(A) - 1 or j < len(B) - 1:
        if j >= len(B) - 1:
            out.append([A[i], A[i + 1], B[j]]); i += 1
        elif i >= len(A) - 1:
            out.append([A[i], B[j + 1], B[j]]); j += 1
        else:
            dA = np.linalg.norm(P[A[i + 1]] - P[B[j]])
            dB = np.linalg.norm(P[A[i]] - P[B[j + 1]])
            if dA <= dB:
                out.append([A[i], A[i + 1], B[j]]); i += 1
            else:
                out.append([A[i], B[j + 1], B[j]]); j += 1
    return out


def _fix_ccw(V, T):
    a, b, c = V[T[:, 0]], V[T[:, 1]], V[T[:, 2]]
    area2 = (b[:, 0] - a[:, 0]) * (c[:, 1] - a[:, 1]) - \
            (b[:, 1] - a[:, 1]) * (c[:, 0] - a[:, 0])
    T = T.copy()
    T[area2 < 0] = T[area2 < 0][:, [0, 2, 1]]
    return T


def _weld(V, T, tol=1e-4):
    key = np.round(V / tol).astype(np.int64)
    _, idx, inv = np.unique(key, axis=0, return_index=True, return_inverse=True)
    Tw = inv[T]
    good = (Tw[:, 0] != Tw[:, 1]) & (Tw[:, 1] != Tw[:, 2]) & (Tw[:, 0] != Tw[:, 2])
    return V[idx], Tw[good]


def build_collar_rings(region, fold, Hfn, wall_z, zmax, target, n_radial=240):
    """Level-set rings with ADAPTIVE z-steps. Each ring is a level set z=const
    (clean cap boundary, aligned columns), but the step dz is shrunk wherever a
    column is gentle so the MAX 2D radial gap stays ~target. This gives ~target 3D
    edges on steep (GROW) and shallow (EAT) domes alike, with no jagged transition.
    Stop before the apex (small rings) and let the CVT cap own it. Returns
    (rings, alpha, beta)."""
    th_s, r_s = _radial_outline(region)
    beta = 2 * np.pi / fold
    alpha = float(th_s[np.argmax(r_s)])               # cut through a LOBE (convex
    # tip): keeps the frozen cut in easy convex geometry and leaves the concave
    # notch in the wedge interior, where flips/smoothing are free.
    thf = np.linspace(alpha, alpha + beta, 1500)
    rf = _r_at(th_s, r_s, thf)
    Pf = np.column_stack([rf * np.cos(thf), rf * np.sin(thf)])
    s = np.concatenate([[0], np.cumsum(np.hypot(*np.diff(Pf, axis=0).T))])
    n_theta = max(3, int(round(s[-1] / target)) + 1)
    th = np.interp(np.linspace(0, s[-1], n_theta), s, thf)
    r_rim = _r_at(th_s, r_s, th); r_rim[-1] = r_rim[0]

    rings = [np.column_stack([r_rim * np.cos(th), r_rim * np.sin(th)])]
    prev = r_rim.copy(); k = 1
    while wall_z + k * target < zmax - 0.6 * target:
        r = _ring_at_height(Hfn, th, r_rim, wall_z + k * target)
        r[-1] = r[0]
        rings.append(np.column_stack([r * np.cos(th), r * np.sin(th)]))
        gap = float(np.median(prev - r))
        prev = r
        if gap > 0.9 * target:                         # gentle now -> stop, cap it
            break
        k += 1
    return rings, alpha, beta


def build_onion(region, fold, Hfn, target, n_radial=240):
    """One wedge as a fully structured 'onion': concentric rings from rim to apex,
    spaced by 3D ARC LENGTH along each radial profile and ALIGNED (every column
    sampled at the same normalised arc-length fractions). This keeps 3D edges ~target
    on steep AND shallow domes alike (uniform-z rings spread on shallow domes,
    uniform-radius rings stretch on steep ones — arc length is the cure), and the
    aligned columns loft into clean quads. The two cuts (first/last column) are exact
    rotations, so replicate+weld is exact. Returns (V2, T, ring0)."""
    from remesh_graph import flip_to_3d
    th_s, r_s = _radial_outline(region)
    beta = 2 * np.pi / fold
    alpha = float(th_s[np.argmax(r_s)])               # cut through a LOBE
    thf = np.linspace(alpha, alpha + beta, 1500)
    rf = _r_at(th_s, r_s, thf)
    Pf = np.column_stack([rf * np.cos(thf), rf * np.sin(thf)])
    s = np.concatenate([[0], np.cumsum(np.hypot(*np.diff(Pf, axis=0).T))])
    n_theta = max(3, int(round(s[-1] / target)) + 1)
    th = np.interp(np.linspace(0, s[-1], n_theta), s, thf)
    r_rim = _r_at(th_s, r_s, th); r_rim[-1] = r_rim[0]

    # per-column radial profile: 3D arc length s(r) from rim (r=r_rim) to apex (r=0)
    prof_s, prof_r, Lmax = [], [], 0.0
    for t, rr0 in zip(th, r_rim):
        rr = np.linspace(rr0, 0.0, n_radial)
        P = np.column_stack([rr * np.cos(t), rr * np.sin(t)])
        sc = np.concatenate([[0], np.cumsum(np.hypot(*np.diff(
            np.column_stack([P, Hfn(P)]), axis=0).T))])
        prof_s.append(sc); prof_r.append(rr); Lmax = max(Lmax, sc[-1])
    n = max(2, int(np.ceil(Lmax / target)))

    rings = []
    for j in range(n):                                # j=0 rim ... j=n-1 near apex
        f = j / n
        rj = np.array([np.interp(f * sc[-1], sc, rr) for sc, rr in zip(prof_s, prof_r)])
        rj[-1] = rj[0]
        rings.append(np.column_stack([rj * np.cos(th), rj * np.sin(th)]))

    V = []; idx = []
    for ring in rings:
        sidx = len(V); V.extend(map(tuple, ring)); idx.append(list(range(sidx, len(V))))
    center = len(V); V.append((0.0, 0.0))
    V = np.array(V, float)
    P3 = np.column_stack([V, Hfn(V)])
    d3 = lambda a, b: np.linalg.norm(P3[a] - P3[b])
    T = []
    for A, B in zip(idx[:-1], idx[1:]):               # aligned-column quads
        for i in range(n_theta - 1):
            if d3(A[i], B[i + 1]) <= d3(A[i + 1], B[i]):
                T += [[A[i], A[i + 1], B[i + 1]], [A[i], B[i + 1], B[i]]]
            else:
                T += [[A[i], A[i + 1], B[i]], [A[i + 1], B[i + 1], B[i]]]
    for i in range(n_theta - 1):                      # fan innermost ring to apex
        T.append([idx[-1][i], idx[-1][i + 1], center])

    frozen = np.zeros(len(V), bool)
    frozen[idx[0]] = True                             # rim
    for col in idx:
        frozen[col[0]] = True; frozen[col[-1]] = True  # the two cuts
    frozen[center] = True
    T = flip_to_3d(V, np.array(T, int), frozen, Hfn)   # optimise quad diagonals (no new verts)
    return V, _fix_ccw(V, T), np.asarray(rings[0], float)


def build_wedge_collar_cap(region, fold, Hfn, wall_z, zmax, target):
    """One wedge = structured collar (steep rim band) + CVT cap (gentle top incl.
    the apex), welded. Returns (V2, T, ring0). The two cuts are matched (rings end
    on them at symmetric radii; the cap cut is sampled symmetrically), so
    replicate+weld is exact."""
    from remesh_graph import remesh_graph
    rings, alpha, beta = build_collar_rings(region, fold, Hfn, wall_z, zmax, target)
    N = len(rings[0])

    Vc = []; ridx = []
    for ring in rings:
        s = len(Vc); Vc.extend(map(tuple, ring)); ridx.append(list(range(s, len(Vc))))
    Tc = []
    for A, B in zip(ridx[:-1], ridx[1:]):              # aligned-column quads
        for i in range(N - 1):
            Tc.append([A[i], A[i + 1], B[i + 1]])
            Tc.append([A[i], B[i + 1], B[i]])
    Vc = np.array(Vc, float); Tc = np.array(Tc, int)

    ringK = rings[-1]
    rK = float(np.hypot(ringK[0, 0], ringK[0, 1]))     # cut radius at ring K (=at beta)
    nrad = max(2, int(round(rK / target)))
    inner = np.linspace(rK, 0.0, nrad + 1)[1:-1]       # strictly between ring K and center
    loop = [tuple(p) for p in ringK]                                   # ring K, alpha..beta
    loop += [(r * np.cos(alpha + beta), r * np.sin(alpha + beta)) for r in inner]
    loop += [(0.0, 0.0)]                                               # center
    loop += [(r * np.cos(alpha), r * np.sin(alpha)) for r in inner[::-1]]
    Vb = np.array(loop); mloop = len(Vb)
    seg = np.array([[i, (i + 1) % mloop] for i in range(mloop)])
    Vcap, Tcap = remesh_graph(Vb, seg, Hfn, target)

    V = np.vstack([Vc, Vcap]); T = np.vstack([Tc, Tcap + len(Vc)])
    V, T = _weld(V, T, tol=4e-3)                       # merge collar/cap ring-K seam
    return V, _fix_ccw(V, T), np.asarray(rings[0], float)


def build_wedge_top(region, fold, Hfn, wall_z, zmax, target,
                    min_angle=15.0, max_edge=1.5):
    """Top-surface wedge, choosing the cheapest mesh that meets quality: try a pure
    CVT mesh first (ideal for SHALLOW domes, where the steep rim band is thin enough
    for the CVT to resolve), and fall back to the structured collar + CVT-cap when
    the CVT slivers (DEEP domes like GROW). Returns (V2, T, ring0)."""
    from remesh_graph import (remesh_graph, min_angles_3d, edge_len_3d,
                              _unique_edges)
    from symmetry import wedge_boundary_star_3d
    Vb, seg, arc = wedge_boundary_star_3d(region, fold, Hfn, target)
    V, T = remesh_graph(Vb, seg, Hfn, target)
    ang = float(min_angles_3d(V, T, Hfn).min())
    emax = float(edge_len_3d(V, _unique_edges(T), Hfn).max())
    if ang >= min_angle and emax <= max_edge:
        return V, _fix_ccw(V, np.asarray(T)), np.asarray(arc, float)
    return build_wedge_collar_cap(region, fold, Hfn, wall_z, zmax, target)


def build_bottom_wedge(fold, ring0, target):
    """Flat, ISOTROPIC wedge mesh whose rim arc is EXACTLY `ring0` (the top's rim,
    so the wall stitches cleanly) but with its own coarse cuts. The bottom is flat,
    so it must NOT inherit the top's anisotropic (collar) triangulation — that is
    what slivers a flat copy. Returns (V2, T)."""
    from remesh_graph import remesh_graph
    ring0 = np.asarray(ring0, float)
    alpha = float(np.arctan2(ring0[0, 1], ring0[0, 0]))
    beta = 2 * np.pi / fold
    rA = float(np.hypot(ring0[0, 0], ring0[0, 1]))
    nrad = max(2, int(round(rA / target)))
    inner = np.linspace(rA, 0.0, nrad + 1)[1:-1]
    loop = [tuple(p) for p in ring0]
    loop += [(r * np.cos(alpha + beta), r * np.sin(alpha + beta)) for r in inner]
    loop += [(0.0, 0.0)]
    loop += [(r * np.cos(alpha), r * np.sin(alpha)) for r in inner[::-1]]
    Vb = np.array(loop); mloop = len(Vb)
    seg = np.array([[i, (i + 1) % mloop] for i in range(mloop)])
    flat = lambda P: np.zeros(len(np.atleast_2d(P)))
    V, T = remesh_graph(Vb, seg, flat, target)
    return V, _fix_ccw(V, T)
