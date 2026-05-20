"""
Rotational symmetry (task #2). Step 1: make a silhouette EXACTLY N-fold.

The hand-drawn SVGs are only approximately symmetric. We rasterize the region,
majority-vote over the N rotations about the centroid (robust for non-star shapes
like the spiral), and re-extract the outline. The result is exactly invariant
under rotation by 2*pi/N, which is the prerequisite for wedge-replication meshing.
"""
from __future__ import annotations
import numpy as np
import triangle as tr
from scipy.ndimage import rotate as nd_rotate
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.path import Path as MplPath

from domain import Region2D, _shoelace
from mesh2d import base_area, tri_gradient_mag

FOLD = {"EAT": 5, "MOVE": 3, "GROW": 4}


def _rasterize(region, gx, gy):
    GX, GY = np.meshgrid(gx, gy)
    pts = np.c_[GX.ravel(), GY.ravel()]
    mask = MplPath(region.outer).contains_points(pts).reshape(len(gy), len(gx))
    for h in region.holes:
        mask &= ~MplPath(h).contains_points(pts).reshape(len(gy), len(gx))
    return mask


def _resample(poly, edge=0.5):
    loop = np.vstack([poly, poly[:1]])
    seg = np.hypot(*np.diff(loop, axis=0).T)
    s = np.concatenate([[0], np.cumsum(seg)]); tot = s[-1]
    n = max(16, int(tot / edge))
    si = np.linspace(0, tot, n, endpoint=False)
    return np.column_stack([np.interp(si, s, loop[:, 0]), np.interp(si, s, loop[:, 1])])


def symmetrize_region(region, fold, res=600) -> Region2D:
    """Return a copy of `region` whose outline is exactly `fold`-fold symmetric."""
    R = float(np.max(np.abs(region.outer))) * 1.15
    gx = np.linspace(-R, R, res); gy = np.linspace(-R, R, res)
    mask = _rasterize(region, gx, gy).astype(float)

    acc = np.zeros_like(mask)
    for k in range(fold):
        acc += nd_rotate(mask, k * 360.0 / fold, reshape=False, order=0, mode="constant")
    sym = (acc >= fold / 2.0).astype(float)

    fig = plt.figure()
    cs = plt.contour(gx, gy, sym, levels=[0.5])
    plt.close(fig)
    segs = [s for s in cs.allsegs[0] if len(s) >= 8]
    segs.sort(key=lambda s: -abs(_shoelace(s)))
    outer = segs[0]
    if _shoelace(outer) < 0:
        outer = outer[::-1]
    holes = [(h[::-1] if _shoelace(h) > 0 else h) for h in segs[1:]]
    return Region2D(region.name, _resample(outer), [_resample(h) for h in holes])


def _radial_outline(region):
    p = region.outer
    th = np.arctan2(p[:, 1], p[:, 0]); r = np.hypot(p[:, 0], p[:, 1])
    o = np.argsort(th)
    return th[o], r[o]


def _r_at(th_s, r_s, theta):
    return np.interp((theta + np.pi) % (2 * np.pi) - np.pi, th_s, r_s, period=2 * np.pi)


def wedge_mesh_star(region, fold, edge=1.0, min_angle=28.0):
    """Mesh one 1/fold pie-slice of a STAR-shaped region with MATCHED radial cuts
    (cut at alpha and alpha+beta use identical radii, so rotate-copy welds).
    Returns (V Nx2, T, is_outline bool N)."""
    th_s, r_s = _radial_outline(region)
    beta = 2 * np.pi / fold
    alpha = float(th_s[np.argmin(r_s)])                 # cut through a valley
    rmax = float(_r_at(th_s, r_s, alpha))
    ncut = max(2, int(round(rmax / edge)))
    rad = np.linspace(0.0, rmax, ncut + 1)
    cutA = np.column_stack([rad * np.cos(alpha), rad * np.sin(alpha)])
    cutB = np.column_stack([rad * np.cos(alpha + beta), rad * np.sin(alpha + beta)])
    narc = max(2, int(round(rmax * beta / edge)))
    tt = np.linspace(alpha, alpha + beta, narc + 1)
    rr = _r_at(th_s, r_s, tt)
    arc = np.column_stack([rr * np.cos(tt), rr * np.sin(tt)])
    loop, flag = [(0.0, 0.0)], [False]                  # center
    for p in cutA[1:-1]: loop.append(tuple(p)); flag.append(False)
    loop.append(tuple(cutA[-1])); flag.append(True)     # P_alpha (outline)
    for p in arc[1:-1]: loop.append(tuple(p)); flag.append(True)
    loop.append(tuple(arc[-1])); flag.append(True)      # P_beta (outline)
    for p in cutB[1:-1][::-1]: loop.append(tuple(p)); flag.append(False)
    V = np.array(loop); m = len(V)
    seg = np.array([[i, (i + 1) % m] for i in range(m)])
    out = tr.triangulate({"vertices": V, "segments": seg},
                         f"pq{min_angle}a{base_area(edge):.6f}Y")
    VV, T = out["vertices"], out["triangles"]
    fixed = np.arange(len(VV)) < m
    VV, T = _improve_wedge(VV, T, fixed, seg)            # Lloyd quality, cuts fixed
    is_outline = np.zeros(len(VV), bool)
    is_outline[:m] = flag                               # input verts kept first
    return VV, T, is_outline


def _improve_wedge(V, T, fixed, seg, iters=12):
    """Lloyd-style smoothing: move interior (non-fixed) vertices toward their
    neighbour centroid and re-triangulate (constrained Delaunay), keeping all
    boundary vertices (cuts/outline/center) fixed so copies still weld."""
    from collections import defaultdict
    V = V.astype(float).copy()
    for _ in range(iters):
        nbr = defaultdict(set)
        for a, b, c in T:
            nbr[a] |= {b, c}; nbr[b] |= {a, c}; nbr[c] |= {a, b}
        Vn = V.copy()
        for i in range(len(V)):
            if not fixed[i] and nbr[i]:
                Vn[i] = V[list(nbr[i])].mean(0)
        out = tr.triangulate({"vertices": Vn, "segments": seg}, "pQ")
        V, T = out["vertices"], out["triangles"]
    return V, T


def replicate2d(V, T, fold, weld=1e-3):
    """Rotate-copy a wedge `fold` times about the origin and weld coincident
    (cut + center) vertices. Returns (V, T, inv) where inv maps the stacked
    per-copy vertices to welded indices."""
    n = len(V); Vs, Ts = [], []
    for k in range(fold):
        a = k * 2 * np.pi / fold; c, s = np.cos(a), np.sin(a)
        Vs.append(V @ np.array([[c, s], [-s, c]]))      # rotate by +a
        Ts.append(T + k * n)
    Vall = np.vstack(Vs); Tall = np.vstack(Ts)
    _, idx, inv = np.unique(np.round(Vall / weld).astype(np.int64), axis=0,
                            return_index=True, return_inverse=True)
    Vw = Vall[idx]; Tw = inv[Tall]
    good = (Tw[:, 0] != Tw[:, 1]) & (Tw[:, 1] != Tw[:, 2]) & (Tw[:, 0] != Tw[:, 2])
    return Vw, Tw[good], inv


def _wedge_loop(region, fold, edge_b):
    """Boundary loop of one pie-slice with FINE matched cuts + fine outline.
    Returns (V, seg, flag, m, alpha, beta, rmax)."""
    th_s, r_s = _radial_outline(region)
    beta = 2 * np.pi / fold
    alpha = float(th_s[np.argmin(r_s)])
    rmax = float(_r_at(th_s, r_s, alpha))
    ncut = max(2, int(round(rmax / edge_b)))
    rad = np.linspace(0.0, rmax, ncut + 1)
    cutA = np.column_stack([rad * np.cos(alpha), rad * np.sin(alpha)])
    cutB = np.column_stack([rad * np.cos(alpha + beta), rad * np.sin(alpha + beta)])
    narc = max(2, int(round(rmax * beta / edge_b)))
    tt = np.linspace(alpha, alpha + beta, narc + 1)
    rr = _r_at(th_s, r_s, tt)
    arc = np.column_stack([rr * np.cos(tt), rr * np.sin(tt)])
    loop, flag = [(0.0, 0.0)], [False]
    for p in cutA[1:-1]: loop.append(tuple(p)); flag.append(False)
    loop.append(tuple(cutA[-1])); flag.append(True)
    for p in arc[1:-1]: loop.append(tuple(p)); flag.append(True)
    loop.append(tuple(arc[-1])); flag.append(True)
    for p in cutB[1:-1][::-1]: loop.append(tuple(p)); flag.append(False)
    V = np.array(loop); m = len(V)
    seg = np.array([[i, (i + 1) % m] for i in range(m)])
    return V, seg, flag, m


def _cap_2d_spacing(r, min2d):
    """Merge samples closer than `min2d` in radius (keep first + last). A radial
    cut sampled by 3D arc length piles up at a vertical rim (2D spacing -> 0);
    that pile-up makes the constrained Delaunay produce slivers among the frozen
    cut vertices, so cap how fine the 2D spacing may get."""
    keep = [r[0]]
    for x in r[1:-1]:
        if x - keep[-1] >= min2d:
            keep.append(x)
    keep.append(r[-1])
    return np.array(keep)


def _arc_sample_radial(alpha, rmax, Hfn, target, n_fine=400, min2d_frac=0.45):
    """Radii along a radial cut, sampled by 3D ARC LENGTH on the curve
    (r cos a, r sin a, H) — finer where the lift is steep, but never finer than
    min2d_frac*target in radius (avoids the vertical-rim pile-up). Returns
    r-samples incl. 0 and rmax; both cuts use these radii, so they match."""
    r = np.linspace(0.0, rmax, n_fine)
    P = np.column_stack([r * np.cos(alpha), r * np.sin(alpha)])
    s = np.concatenate([[0], np.cumsum(np.linalg.norm(
        np.diff(np.column_stack([P, Hfn(P)]), axis=0), axis=1))])
    n = max(2, int(round(s[-1] / target)))
    ri = np.interp(np.linspace(0, s[-1], n + 1), s, r)
    return _cap_2d_spacing(ri, min2d_frac * target)


def _arc_sample_outline(th_s, r_s, a0, a1, Hfn, target, n_fine=600):
    """Outline points P(a0)..P(a1) sampled by 3D arc length on the lifted rim."""
    tt = np.linspace(a0, a1, n_fine)
    rr = _r_at(th_s, r_s, tt)
    P = np.column_stack([rr * np.cos(tt), rr * np.sin(tt)])
    s = np.concatenate([[0], np.cumsum(np.linalg.norm(
        np.diff(np.column_stack([P, Hfn(P)]), axis=0), axis=1))])
    n = max(2, int(round(s[-1] / target)))
    ti = np.interp(np.linspace(0, s[-1], n + 1), s, tt)
    ri = _r_at(th_s, r_s, ti)
    return np.column_stack([ri * np.cos(ti), ri * np.sin(ti)])


def wedge_boundary_star_3d(region, fold, Hfn, target):
    """Ordered, frozen wedge boundary loop (center -> cutA -> arc -> cutB) for a
    STAR region, with cuts + arc sampled by 3D arc length on z=H. Returns
    (Vb, seg, arc): Vb is the (m,2) loop, seg the closed-loop segments, arc the rim
    polyline P_alpha..P_beta (shared with the bottom). The two cuts are exact
    rotations of each other, so replicate2d welds them seamlessly."""
    th_s, r_s = _radial_outline(region)
    beta = 2 * np.pi / fold
    alpha = float(th_s[np.argmin(r_s)])               # cut through a valley
    rmax = float(_r_at(th_s, r_s, alpha))
    rr = _arc_sample_radial(alpha, rmax, Hfn, target)
    cutA = np.column_stack([rr * np.cos(alpha), rr * np.sin(alpha)])
    cutB = np.column_stack([rr * np.cos(alpha + beta), rr * np.sin(alpha + beta)])
    arc = _arc_sample_outline(th_s, r_s, alpha, alpha + beta, Hfn, target)
    loop = [(0.0, 0.0)]                                # center (shared by both cuts)
    loop += [tuple(p) for p in cutA[1:-1]]
    loop += [tuple(p) for p in arc]                   # P_alpha .. P_beta (incl. ends)
    loop += [tuple(p) for p in cutB[1:-1][::-1]]
    Vb = np.array(loop)
    m = len(Vb)
    seg = np.array([[i, (i + 1) % m] for i in range(m)])
    return Vb, seg, arc


def seed_interior(Vb, seg, target, min_angle=28.0):
    """A coarse 2D-uniform interior point seed for the remesh (Triangle Steiner
    points inside the frozen loop). The graph remesh then redistributes them for
    3D uniformity."""
    out = tr.triangulate({"vertices": Vb, "segments": seg},
                         f"pq{min_angle}a{base_area(target):.6f}Y")
    return out["vertices"][len(Vb):]


def wedge_mesh_metric(region, fold, coarse, edge_b=0.4, base=0.8,
                      target3d=0.9, min_angle=28.0):
    """Metric wedge mesh: FINE matched cuts/outline (edge_b) + interior refined
    by the lifted surface gradient (finer where steep, e.g. the wall->dome
    shoulder). `coarse` = (Vc, Tc, Hc) from a first pass supplies |grad H|."""
    from scipy.interpolate import LinearNDInterpolator, NearestNDInterpolator
    Vc, Tc, Hc = coarse
    V, seg, flag, m = _wedge_loop(region, fold, edge_b)
    t0 = tr.triangulate({"vertices": V, "segments": seg},
                        f"pq{min_angle}a{base_area(base):.6f}Y")
    cents0 = Vc[Tc].mean(1); g0 = tri_gradient_mag(Vc, Tc, Hc)
    gi = LinearNDInterpolator(cents0, g0); gn = NearestNDInterpolator(cents0, g0)
    cents = t0["vertices"][t0["triangles"]].mean(1)
    g = gi(cents); nan = np.isnan(g); g[nan] = gn(cents[nan])
    amax, amin = base_area(target3d), base_area(0.3)
    t0["triangle_max_area"] = np.clip(amax / (1 + g ** 2), amin, amax).reshape(-1, 1)
    t1 = tr.triangulate(t0, f"rpq{min_angle}aY")
    VV, T = t1["vertices"], t1["triangles"]
    fixed = np.arange(len(VV)) < m
    VV, T = _improve_wedge(VV, T, fixed, seg)
    is_outline = np.zeros(len(VV), bool); is_outline[:m] = flag
    return VV, T, is_outline


# --------------------------------------------------------------- diagnostics
if __name__ == "__main__":
    from pathlib import Path
    from domain import load_region
    here = Path(__file__).resolve().parent.parent
    fig, axes = plt.subplots(1, 3, figsize=(13, 4.6))
    for ax, (name, fold) in zip(axes, FOLD.items()):
        svg = {"EAT": "eat.svg", "MOVE": "move.svg", "GROW": "grow.svg"}[name]
        reg = load_region(here / "inputs" / svg, name)
        sym = symmetrize_region(reg, fold)
        ax.plot(np.r_[reg.outer[:, 0], reg.outer[0, 0]],
                np.r_[reg.outer[:, 1], reg.outer[0, 1]], "-", color="0.6", lw=2, label="original")
        ax.plot(np.r_[sym.outer[:, 0], sym.outer[0, 0]],
                np.r_[sym.outer[:, 1], sym.outer[0, 1]], "r-", lw=1, label=f"{fold}-fold sym")
        ax.set_aspect("equal"); ax.legend(fontsize=7)
        ax.set_title(f"{name} ({fold}-fold)  area {reg.area():.0f}->{sym.area():.0f}mm^2", fontsize=9)
    fig.suptitle("Step 1: exact N-fold symmetrized silhouettes (red) vs original (gray)",
                 fontsize=13, weight="bold")
    fig.tight_layout()
    p = here / "diagrams" / "sym_outlines.png"
    fig.savefig(p, dpi=130); print("wrote", p)
