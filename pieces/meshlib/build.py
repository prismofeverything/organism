"""
End-to-end build: domain -> mesh2d -> field -> lift -> solid -> remesh -> checks -> OBJ.

A uniform coarse mesh is lifted and closed; a single 3D isotropic remesh pass
then makes triangles uniform + near-equilateral everywhere (top, wall, rim).

    .venv/bin/python pieces/meshlib/build.py            # all pieces
    .venv/bin/python pieces/meshlib/build.py GROW       # one piece
"""
from __future__ import annotations
import sys
from pathlib import Path
import numpy as np
import trimesh

from scipy.interpolate import LinearNDInterpolator, NearestNDInterpolator

from domain import load_region, round_corners
from mesh2d import triangulate_region, _infer_boundary
from field import membrane_field, smooth_scalar
from profile import height
from solid import build_solid, build_solid_split, inset_ring
from remesh import isotropic_remesh
from invariants import Spec, validate
from symmetry import (symmetrize_region, wedge_mesh_star, wedge_mesh_metric, replicate2d,
                      slice_loop, resample_ring, symmetrize_ring, align_ring)
from collar import build_wedge_top, build_bottom_wedge

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "out"; OUT.mkdir(exist_ok=True)
INIT_EDGE = 0.5          # initial lift resolution
REMESH_EDGE = 0.8        # final uniform triangle size
TARGET_EDGE = 1.0        # nominal size for invariants (long-edge limit = 1.5x)
CORNER_RADIUS = 1.5      # round sharp silhouette corners (edge/radius < 45deg)
RIM_FILLET = 1.1         # round the bottom rim (G1, sculptable) instead of a hard 90deg edge

FOLD = {"EAT": 5, "MOVE": 3, "GROW": 4}   # rotational symmetry order

# name -> (svg, z_max, shape, wall_frac, smooth_iters)
# smooth_iters rounds the wall->dome shoulder; MOVE needs heavy rounding to keep
# its thin spiral crest smooth, then the height is rescaled back up to z_max.
SPECS = {
    "EAT":  ("eat.svg",  48.0, "hemisphere", 7 / 8,  40),
    "MOVE": ("move.svg", 60.0, "shallow",    0.03,    0),  # tall; arms RAISED; small wall lifts rim for the bottom fillet
    "GROW": ("grow.svg", 36.0, "hemisphere", 0.5,   40),
}


def face_min_angles(V, F):
    P = V[F]
    out = []
    for i in range(3):
        a = P[:, (i + 1) % 3] - P[:, i]
        b = P[:, (i + 2) % 3] - P[:, i]
        cos = np.einsum("ij,ij->i", a, b) / (
            np.linalg.norm(a, axis=1) * np.linalg.norm(b, axis=1) + 1e-12)
        out.append(np.degrees(np.arccos(np.clip(cos, -1, 1))))
    return np.min(np.array(out), axis=0)


def build_piece(name, symmetric_shape=False):
    """Asymmetric (global-remesh) build. With symmetric_shape=True the region is
    first symmetrized, so the SHAPE/silhouette/profile are exactly N-fold and only
    the triangle pattern is irregular — the practical fallback for MOVE (Blender
    sculpt symmetry is spatial, so the sculpted result is still symmetric)."""
    svg, zmax, shape, wall, sm_iters = SPECS[name]
    reg = load_region(ROOT / "inputs" / svg, name)
    if symmetric_shape:
        reg = symmetrize_region(reg, FOLD[name])
    reg = round_corners(reg, radius=CORNER_RADIUS)

    V, T, bnd = triangulate_region(reg, edge=INIT_EDGE)
    u = membrane_field(V, T, bnd)
    H = height(u, zmax, shape, wall)
    if sm_iters:
        H = smooth_scalar(V, T, H, bnd, iters=sm_iters)    # fillet the wall->dome shoulder
    H *= zmax / H.max()                                    # rescale to exact target height
    H_floor = height(u, zmax, "parabola", wall)            # additive floor: never carve below
    V3, F = build_solid(V, T, H, edge=INIT_EDGE, fillet_r=RIM_FILLET)
    Vr, Fr = isotropic_remesh(V3, F, target_edge=REMESH_EDGE)

    mesh = trimesh.Trimesh(vertices=Vr, faces=Fr, process=True)
    trimesh.repair.fix_normals(mesh)

    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=FOLD[name])
    print(f"\n=== {name} ===  {len(mesh.vertices)} verts, {len(mesh.faces)} faces, "
          f"vol={mesh.volume:.0f}mm^3")
    ok, _ = validate(mesh, reg, spec, ideal=(H, H_floor))
    mesh.export(OUT / f"{name}.obj")
    print(f"  wrote {OUT / f'{name}.obj'}  [{'OK' if ok else 'INVARIANTS FAILED'}]")
    return mesh


SYM_TARGET = 0.7         # wedge triangle size for the symmetric build


def _make_Hfn(V2, H):
    """Continuous height field z=H(x,y) from a per-vertex solve, for placing the
    collar rings / measuring 3D edges. Symmetric in, symmetric out."""
    li = LinearNDInterpolator(V2, H); ne = NearestNDInterpolator(V2, H)
    def f(P):
        P = np.atleast_2d(np.asarray(P, float)); z = li(P); nan = np.isnan(z)
        if nan.any():
            z[nan] = ne(P[nan])
        return z
    return f


def _solve_field(V2, T, zmax, shape, wall, sm_iters):
    bnd = _infer_boundary(V2, T)
    u = membrane_field(V2, T, bnd)
    H = height(u, zmax, shape, wall)
    if sm_iters:
        H = smooth_scalar(V2, T, H, bnd, iters=sm_iters)
    H *= zmax / H.max()
    return H, height(u, zmax, "parabola", wall)


def build_piece_symmetric(name, target=SYM_TARGET, outer=3):
    """Exactly N-fold symmetric build WITH full mesh quality.

    Mesh ONE wedge as a structured level-set COLLAR over the steep wall->dome rim
    plus a CVT CAP over the gentle top; the cut runs through a LOBE so the frozen
    matched cuts sit in easy convex geometry. Replicate+weld (cuts are exact
    rotations -> exact N-fold), solve the membrane field on the FULL mesh
    (rotation-equivariant -> symmetric H), lift, and close with an INDEPENDENT flat
    bottom (its own isotropic mesh; a flat copy of the collar would sliver) + wall.
    A metric outer-loop refines the field the rings are placed on. No global remesh
    (it would break symmetry)."""
    svg, zmax, shape, wall, sm = SPECS[name]
    fold = FOLD[name]
    reg = round_corners(symmetrize_region(load_region(ROOT / "inputs" / svg, name), fold),
                        radius=CORNER_RADIUS)

    # initial coarse field -> metric for ring placement
    Vw, Tw, _ = wedge_mesh_star(reg, fold, edge=0.6)
    Vc, Tc, _ = replicate2d(Vw, Tw, fold)
    Hfn = _make_Hfn(Vc, _solve_field(Vc, Tc, zmax, shape, wall, sm)[0])

    Vt = Tt = Ht = Ff = ring0 = None
    for _ in range(outer):                       # refine the metric the rings sit on
        Vtw, Ttw, ring0 = build_wedge_top(reg, fold, Hfn, wall * zmax, zmax, target)
        Vt, Tt, _ = replicate2d(Vtw, Ttw, fold)
        Ht, Ff = _solve_field(Vt, Tt, zmax, shape, wall, sm)
        Hfn = _make_Hfn(Vt, Ht)

    Vbw, Tbw = build_bottom_wedge(fold, inset_ring(ring0, RIM_FILLET), target)
    Vb, Tb, _ = replicate2d(Vbw, Tbw, fold)
    V3, F = build_solid_split(Vt, Tt, Ht, Vb, Tb, edge=target, fillet_r=RIM_FILLET)

    mesh = trimesh.Trimesh(vertices=V3, faces=F, process=True)
    trimesh.repair.fix_normals(mesh)
    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=fold)
    print(f"\n=== {name} (symmetric) ===  {len(mesh.vertices)} verts, {len(mesh.faces)} faces, "
          f"vol={mesh.volume:.0f}mm^3")
    ok, _ = validate(mesh, reg, spec, ideal=(Ht, Ff))
    mesh.export(OUT / f"{name}.obj")
    print(f"  wrote {OUT / f'{name}.obj'}  [{'OK' if ok else 'INVARIANTS FAILED'}]")
    return mesh


MOVE_FILLET = 0.7        # rounded bottom rim for MOVE; smaller than RIM_FILLET (1.1)
                         # because the spiral arm TIPS pinch to ~2mm necks (a >=0.9mm
                         # inset from both sides would collapse them). Still G1/sculptable.
MOVE_M = 192             # ring resolution for the re-stack (3*2^6); columns per sector M/3
MOVE_WALL_Z = 1.4        # slice the source body above its own rounded rim (RIM_FILLET 1.1)


def _weld3d(V, F, weld=1e-5):
    """Weld coincident vertices (round to `weld`) and drop the collapsed faces."""
    V = np.asarray(V, float)
    _, idx, inv = np.unique(np.round(V / weld).astype(np.int64), axis=0,
                            return_index=True, return_inverse=True)
    Fw = inv[np.asarray(F, int)]
    good = (Fw[:, 0] != Fw[:, 1]) & (Fw[:, 1] != Fw[:, 2]) & (Fw[:, 0] != Fw[:, 2])
    return V[idx], Fw[good]


def _replicate3d(V, F, fold, beta, weld=1e-5):
    """Rotate-copy a 3D orange-slice `fold` times about z and weld the coincident
    cut-curve + axis vertices -> watertight, exactly fold-fold (the slice's two cut
    cross-sections are exact rotations). Returns (V, F, copy0_mask)."""
    n = len(V); nF = len(F); Vs, Fs = [], []
    for k in range(fold):
        a = k * beta; c, s = np.cos(a), np.sin(a)
        R = np.array([[c, -s, 0.0], [s, c, 0.0], [0.0, 0.0, 1.0]])
        Vs.append(np.asarray(V) @ R.T); Fs.append(np.asarray(F) + k * n)
    Vall = np.vstack(Vs); Fall = np.vstack(Fs)
    _, idx, inv = np.unique(np.round(Vall / weld).astype(np.int64), axis=0,
                            return_index=True, return_inverse=True)
    Fw = inv[Fall]
    good = (Fw[:, 0] != Fw[:, 1]) & (Fw[:, 1] != Fw[:, 2]) & (Fw[:, 0] != Fw[:, 2])
    copy0 = np.zeros(len(Fall), bool); copy0[:nF] = True
    return Vall[idx], Fw[good], copy0[good]


def _inset_loop(ring, d, fold):
    """Fold-safe inset of a CLOSED symmetric ring inward by `d`: miter targets projected
    onto a shapely buffer(-d) (resolves the spiral-tip neck folds), kept 1:1/in-order with
    `ring`, then symmetrized so inset[i+M/fold] is the EXACT rotation of inset[i]."""
    from shapely.geometry import Polygon, Point, LineString
    ring = np.asarray(ring, float)
    e = np.roll(ring, -1, 0) - ring
    e = e / np.maximum(np.hypot(e[:, 0], e[:, 1]), 1e-9)[:, None]
    inw = np.stack([-e[:, 1], e[:, 0]], axis=1)
    nrm = inw + np.roll(inw, 1, 0)
    nrm = nrm / np.maximum(np.hypot(nrm[:, 0], nrm[:, 1]), 1e-9)[:, None]
    tgt = ring + nrm * d
    poly = Polygon(ring).buffer(-d)
    if poly.geom_type != "Polygon":
        poly = max(poly.geoms, key=lambda g: g.area)
    rg = LineString(poly.exterior.coords)
    out = np.array([rg.interpolate(rg.project(Point(t[0], t[1]))).coords[0] for t in tgt])
    return symmetrize_ring(out, fold)


def _flat_wedge_from_arc(arc, edge, min_angle=28.0):
    """Flat (z=0) Triangle mesh of one sector bounded by the real `arc` (throat0..throat1)
    and two radial cuts to the centre. Anchoring the cut at the THROAT (not the arm tip)
    keeps this loop simple even for MOVE's hooked arm -- unlike build_bottom_wedge's CVT,
    which adds a Steiner on the non-star arc and self-intersects. For the inset flat base.
    Returns (V2, T)."""
    import triangle as tr
    from mesh2d import base_area
    arc = np.asarray(arc, float)
    aA = np.arctan2(arc[0, 1], arc[0, 0]); aB = np.arctan2(arc[-1, 1], arc[-1, 0])
    rA = np.hypot(*arc[0]); rB = np.hypot(*arc[-1])
    cutA = np.linspace(0, rA, max(2, int(round(rA / edge))) + 1)[:, None] * [np.cos(aA), np.sin(aA)]
    cutB = np.linspace(0, rB, max(2, int(round(rB / edge))) + 1)[:, None] * [np.cos(aB), np.sin(aB)]
    loop = ([(0.0, 0.0)] + [tuple(p) for p in cutA[1:-1]] + [tuple(p) for p in arc]
            + [tuple(p) for p in cutB[1:-1][::-1]])
    V = np.array(loop); m = len(V)
    seg = np.array([[i, (i + 1) % m] for i in range(m)])
    out = tr.triangulate({"vertices": V, "segments": seg},
                         f"pq{min_angle}a{base_area(edge):.6f}Y")
    return out["vertices"], out["triangles"]


def _move_orange_slice(src, fold, M, wall_z, rf, zmax, edge, zs=None):
    """Build ONE orange-slice (sector 0) of a piece by stacking HORIZONTAL slices of the
    source mesh. Each slice is resampled to M points, symmetrized (so index i+M/fold is
    the exact 120 deg partner -> column 0 & column M/fold are a clean, exact-rotation
    CUT), and registered to the one above (no spiral twist). The cut follows the drifting
    throat one point per z-level, so it resolves a steep hub cliff a radial cut can't.
    Closes with a vertical wall -> quarter-round fillet -> miter-inset flat base wedge.
    `zs` = z-levels to slice (default: uniform 0.6mm to the tip; build_blank passes adaptive
    ones). Returns (V, F) of the welded slice; replicate3d tiles it into the full solid."""
    k = M // fold
    if zs is None:
        # z-spacing FINER than the remesh target: the cut runs along the slice stack and is
        # LOCKED by sel-remesh, so its 3D edge == the z-step (times the throat's r-slope,
        # steepest near the apex). 0.6mm keeps even near-apex cut edges <1.5mm; slice to the
        # tip so the apex fan + its locked cut->apex edge stay short.
        zs = np.linspace(wall_z, zmax - 0.06, max(24, int(round((zmax - wall_z) / 0.6))))
    anchored = False; ref = None; rings = []
    for z in zs:
        loop = slice_loop(src, z)
        r = resample_ring(loop, M)
        if not anchored:
            # anchor column 0 at the THROAT POINT (min radius), NOT nearest-angle: the
            # hooked arm reaches the same angle as the throat, so angle-anchoring can grab
            # the arm tip -> the base cut would run centre->tip and self-intersect the arm.
            thr = loop[np.argmin(np.hypot(loop[:, 0], loop[:, 1]))]
            r = np.roll(r, -int(np.argmin(np.hypot(r[:, 0] - thr[0], r[:, 1] - thr[1]))), axis=0)
            anchored = True
        r = symmetrize_ring(r, fold)
        if ref is not None:
            r = align_ring(ref, r)
        ref = r; rings.append(r)
    rim = rings[0]
    inset = _inset_loop(rim, rf, fold)
    rim_arc, inset_arc = rim[:k + 1], inset[:k + 1]

    V, F = [], []
    def addring(pts, z):
        b = len(V); V.extend((float(p[0]), float(p[1]), float(z)) for p in pts)
        return list(range(b, len(V)))
    def loft(A, B):
        for i in range(len(A) - 1):
            a, b, c, d = A[i], A[i + 1], B[i], B[i + 1]
            da = (V[a][0]-V[d][0])**2+(V[a][1]-V[d][1])**2+(V[a][2]-V[d][2])**2
            db = (V[b][0]-V[c][0])**2+(V[b][1]-V[c][1])**2+(V[b][2]-V[c][2])**2
            if da <= db: F.append([a, b, d]); F.append([a, d, c])
            else: F.append([a, b, c]); F.append([b, d, c])

    body = [addring(rings[j][:k + 1], zs[j]) for j in range(len(zs))]
    for j in range(len(zs) - 1):
        loft(body[j], body[j + 1])
    apex = len(V); V.append((0.0, 0.0, float(zmax)))      # apex fan
    for i in range(len(body[-1]) - 1):
        F.append([body[-1][i], body[-1][i + 1], apex])

    prev = body[0]                                        # rim arc at z=wall_z
    nz = max(1, int(round((wall_z - rf) / edge)))         # vertical wall wall_z -> rf
    for s in range(1, nz + 1):
        cur = addring(rim_arc, wall_z - (wall_z - rf) * s / nz); loft(prev, cur); prev = cur
    nfil = max(2, int(round(rf * (np.pi / 2) / edge)))    # quarter-round fillet rf -> 0
    for s in range(1, nfil + 1):
        if s == nfil:
            cur = addring(inset_arc, 0.0)
        else:
            t = s / nfil; w = np.cos(t * np.pi / 2)
            cur = addring(inset_arc + (rim_arc - inset_arc) * w, rf * (1 - np.sin(t * np.pi / 2)))
        loft(prev, cur); prev = cur

    Vbw, Tbw = _flat_wedge_from_arc(inset_arc, edge)      # flat base wedge (rim = inset_arc)
    off = len(V); V.extend((float(p[0]), float(p[1]), 0.0) for p in Vbw)
    for t in Tbw:
        F.append([t[0] + off, t[1] + off, t[2] + off])
    return _weld3d(V, F)


def _sym_remesh(V, F, copy0, fold, beta, target):
    """Make the exactly-fold-fold orange-slice solid uniform WITHOUT breaking symmetry:
    remesh only sector 0's faces (`selectedonly`), which LOCKS the cut curves (the
    selection boundary) byte-for-byte, then replicate the remeshed sector. Returns (V, F)."""
    import pymeshlab as ml
    fq = np.where(copy0, 0.0, 1.0).astype(float)
    ms = ml.MeshSet()
    ms.add_mesh(ml.Mesh(vertex_matrix=np.asarray(V, float),
                        face_matrix=np.asarray(F, np.int32), f_scalar_array=fq))
    ms.compute_selection_by_condition_per_face(condselect="fq < 0.5")
    ms.meshing_isotropic_explicit_remeshing(
        targetlen=ml.PureValue(target), iterations=14, adaptive=False,
        selectedonly=True, featuredeg=65.0)                  # keep only the crisp rim;
        # smooth the spiral arm's interior ridge (its preserved crease left thin tris)
    mm = ms.current_mesh(); Vr = mm.vertex_matrix(); Fr = mm.face_matrix()
    Fw = Fr[mm.face_selection_array().astype(bool)]      # the remeshed sector (selection-tracked)
    used = np.unique(Fw); remap = -np.ones(len(Vr), int); remap[used] = np.arange(len(used))
    Vrep, Frep, _ = _replicate3d(Vr[used], remap[Fw], fold, beta)
    return Vrep, Frep


def build_move_symmetric(M=MOVE_M, target=REMESH_EDGE, fillet=MOVE_FILLET):
    """Exactly 3-fold MOVE with full mesh quality + a rounded sculptable bottom rim,
    for the NON-STAR spiral. A radial-cut wedge can't resolve MOVE's near-vertical hub
    cliff, so we RE-STACK horizontal slices of the (global-remesh) source into an exactly
    3-fold orange slice (the cut follows the drifting throat, one point per z-level),
    replicate to the full solid, then _sym_remesh (sector-locked global remesh) makes it
    uniform while keeping it exactly 3-fold."""
    name = "MOVE"; svg, zmax, shape, wall, sm = SPECS[name]; fold = FOLD[name]
    reg = round_corners(symmetrize_region(load_region(ROOT / "inputs" / svg, name), fold),
                        radius=CORNER_RADIUS)
    beta = 2 * np.pi / fold

    src = build_piece(name, symmetric_shape=True)         # global-remesh source (good shape)
    Vs, Fs = _move_orange_slice(src, fold, M, MOVE_WALL_Z, fillet, zmax, target)
    Vfull, Ffull, copy0 = _replicate3d(Vs, Fs, fold, beta)
    Vr, Fr = _sym_remesh(Vfull, Ffull, copy0, fold, beta, target)

    mesh = trimesh.Trimesh(vertices=Vr, faces=Fr, process=True)
    trimesh.repair.fix_normals(mesh)
    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=fold)
    print(f"\n=== {name} (symmetric) ===  {len(mesh.vertices)} verts, {len(mesh.faces)} faces, "
          f"vol={mesh.volume:.0f}mm^3")
    ok, _ = validate(mesh, reg, spec)
    mesh.export(OUT / f"{name}.obj")
    print(f"  wrote {OUT / f'{name}.obj'}  [{'OK' if ok else 'INVARIANTS FAILED'}]")
    return mesh


# ==================== unified, shape-agnostic blank builder ====================
# The re-stack -> replicate -> sector-locked-remesh finisher is shape-agnostic (it works
# for a star or a hooked spiral alike). build_blank wraps it with auto-derived per-piece
# knobs so ONE call -- silhouette + radial profile + fold -> 8/8 sculptable blank -- covers
# every piece. The dedicated build_piece_symmetric (star collar) and build_move_symmetric
# paths are kept alongside.

def _min_neck(P):
    """Min distance between NON-adjacent points of a loop -- the narrowest 'neck' (e.g. a
    spiral arm tip). Caps the bottom fillet: a >0.5*neck inset from both sides collapses it."""
    n = len(P)
    D = np.hypot(P[:, 0, None] - P[None, :, 0], P[:, 1, None] - P[None, :, 1])
    ii = np.arange(n); off = np.abs(ii[:, None] - ii[None, :])
    D[(off <= 2) | (off >= n - 2)] = np.inf
    return float(D.min())


def _auto_blank_knobs(src, fold, target):
    """Derive (M, fillet, wall_z) for build_blank from the source mesh: M from the widest
    slice's perimeter (arc spacing ~target, divisible by fold), wall_z just above the
    source's own rounded rim, fillet from the narrowest neck of the rim ring."""
    z = src.vertices[:, 2]; zlo, zhi = float(z.min()), float(z.max())
    wall_z = zlo + RIM_FILLET + 0.3
    perims = []
    for zz in np.linspace(wall_z, zhi - 0.5, 14):
        try:
            perims.append(_perim_loop(slice_loop(src, zz)))
        except ValueError:
            pass
    M = int(np.ceil(max(perims) / target / fold)) * fold
    rim = resample_ring(slice_loop(src, wall_z), M)
    fillet = float(min(RIM_FILLET, 0.38 * _min_neck(rim)))
    return M, fillet, wall_z


def _perim_loop(loop):
    return float(np.sum(np.hypot(*np.diff(np.vstack([loop, loop[:1]]), axis=0).T)))


def _adaptive_zlevels(src, fold, M, wall_z, zmax, target):
    """z-levels spaced ~uniformly in 3D ALONG THE CUT (column 0 of the symmetric stack):
    a coarse pass traces the drifting throat, then we resample z by the cut's 3D arc length
    so its LOCKED edges stay ~target everywhere -- dense through the hub cliff, the wall->
    dome shoulder, and the apex; sparse on gentle spans. Generalizes the fixed 0.6mm step."""
    zc = np.linspace(wall_z, zmax - 0.06, 44)
    anchored = False; ref = None; col0 = []
    for zz in zc:
        loop = slice_loop(src, zz); r = resample_ring(loop, M)
        if not anchored:
            thr = loop[np.argmin(np.hypot(loop[:, 0], loop[:, 1]))]
            r = np.roll(r, -int(np.argmin(np.hypot(r[:, 0] - thr[0], r[:, 1] - thr[1]))), axis=0)
            anchored = True
        r = symmetrize_ring(r, fold)
        if ref is not None:
            r = align_ring(ref, r)
        ref = r; col0.append(r[0])
    P = np.column_stack([np.array(col0), zc])
    s = np.concatenate([[0], np.cumsum(np.linalg.norm(np.diff(P, axis=0), axis=1))])
    n = max(24, int(np.ceil(s[-1] / target)))
    return np.interp(np.linspace(0, s[-1], n), s, zc)


def build_blank(name, target=REMESH_EDGE, export=True):
    """ONE shape-agnostic path: silhouette + radial profile + fold -> 8/8 sculptable blank.
    Builds a global-remesh source for the shape, re-stacks its horizontal slices into an
    exactly fold-fold orange slice (auto M / fillet / adaptive z), replicates, then a
    sector-locked remesh makes it uniform while staying exactly fold-fold. Works for the
    star pieces (EAT/GROW) and the non-star spiral (MOVE) without per-shape symmetry code."""
    svg, zmax, shape, wall, sm = SPECS[name]; fold = FOLD[name]; beta = 2 * np.pi / fold
    reg = round_corners(symmetrize_region(load_region(ROOT / "inputs" / svg, name), fold),
                        radius=CORNER_RADIUS)
    src = build_piece(name, symmetric_shape=True)         # global-remesh source (shape only)
    M, fillet, wall_z = _auto_blank_knobs(src, fold, target)
    zs = _adaptive_zlevels(src, fold, M, wall_z, zmax, target)
    Vs, Fs = _move_orange_slice(src, fold, M, wall_z, fillet, zmax, target, zs=zs)
    Vfull, Ffull, copy0 = _replicate3d(Vs, Fs, fold, beta)
    Vr, Fr = _sym_remesh(Vfull, Ffull, copy0, fold, beta, target)
    mesh = trimesh.Trimesh(vertices=Vr, faces=Fr, process=True)
    trimesh.repair.fix_normals(mesh)
    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=fold)
    print(f"\n=== {name} (build_blank  M={M} fillet={fillet:.2f} zlevels={len(zs)}) ===  "
          f"{len(mesh.vertices)} verts, {len(mesh.faces)} faces")
    ok, _ = validate(mesh, reg, spec)
    if export:
        mesh.export(OUT / f"{name}.obj")
        print(f"  wrote {OUT / f'{name}.obj'}  [{'OK' if ok else 'INVARIANTS FAILED'}]")
    return mesh, ok


if __name__ == "__main__":
    args = sys.argv[1:]
    blank = "--blank" in args                              # unified shape-agnostic path
    for nm in ([a for a in args if a in SPECS] or list(SPECS)):
        if blank:
            build_blank(nm)
        elif nm == "MOVE":
            build_move_symmetric()
        else:
            build_piece_symmetric(nm)
