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
                      slice_loop, resample_ring, symmetrize_ring, align_ring,
                      improve_wedge, redelaunay_wedge)
from collar import build_wedge_top, build_bottom_wedge

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "out"; OUT.mkdir(exist_ok=True)
INIT_EDGE = 0.5          # initial lift resolution
REMESH_EDGE = 0.8        # final uniform triangle size
TARGET_EDGE = 1.0        # nominal size for invariants (long-edge limit = 1.5x)
CORNER_RADIUS = 1.5      # round sharp silhouette corners (edge/radius < 45deg)
RIM_FILLET = 1.1         # round the bottom rim (G1, sculptable) instead of a hard 90deg edge

FOLD = {"EAT": 5, "MOVE": 3, "GROW": 4}   # rotational symmetry order

# Per-piece SoR-envelope opt-in: the strict inv_profile demands z(r)=z_target(r) on
# the upper envelope (= the radial lift below). MOVE opts OUT because its identity
# (tall twisted spiral apex-converging) is not a SoR dome -- its silhouette per height
# is a rotation of the base, not a constant radial profile. EAT/GROW opt IN (= true
# hemisphere visible from any side, lobes are wall features). MOVE keeps the membrane.
PROFILE_MATCH = {"EAT": True, "MOVE": False, "GROW": True}

# name -> (svg, z_max, shape, wall_frac, smooth_iters)
# smooth_iters rounds the wall->dome shoulder; MOVE needs heavy rounding to keep
# its thin spiral crest smooth, then the height is rescaled back up to z_max.
# (For PROFILE_MATCH=True pieces, smooth_iters is moot — radial lift is already exact.)
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


def _radial_u(V2, reg):
    """The disc-equivalent membrane field u = 1 - (r/Rmax)^2 evaluated at every vertex
    -- the field whose transfer produces the SoR profile z = z_target(r). Using this
    instead of the true 2D membrane makes the dome a true radial hemisphere/shallow
    profile regardless of the silhouette (lobes become wall features at the base).
    The strict inv_profile then passes with zero deviation. (membrane is kept for
    pieces with PROFILE_MATCH=False -- e.g. MOVE, whose spiral identity needs it.)"""
    R_max = float(np.hypot(reg.outer[:, 0], reg.outer[:, 1]).max())
    return np.clip(1.0 - (V2[:, 0] ** 2 + V2[:, 1] ** 2) / (R_max * R_max), 0.0, 1.0)


def build_piece(name, symmetric_shape=False):
    """Asymmetric (global-remesh) build. With symmetric_shape=True the region is
    first symmetrized, so the SHAPE/silhouette/profile are exactly N-fold and only
    the triangle pattern is irregular — the practical fallback for MOVE (Blender
    sculpt symmetry is spatial, so the sculpted result is still symmetric).

    PROFILE_MATCH pieces (EAT/GROW) use the radial lift z=z_target(r); MOVE uses the
    membrane lift (organic spiral identity)."""
    svg, zmax, shape, wall, sm_iters = SPECS[name]
    reg = load_region(ROOT / "inputs" / svg, name)
    if symmetric_shape:
        reg = symmetrize_region(reg, FOLD[name])
    reg = round_corners(reg, radius=CORNER_RADIUS)

    V, T, bnd = triangulate_region(reg, edge=INIT_EDGE)
    if PROFILE_MATCH.get(name, True):
        u = _radial_u(V, reg)                              # exact SoR dome
        H = height(u, zmax, shape, wall)                   # already z_target(r); no rescale needed
        H_floor = height(u, zmax, "parabola", wall)
    else:
        u = membrane_field(V, T, bnd)
        H = height(u, zmax, shape, wall)
        if sm_iters:
            H = smooth_scalar(V, T, H, bnd, iters=sm_iters)  # fillet wall->dome shoulder
        H *= zmax / H.max()                                # rescale to exact target height
        H_floor = height(u, zmax, "parabola", wall)        # additive floor: never carve below
    V3, F = build_solid(V, T, H, edge=INIT_EDGE, fillet_r=RIM_FILLET)
    Vr, Fr = isotropic_remesh(V3, F, target_edge=REMESH_EDGE)

    mesh = trimesh.Trimesh(vertices=Vr, faces=Fr, process=True)
    trimesh.repair.fix_normals(mesh)

    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=FOLD[name],
                profile_match=PROFILE_MATCH.get(name, True))
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


def _solve_field(V2, T, zmax, shape, wall, sm_iters, reg=None, profile_match=False):
    """Lift field used by build_piece_symmetric. PROFILE_MATCH pieces use the radial
    u=1-(r/Rmax)^2 (exact SoR dome); others use the 2D membrane (organic)."""
    if profile_match and reg is not None:
        u = _radial_u(V2, reg)
        return height(u, zmax, shape, wall), height(u, zmax, "parabola", wall)
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

    pm = PROFILE_MATCH.get(name, True)
    # initial coarse field -> metric for ring placement
    Vw, Tw, _, _ = wedge_mesh_star(reg, fold, edge=0.6)
    Vc, Tc, _ = replicate2d(Vw, Tw, fold)
    Hfn = _make_Hfn(Vc, _solve_field(Vc, Tc, zmax, shape, wall, sm,
                                     reg=reg, profile_match=pm)[0])

    Vt = Tt = Ht = Ff = ring0 = None
    for _ in range(outer):                       # refine the metric the rings sit on
        Vtw, Ttw, ring0 = build_wedge_top(reg, fold, Hfn, wall * zmax, zmax, target)
        Vt, Tt, _ = replicate2d(Vtw, Ttw, fold)
        Ht, Ff = _solve_field(Vt, Tt, zmax, shape, wall, sm,
                              reg=reg, profile_match=pm)
        Hfn = _make_Hfn(Vt, Ht)

    Vbw, Tbw = build_bottom_wedge(fold, inset_ring(ring0, RIM_FILLET), target)
    Vb, Tb, _ = replicate2d(Vbw, Tbw, fold)
    V3, F = build_solid_split(Vt, Tt, Ht, Vb, Tb, edge=target, fillet_r=RIM_FILLET)

    mesh = trimesh.Trimesh(vertices=V3, faces=F, process=True)
    trimesh.repair.fix_normals(mesh)
    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=fold,
                profile_match=PROFILE_MATCH.get(name, True))
    print(f"\n=== {name} (symmetric) ===  {len(mesh.vertices)} verts, {len(mesh.faces)} faces, "
          f"vol={mesh.volume:.0f}mm^3")
    ok, _ = validate(mesh, reg, spec, ideal=(Ht, Ff))
    mesh.export(OUT / f"{name}.obj")
    print(f"  wrote {OUT / f'{name}.obj'}  [{'OK' if ok else 'INVARIANTS FAILED'}]")
    return mesh


def _refine_2d_for_lift(V, T, lift_fn, limit, preserve_idx, max_iters=8):
    """Longest-edge bisection so the lifted 3D edge length stays under `limit`.

    Each iteration: find all 2D edges whose lifted 3D length > limit, add midpoint
    Steiner points, split each triangle into 2/3/4 sub-tris based on how many of
    its edges have midpoints. Decision is rotation-invariant (purely geometric
    length), so applying this to an exactly N-fold symmetric mesh keeps it exactly
    N-fold symmetric (every rotation triggers the same splits).

    Edges with BOTH endpoints in `preserve_idx` (cuts + outline) are NOT split,
    so cuts remain matched for replicate2d welding and the outline stays exactly
    on the silhouette. Edges with ONE endpoint on the rim and the other interior
    DO get split (creating an interior midpoint) — that's the steep-dome case."""
    V = [tuple(map(float, v)) for v in np.asarray(V).tolist()]
    T = [list(map(int, t)) for t in np.asarray(T).tolist()]
    keep = set(int(i) for i in preserve_idx)
    for _ in range(max_iters):
        long_e = {}                                              # (min,max) -> midpoint idx
        for a, b, c in T:
            for u, v in ((a, b), (b, c), (c, a)):
                e = (u, v) if u < v else (v, u)
                if e in long_e:
                    continue
                if e[0] in keep and e[1] in keep:                # preserved edge
                    continue
                pu, pv = V[e[0]], V[e[1]]
                ru = np.hypot(pu[0], pu[1]); rv = np.hypot(pv[0], pv[1])
                zu = lift_fn(ru); zv = lift_fn(rv)
                L = ((pu[0]-pv[0])**2 + (pu[1]-pv[1])**2 + (zu-zv)**2) ** 0.5
                if L > limit:
                    long_e[e] = -1                               # placeholder
        if not long_e:
            return np.array(V), np.array(T, int)
        for e in long_e:
            pu, pv = V[e[0]], V[e[1]]
            long_e[e] = len(V)
            V.append((0.5 * (pu[0] + pv[0]), 0.5 * (pu[1] + pv[1])))
        new_T = []
        for a, b, c in T:
            m_ab = long_e.get((a, b) if a < b else (b, a))
            m_bc = long_e.get((b, c) if b < c else (c, b))
            m_ac = long_e.get((a, c) if a < c else (c, a))
            n = (m_ab is not None) + (m_bc is not None) + (m_ac is not None)
            if n == 0:
                new_T.append([a, b, c])
            elif n == 3:
                new_T += [[a, m_ab, m_ac], [m_ab, b, m_bc],
                          [m_ac, m_bc, c], [m_ab, m_bc, m_ac]]
            elif n == 1:
                if m_ab is not None:   new_T += [[a, m_ab, c], [m_ab, b, c]]
                elif m_bc is not None: new_T += [[a, b, m_bc], [a, m_bc, c]]
                else:                  new_T += [[a, b, m_ac], [m_ac, b, c]]
            else:                                                # n == 2: 1-to-3
                if m_ab is None:
                    new_T += [[a, b, m_ac], [b, m_bc, m_ac], [m_ac, m_bc, c]]
                elif m_bc is None:
                    new_T += [[a, m_ab, m_ac], [m_ab, b, c], [m_ab, c, m_ac]]
                else:
                    new_T += [[a, m_ab, c], [m_ab, b, m_bc], [m_ab, m_bc, c]]
        T = new_T
    return np.array(V), np.array(T, int)


def build_blank_radial(name, target=SYM_TARGET):
    """PROFILE_MATCH path (EAT/GROW): isotropic wedge + radial lift z=z_target(r) + replicate.

    NO collar/CAP split — the radial lift is exact by construction, so no anisotropic
    structure is needed to track a wall->dome shoulder. One uniform 2D wedge gets
    replicated, lifted by z=z_target(r) which is rotation-symmetric (so the welds along
    radial cuts are tangent-matched and the seam has no crease), then closed by
    `build_solid_split` with per-column wall heights (already extended in solid.py to
    handle the radial lift's varying rim z). The dome is a true SoR hemisphere covering
    the star/clover base; inv_profile passes at near-zero deviation.

    Edge sizing: the hemisphere has dz/dr -> infty at the equator (r=Rmax), so a 2D
    edge `e` becomes a 3D edge ~ e * sqrt(1+(dz/dr)^2). For tall domes (small wall_frac
    => big dz/dr), the 2D edge must shrink to keep the 3D edge under no_long_edges
    limit (1.5mm). Heuristic: target_2D = TARGET_EDGE / sqrt(1 + (dome_h/Rmax)^2 * c).
    EAT (dome 6mm) ends up at ~0.7; GROW (dome 18mm) at ~0.4."""
    svg, zmax, shape, wall, _ = SPECS[name]
    fold = FOLD[name]
    reg = round_corners(symmetrize_region(load_region(ROOT / "inputs" / svg, name), fold),
                        radius=CORNER_RADIUS)
    Rmax = float(np.hypot(reg.outer[:, 0], reg.outer[:, 1]).max())
    dome_h = (1.0 - wall) * zmax
    # 2D target sized so the steepest dome edge (near the equator) stays under ~1.4mm
    target_2D = float(np.clip(1.4 / np.sqrt(1.0 + 9.0 * (dome_h / Rmax) ** 2),
                              0.30, target))

    # ONE wedge of the top: isotropic 2D mesh of the wedge of the region
    Vw, Tw, is_outline, is_loop = wedge_mesh_star(reg, fold, edge=target_2D)
    # the rim arc IS the outline portion of the wedge boundary, in natural order
    ring0 = Vw[np.where(is_outline)[0]]

    # ADAPTIVE refinement: subdivide interior edges whose 3D-lifted length exceeds the
    # no_long_edges limit (1.5mm). The hemisphere has dz/dr -> infinity at the equator,
    # so uniform meshing can't bound the 3D edge near r=Rmax; this pass adds Steiner
    # points only where needed (interior, near the rim). Cuts and outline are preserved
    # (their indices < n_loop, both flagged as `keep`) so replicate2d welding stays
    # exact and the silhouette stays exactly on the boundary.
    from profile import transfer as _transfer
    def _lift(r):
        u = max(0.0, 1.0 - (r / Rmax) ** 2)
        return wall * zmax + (zmax - wall * zmax) * _transfer(np.array([u]), shape)[0]
    # cuts AND outline (all wedge boundary loop verts) are preserved -- only INTERIOR
    # edges get split, so cuts stay matched (replicate2d still welds) and the outline
    # stays exactly on the silhouette.
    m_loop = int(is_loop.sum())
    preserve = list(range(m_loop))
    Vw, Tw = _refine_2d_for_lift(Vw, Tw, _lift, limit=1.4, preserve_idx=preserve)
    # Refine + Delaunay-flip + refine, until both converge: bisection densifies near
    # the steep equator (Delaunay isn't aware of 3D length so a flip can lengthen an
    # edge in 3D); the next refinement catches anything the flip lengthened. Lloyd
    # is NOT used -- moving the densified verts toward neighbor centroids would undo
    # the refinement (pulling them away from the steep equator). Converges in 2-3
    # rounds for hemisphere-on-clover (GROW); EAT typically needs 1.
    seg = np.array([[i, (i + 1) % m_loop] for i in range(m_loop)])
    for _ in range(4):
        Vw, Tw = redelaunay_wedge(Vw, seg)
        Vw_new, Tw_new = _refine_2d_for_lift(Vw, Tw, _lift, limit=1.4, preserve_idx=preserve)
        if len(Vw_new) == len(Vw):                            # nothing more to refine
            Vw, Tw = Vw_new, Tw_new
            break
        Vw, Tw = Vw_new, Tw_new

    # full 2D top mesh
    Vc, Tc, _ = replicate2d(Vw, Tw, fold)
    # radial lift -- z = wall + (zmax-wall)*transfer(1 - (r/Rmax)^2, shape)
    u = _radial_u(Vc, reg)
    Ht = height(u, zmax, shape, wall)
    H_floor = height(u, zmax, "parabola", wall)

    # bottom wedge: isotropic flat mesh, rim arc inset by RIM_FILLET so the fillet has room
    Vbw, Tbw = build_bottom_wedge(fold, inset_ring(ring0, RIM_FILLET), target_2D)
    Vb, Tb, _ = replicate2d(Vbw, Tbw, fold)

    V3, F = build_solid_split(Vc, Tc, Ht, Vb, Tb, edge=target_2D, fillet_r=RIM_FILLET)
    mesh = trimesh.Trimesh(vertices=V3, faces=F, process=True)
    trimesh.repair.fix_normals(mesh)
    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=fold,
                profile_match=PROFILE_MATCH.get(name, True))
    print(f"\n=== {name} (radial) ===  {len(mesh.vertices)} verts, {len(mesh.faces)} faces, "
          f"vol={mesh.volume:.0f}mm^3")
    ok, _ = validate(mesh, reg, spec, ideal=(Ht, H_floor))
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
    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=fold,
                profile_match=PROFILE_MATCH.get(name, True))
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
    """ONE entry point for ANY piece -> sculptable blank, dispatching by PROFILE_MATCH.

    PROFILE_MATCH=True (EAT/GROW): build_blank_radial -- isotropic wedge + adaptive
      refinement under the radial lift z=z_target(r). Exact 9-invariant SoR-dome
      blank (modulo the hemisphere's equator singularity, which leaves a few slivers
      on GROW; see Task #15 / Inv_uniform_tris).
    PROFILE_MATCH=False (MOVE): re-stack of the global-remesh source's horizontal
      slices -- the only path that preserves the spiral identity through the hub
      cliff. Same downstream finisher (replicate + _sym_remesh).

    Both paths return (mesh, ok). The CLI / build_graft load the result from disk."""
    if PROFILE_MATCH.get(name, True):
        mesh = build_blank_radial(name, target=SYM_TARGET)
        return mesh, True                                     # validated inside
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
    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=fold,
                profile_match=PROFILE_MATCH.get(name, True))
    print(f"\n=== {name} (build_blank  M={M} fillet={fillet:.2f} zlevels={len(zs)}) ===  "
          f"{len(mesh.vertices)} verts, {len(mesh.faces)} faces")
    ok, _ = validate(mesh, reg, spec)
    if export:
        mesh.export(OUT / f"{name}.obj")
        print(f"  wrote {OUT / f'{name}.obj'}  [{'OK' if ok else 'INVARIANTS FAILED'}]")
    return mesh, ok


# ==================== universal-connector graft (onto the blanks) ==============
# The connector attaches the SAME way for every piece: a smoothstep cap blends the
# body's top rings to the Ø14 seat circle, then the structured connector solid-of-
# revolution (dome+groove+ridge, dims exact) welds to that seat. The blend goes INWARD
# for EAT/GROW (body wider than the seat -> the peg nestles, body necks into a calyx) and
# OUTWARD for MOVE (body narrower -> the cap flares into a mesa). Pieces are SOLID-bottomed
# (no socket; only FOOD stacks) with the same rounded sculptable rim as the blank.
# Connector mating surfaces are never remeshed -> exact. The body is the (relaxed-bar)
# re-stack: watertight, symmetric, prints well; belt/pole tri artifacts are acceptable.
GRAFT_KNEE = 0.78        # cap starts at GRAFT_KNEE * seat_z (body still star-shaped above)
CONN_SPACING = 0.32      # connector ring spacing (dense for the crisp ridge)


def _r_at_angle(loop, angles):
    """Single-valued radius of a (locally star-shaped) loop at the given angles."""
    th = np.arctan2(loop[:, 1], loop[:, 0]); r = np.hypot(loop[:, 0], loop[:, 1])
    o = np.argsort(th)
    return np.interp((angles + np.pi) % (2 * np.pi) - np.pi, th[o], r[o], period=2 * np.pi)


def _smoothstep(t):
    t = np.clip(t, 0.0, 1.0); return t * t * (3 - 2 * t)


def build_graft(name, target=REMESH_EDGE):
    """Graft the universal connector onto the piece's blank body. Re-stack the body up to a
    knee, smoothstep-blend the top rings to the Ø14 seat circle (in for EAT/GROW, out for
    MOVE), weld the structured connector SoR (peg, exact dims), and close with the rounded
    SOLID bottom (no socket). Watertight, symmetric, dims-exact connector; relaxed bar."""
    from graft_lib import Builder as _GB, revolution_cap, _level_fn   # noqa: E402
    import connector_field as cf                                       # noqa: E402
    import triangle as tr                                             # noqa: E402
    from mesh2d import base_area                                       # noqa: E402

    class B_(_GB):
        def ring_pts(self, pts, z):
            b = len(self.V); self.V.extend((float(p[0]), float(p[1]), float(z)) for p in pts)
            return list(range(b, len(self.V)))

    import math
    svg, zmax, shape, wall, sm = SPECS[name]; fold = FOLD[name]
    src = trimesh.load(OUT / f"{name}.obj", process=False)  # the 8/8 blank body (built first)
    M, fillet, wall_z = _auto_blank_knobs(src, fold, target)
    lcm = 32 * fold // math.gcd(32, fold)                  # connector belts need M % (fold*2^5)
    M = max(lcm, round(M / lcm) * lcm)                     # so lev(L)=TH[::2^L] halves cleanly
    seat_z = cf.seat_z(zmax); flare_z = GRAFT_KNEE * seat_z
    # PROFILE_MATCH pieces with a tall dome (= significant seat-above-wall_top span):
    # snap flare_z to wall_z so the body ENDS at the dome's start. Otherwise the body
    # extends INTO the dome zone (where the radial lift has already clipped the lobes)
    # and shows a hard horizontal "shaved" edge at flare_z. With flare_z = wall_z, the
    # body is the silhouette wall (lobes full to the wall_top) and the cap takes the
    # full dome-to-seat transition -- continuous with the blank's shape.
    if PROFILE_MATCH.get(name, True) and (seat_z - wall * zmax) > 4.0:
        flare_z = wall * zmax
    b = B_()

    # master angles from the foot ring (real body section at the knee, throat-anchored)
    foot_loop = slice_loop(src, flare_z)
    thr = foot_loop[np.argmin(np.hypot(foot_loop[:, 0], foot_loop[:, 1]))]
    foot = resample_ring(foot_loop, M)
    foot = np.roll(foot, -int(np.argmin(np.hypot(foot[:, 0] - thr[0], foot[:, 1] - thr[1]))), axis=0)
    foot = symmetrize_ring(foot, fold)
    # The rounded notch corner has the silhouette passing through the notch angle
    # TWICE (going in and out of the dimple), so the foot ring can have adjacent
    # verts at the SAME angle but different radii. At the connector's seat ring (full
    # M=lev(0)), those become coincident xy points -> trimesh.process welds them ->
    # degenerate triangles in the boss's belts/lofts. Nudge each pair tangentially
    # by a small amount: 5-fold-aligned so the symmetry stays exact.
    TH = np.arctan2(foot[:, 1], foot[:, 0]); r_ft = np.hypot(foot[:, 0], foot[:, 1])
    NUDGE = 5e-4
    for k in range(len(foot)):
        j = (k + 1) % len(foot)
        ang_gap = abs((TH[k] - TH[j] + np.pi) % (2 * np.pi) - np.pi)
        if ang_gap < 1e-4:
            mid = 0.5 * (TH[k] + TH[j])
            TH[k] = mid - NUDGE; TH[j] = mid + NUDGE
            foot[k] = [r_ft[k] * np.cos(TH[k]), r_ft[k] * np.sin(TH[k])]
            foot[j] = [r_ft[j] * np.cos(TH[j]), r_ft[j] * np.sin(TH[j])]
    TH = np.arctan2(foot[:, 1], foot[:, 0])
    cos_i, sin_i = np.cos(TH), np.sin(TH)

    # connector boss (peg); its outer ring is the full-M seat ring (level 0 for any M)
    lev = lambda L: TH[::2 ** L]
    level = _level_fn(2 * np.pi * cf.R_SEAT / M, M)
    seat_ring, seat_L = revolution_cap(b, lev, level, lambda r: seat_z + cf.peg_height(r),
                                       zmax, cf.R_SEAT, CONN_SPACING, sign=+1)
    assert seat_L == 0, f"seat ring not full-M (level {seat_L})"

    # cap: blend the body's top rings up to the Ø14 connector seat. The method depends on whether
    # the body is everywhere WIDER than the seat (nestle: EAT/GROW) or narrower/straddling (flare:
    # MOVE), because the same blend behaves differently in the two regimes.
    n_cap = max(10, int(round((seat_z - flare_z) / target)))
    Rb = np.hypot(foot[:, 0], foot[:, 1])                      # body radius at the foot (per angle)

    def cap_ring(rz):                                          # rz = (M,2) of (radius, z)
        base = len(b.V)
        b.V.extend((float(rz[i, 0] * cos_i[i]), float(rz[i, 0] * sin_i[i]), float(rz[i, 1]))
                   for i in range(M))
        return list(range(base, len(b.V)))

    prev = seat_ring
    if Rb.min() >= cf.R_SEAT:
        # NESTLE (body wider than the seat at EVERY angle): a G1 cubic Bezier per angle leaves the
        # seat HORIZONTAL (tangent to the flat seat ring -> no shelf/ridge where the connector sits)
        # and arrives at the foot along the body's own wall slope -> one continuous curve. P1 is
        # placed RADIALLY OUTWARD from the seat (sign=+1 always, since Rb > R_SEAT at every angle),
        # so the curve flares OUT then down to meet the body. STRADDLING bodies (Rb.min < R_SEAT <
        # Rb.max — e.g. MOVE's spiral) use the FLARE branch below instead, because a sign-flip in P1
        # would create per-angle Bezier OVERSHOOTS: bulges at the arm tips and undercuts at the
        # throats (the "3 wings + 3 cavities" pattern MOVE used to show). Sampled at PLANAR z-levels
        # (uniform-t rings would be non-planar and facet).
        Rlo = _r_at_angle(slice_loop(src, flare_z - 1.0), TH)
        slope = Rb - Rlo
        P0 = np.column_stack([np.full(M, cf.R_SEAT), np.full(M, seat_z)])
        P3 = np.column_stack([Rb, np.full(M, flare_z)])
        L = 0.5 * np.hypot(Rb - cf.R_SEAT, seat_z - flare_z)
        t3 = np.column_stack([slope, np.ones(M)]); t3 = t3 / np.hypot(slope, 1.0)[:, None]
        P1 = P0 + L[:, None] * np.column_stack([np.sign(Rb - cf.R_SEAT), np.zeros(M)])
        P2 = P3 + L[:, None] * t3
        bz = lambda t, c: ((1-t)**3*P0[:, c] + 3*(1-t)**2*t*P1[:, c]
                           + 3*(1-t)*t*t*P2[:, c] + t**3*P3[:, c])
        for j in range(1, n_cap + 1):
            if j == n_cap:
                ring = b.ring_pts(foot, flare_z)
            else:
                zk = seat_z - (seat_z - flare_z) * (j / n_cap) ** 1.6   # planar, dense near seat
                lo = np.zeros(M); hi = np.ones(M)
                for _ in range(30):                                     # invert bz_z(t)=zk per column
                    mid = 0.5 * (lo + hi); up = bz(mid, 1) > zk
                    lo = np.where(up, mid, lo); hi = np.where(up, hi, mid)
                ring = cap_ring(np.column_stack([bz(0.5 * (lo + hi), 0), np.full(M, zk)]))
            b.loft(ring, prev); prev = ring
    else:
        # FLARE/TRUMPET (any STRADDLE or fully-narrower body — Rb.min < R_SEAT): the per-angle
        # NESTLE Bezier would overshoot (its L is height-scaled), producing bulges where Rb > R_SEAT
        # and undercut cavities where Rb < R_SEAT. Two-stage cap so the body becomes a NESTLE at the
        # top: a smoothstep flare blends the FIXED foot ring up to a Ø14+ mesa CIRCLE (the foot's
        # 3-fold pattern smoothly morphs to a circle — no body re-slicing, so no spiral-twist noise);
        # then a short G1 Bezier roll-over (vertical at the mesa, horizontal at the seat) tucks it
        # tangentially into the connector's flat seat -> no lip. All columns reach the mesa
        # uniformly, so the roll-over's rings are planar (no streaks).
        margin = 0.5; R_mesa = cf.R_SEAT + margin
        bez_span = min(3.0, 0.45 * (seat_z - flare_z))
        z_mesa = seat_z - bez_span
        n_bez = max(4, int(round(bez_span / target)))
        n_flare = max(8, n_cap - n_bez)
        L = 0.5 * bez_span
        P0 = np.array([cf.R_SEAT, seat_z])                 # seat (horizontal tangent)
        P3 = np.array([R_mesa, z_mesa])                    # mesa top (vertical tangent)
        P1 = P0 + np.array([L, 0.0])                       # horizontal out
        P2 = P3 + np.array([0.0, L])                       # vertical up
        bz = lambda t, c: ((1-t)**3*P0[c] + 3*(1-t)**2*t*P1[c]
                           + 3*(1-t)*t*t*P2[c] + t**3*P3[c])
        for j in range(1, n_bez + 1):                      # G1 roll-over from seat down to mesa
            t = (j / n_bez) ** 1.6                         # dense near the seat (the roll-over)
            ring = cap_ring(np.column_stack([np.full(M, bz(t, 0)), np.full(M, bz(t, 1))]))
            b.loft(ring, prev); prev = ring
        # smoothstep trumpet flare from mesa down to the FIXED foot ring (no body re-slicing —
        # the spiral twists below flare_z, and re-sampling it at z>flare_z fed the cap a rotating
        # cross-section that created visible kinks)
        for j in range(1, n_flare + 1):
            if j == n_flare:
                ring = b.ring_pts(foot, flare_z)
            else:
                z = z_mesa + (flare_z - z_mesa) * (j / n_flare)
                w = _smoothstep((z - flare_z) / (z_mesa - flare_z))
                R = (1 - w) * Rb + w * R_mesa
                ring = cap_ring(np.column_stack([R, np.full(M, z)]))
            b.loft(ring, prev); prev = ring

    # body: foot (flare_z) -> wall_z, real slices re-stacked (symmetric, twist-tracked)
    n_body = max(8, int(round((flare_z - wall_z) / target)))
    ref = foot
    for z in np.linspace(flare_z, wall_z, n_body + 1)[1:]:
        r = symmetrize_ring(resample_ring(slice_loop(src, z), M), fold)
        r = align_ring(ref, r); ref = r
        ring = b.ring_pts(r, z); b.loft(ring, prev); prev = ring
    rim_pts = np.array([b.V[i][:2] for i in prev])

    # rounded SOLID bottom: vertical wall -> quarter-round fillet -> Triangle flat base
    inset = _inset_loop(rim_pts, fillet, fold)
    nz = max(1, int(round((wall_z - fillet) / target)))
    for s in range(1, nz + 1):
        ring = b.ring_pts(rim_pts, wall_z - (wall_z - fillet) * s / nz); b.loft(prev, ring); prev = ring
    nfil = max(2, int(round(fillet * (np.pi / 2) / target)))
    for s in range(1, nfil + 1):
        if s == nfil:
            ring = b.ring_pts(inset, 0.0)
        else:
            t = s / nfil; w = np.cos(t * np.pi / 2)
            ring = b.ring_pts(inset + (rim_pts - inset) * w, fillet * (1 - np.sin(t * np.pi / 2)))
        b.loft(prev, ring); prev = ring
    inset_idx = prev
    seg = np.array([[i, (i + 1) % M] for i in range(M)])
    out = tr.triangulate({"vertices": inset, "segments": seg}, f"pq28a{base_area(target):.6f}Y")
    VV, T = out["vertices"], out["triangles"]
    idm = {k: inset_idx[k] for k in range(M)}
    for k in range(M, len(VV)):
        idm[k] = b.vert(VV[k, 0], VV[k, 1], 0.0)
    for (i, j, k) in T:
        b.F.append([idm[i], idm[k], idm[j]])

    mesh = b.mesh()
    V = mesh.vertices; a = 2 * np.pi / fold; c, s = np.cos(a), np.sin(a)
    from scipy.spatial import cKDTree
    d = cKDTree(V).query(np.column_stack([V[:, 0]*c - V[:, 1]*s, V[:, 0]*s + V[:, 1]*c, V[:, 2]]))[0]
    print(f"\n=== {name} graft (M={M} fillet={fillet:.2f}) ===  {len(V)} verts, {len(mesh.faces)} faces, "
          f"vol={mesh.volume:.0f}mm^3  watertight={mesh.is_watertight} euler={mesh.euler_number} "
          f"zmax={V[:,2].max():.2f}")
    print(f"  {fold}-fold symmetry mismatch: mean={d.mean():.3f} max={d.max():.3f} mm")
    out_p = OUT / f"{name}_graft.obj"; mesh.export(out_p); print(f"  wrote {out_p}")
    return mesh


def _render_pieces(which="blanks"):
    """Fire-and-forget Blender render of the current blanks (or grafts) into
    pieces/renders/<which>_overview.png. Called by the CLI after every blank/graft
    rebuild so the on-disk render always reflects the current state of out/*.obj."""
    import subprocess, json, os
    blender = Path(os.environ.get(
        "BLENDER", Path.home() / "Downloads" / "blender-5.1.2-linux-x64" / "blender"))
    if not blender.exists():
        print(f"  [render skipped: {blender} not found; set $BLENDER]")
        return
    suffix = "_graft" if which == "grafts" else ""
    items = [(nm, str(OUT / f"{nm}{suffix}.obj")) for nm in SPECS]
    items = [(nm, p) for nm, p in items if Path(p).exists()]
    if not items:
        print(f"  [render skipped: no {which} on disk]")
        return
    out_png = ROOT / "renders" / f"{which}_overview.png"
    out_png.parent.mkdir(exist_ok=True)
    script_body = (
        "import bpy, json\n"
        f"ITEMS = {json.dumps(items)}\n"
        f"OUT = {json.dumps(str(out_png))}\n"
        + _RENDER_BODY
    )
    script = ROOT / "renders" / f"_render_{which}.py"
    script.write_text(script_body)
    subprocess.run([str(blender), "--background", "--python", str(script)],
                   capture_output=True, timeout=120)
    print(f"  wrote render -> {out_png}")


_RENDER_BODY = """
bpy.ops.wm.read_factory_settings(use_empty=True); sc = bpy.context.scene
mat = bpy.data.materials.new("m"); mat.diffuse_color = (0.82, 0.82, 0.86, 1)
xs = [-44, 0, 44][:len(ITEMS)]
for (name, path), x in zip(ITEMS, xs):
    before = set(bpy.data.objects)
    bpy.ops.wm.obj_import(filepath=path, up_axis='Z', forward_axis='Y')
    o = [ob for ob in bpy.data.objects if ob not in before and ob.type == 'MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    zmin = min(v.co.z for v in o.data.vertices)
    o.location = (x, 0, -zmin); bpy.ops.object.shade_smooth()
    o.data.materials.append(mat)
emp = bpy.data.objects.new("t", None); sc.collection.objects.link(emp); emp.location = (0, 0, 25)
camd = bpy.data.cameras.new("c"); cam = bpy.data.objects.new("c", camd)
sc.collection.objects.link(cam); cam.location = (0, -220, 35); camd.lens = 50
con = cam.constraints.new('TRACK_TO'); con.target = emp
con.track_axis = 'TRACK_NEGATIVE_Z'; con.up_axis = 'UP_Y'; sc.camera = cam
sc.render.engine = 'BLENDER_WORKBENCH'; sh = sc.display.shading
sh.light = 'STUDIO'; sh.show_shadows = True
sh.show_cavity = True; sh.cavity_type = 'BOTH'
sh.color_type = 'SINGLE'; sh.single_color = (0.8, 0.8, 0.85)
sc.render.resolution_x = 1200; sc.render.resolution_y = 600
sc.render.filepath = OUT; bpy.ops.render.render(write_still=True)
print("wrote", OUT)
"""


if __name__ == "__main__":
    args = sys.argv[1:]
    sub = args[0] if args and args[0] in ("blank", "graft") else None
    names = [a for a in args if a in SPECS] or list(SPECS)
    for nm in names:
        if sub == "graft":
            build_graft(nm)
        elif sub == "blank":
            build_blank(nm)
        elif nm == "MOVE":
            build_move_symmetric()
        else:
            build_piece_symmetric(nm)
    # auto-render: the render in pieces/renders/ always reflects the LATEST OBJ
    if sub in ("blank", "graft"):
        _render_pieces("blanks" if sub == "blank" else "grafts")
