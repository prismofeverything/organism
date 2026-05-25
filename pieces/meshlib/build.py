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
from symmetry import symmetrize_region, wedge_mesh_star, wedge_mesh_metric, replicate2d
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


STAR = {"EAT", "GROW"}   # star-shaped -> symmetric pipeline ready; MOVE = spiral (WIP)

if __name__ == "__main__":
    for nm in (sys.argv[1:] or list(SPECS)):
        if nm in STAR:
            build_piece_symmetric(nm)
        else:
            build_piece(nm, symmetric_shape=True)   # MOVE: symmetric SHAPE + quality (7/8)
