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

from domain import load_region, round_corners
from mesh2d import triangulate_region, _infer_boundary
from field import membrane_field, smooth_scalar
from profile import height
from solid import build_solid
from remesh import isotropic_remesh
from invariants import Spec, validate
from symmetry import symmetrize_region, wedge_mesh_star, wedge_mesh_metric, replicate2d

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / "out"; OUT.mkdir(exist_ok=True)
INIT_EDGE = 0.5          # initial lift resolution
REMESH_EDGE = 0.8        # final uniform triangle size
TARGET_EDGE = 1.0        # nominal size for invariants (long-edge limit = 1.5x)
CORNER_RADIUS = 1.5      # round sharp silhouette corners (edge/radius < 45deg)

FOLD = {"EAT": 5, "MOVE": 3, "GROW": 4}   # rotational symmetry order

# name -> (svg, z_max, shape, wall_frac, smooth_iters)
# smooth_iters rounds the wall->dome shoulder; MOVE needs heavy rounding to keep
# its thin spiral crest smooth, then the height is rescaled back up to z_max.
SPECS = {
    "EAT":  ("eat.svg",  48.0, "hemisphere", 7 / 8,  40),
    "MOVE": ("move.svg", 60.0, "shallow",    0.0,     0),  # tall; arms RAISED (additive); crest=spine
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


def build_piece(name):
    svg, zmax, shape, wall, sm_iters = SPECS[name]
    reg = load_region(ROOT / "inputs" / svg, name)
    reg = round_corners(reg, radius=CORNER_RADIUS)

    V, T, bnd = triangulate_region(reg, edge=INIT_EDGE)
    u = membrane_field(V, T, bnd)
    H = height(u, zmax, shape, wall)
    if sm_iters:
        H = smooth_scalar(V, T, H, bnd, iters=sm_iters)    # fillet the wall->dome shoulder
    H *= zmax / H.max()                                    # rescale to exact target height
    H_floor = height(u, zmax, "parabola", wall)            # additive floor: never carve below
    V3, F = build_solid(V, T, H, edge=INIT_EDGE)
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


def build_piece_symmetric(name):
    """Exactly N-fold symmetric build. Mesh ONE wedge with matched radial cuts,
    replicate+weld into a full symmetric 2D mesh, then solve the membrane field
    on the FULL mesh. The FEM operator is rotation-equivariant, so on a symmetric
    mesh the solution is symmetric BY CONSTRUCTION -> the two cuts carry identical
    heights and the lifted solid is exactly N-fold (vs. the per-wedge solve, which
    left the cuts mismatched and broke symmetry). NO global remesh (it would break
    symmetry); triangle quality comes from the frozen-cut remesh stage."""
    svg, zmax, shape, wall, sm_iters = SPECS[name]
    fold = FOLD[name]
    reg = round_corners(symmetrize_region(load_region(ROOT / "inputs" / svg, name), fold),
                        radius=CORNER_RADIUS)

    # one wedge -> replicate+weld -> full, exactly-symmetric 2D mesh
    Vw, Tw, _ = wedge_mesh_star(reg, fold, edge=0.7)
    V2, T, _inv = replicate2d(Vw, Tw, fold)
    bnd = _infer_boundary(V2, T)

    # field on the FULL mesh -> symmetric H (cuts now carry identical heights)
    u = membrane_field(V2, T, bnd)
    H = height(u, zmax, shape, wall)
    if sm_iters:
        H = smooth_scalar(V2, T, H, bnd, iters=sm_iters)
    H *= zmax / H.max()
    H_floor = height(u, zmax, "parabola", wall)

    V3, F = build_solid(V2, T, H, edge=0.6)
    mesh = trimesh.Trimesh(vertices=V3, faces=F, process=True)
    trimesh.repair.fix_normals(mesh)
    spec = Spec(name, zmax, shape, wall, TARGET_EDGE, fold=fold)
    print(f"\n=== {name} (symmetric) ===  {len(mesh.vertices)} verts, {len(mesh.faces)} faces, "
          f"vol={mesh.volume:.0f}mm^3")
    ok, _ = validate(mesh, reg, spec, ideal=(H, H_floor))
    mesh.export(OUT / f"{name}.obj")
    print(f"  wrote {OUT / f'{name}.obj'}  [{'OK' if ok else 'INVARIANTS FAILED'}]")
    return mesh


if __name__ == "__main__":
    for nm in (sys.argv[1:] or list(SPECS)):
        build_piece(nm)
