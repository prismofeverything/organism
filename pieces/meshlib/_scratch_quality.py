"""scratch: prove the graph-domain frozen-cut remesh on a piece wedge."""
import sys
import numpy as np
from scipy.interpolate import LinearNDInterpolator, NearestNDInterpolator
import trimesh

from domain import load_region, round_corners
from mesh2d import _infer_boundary
from field import membrane_field, smooth_scalar
from profile import height
from solid import build_solid
from symmetry import (symmetrize_region, wedge_mesh_star, replicate2d,
                      wedge_boundary_star_3d)
from remesh_graph import remesh_graph, min_angles_3d, edge_len_3d, _unique_edges
from invariants import Spec, validate
import build


def make_Hfn(V2, H):
    li = LinearNDInterpolator(V2, H); ne = NearestNDInterpolator(V2, H)
    def f(P):
        P = np.atleast_2d(np.asarray(P, float))
        z = li(P); nan = np.isnan(z)
        if nan.any():
            z[nan] = ne(P[nan])
        return z
    return f


def solve_full(V2, T, zmax, shape, wall, sm_iters):
    bnd = _infer_boundary(V2, T)
    u = membrane_field(V2, T, bnd)
    H = height(u, zmax, shape, wall)
    if sm_iters:
        H = smooth_scalar(V2, T, H, bnd, iters=sm_iters)
    H *= zmax / H.max()
    Hfloor = height(u, zmax, "parabola", wall)
    return H, Hfloor


name = sys.argv[1] if len(sys.argv) > 1 else "GROW"
TARGET = 0.85
svg, zmax, shape, wall, sm_iters = build.SPECS[name]
fold = build.FOLD[name]
reg = round_corners(symmetrize_region(load_region(build.ROOT / "inputs" / svg, name), fold),
                    radius=build.CORNER_RADIUS)

# initial (coarse) field -> metric
Vw, Tw, _ = wedge_mesh_star(reg, fold, edge=0.6)
Vc, Tc, _ = replicate2d(Vw, Tw, fold)
Hc, _ = solve_full(Vc, Tc, zmax, shape, wall, sm_iters)
Hfn = make_Hfn(Vc, Hc)

Vf = Tf = Hf = Ff = None
for outer in range(3):
    Vb, seg = wedge_boundary_star_3d(reg, fold, Hfn, TARGET)
    Vwq, Twq = remesh_graph(Vb, seg, Hfn, TARGET, verbose=(outer == 2))
    Vf, Tf, _ = replicate2d(Vwq, Twq, fold)
    Hf, Ff = solve_full(Vf, Tf, zmax, shape, wall, sm_iters)
    ang = min_angles_3d(Vf, Tf, make_Hfn(Vf, Hf))
    L = edge_len_3d(Vf, _unique_edges(Tf), make_Hfn(Vf, Hf))
    print(f"outer {outer}: wedge {len(Vwq)}v -> full {len(Vf)}v {len(Tf)}t | "
          f"3D min ang {ang.min():.1f}  max edge {L.max():.2f}  "
          f"p95/med {np.percentile(L,95)/np.median(L):.2f}")
    Hfn = make_Hfn(Vf, Hf)

V3, F = build_solid(Vf, Tf, Hf, edge=0.6)
mesh = trimesh.Trimesh(vertices=V3, faces=F, process=True)
trimesh.repair.fix_normals(mesh)
spec = Spec(name, zmax, shape, wall, build.TARGET_EDGE, fold=fold)
print(f"\n=== {name} (symmetric + graph remesh) === {len(mesh.vertices)} v, {len(mesh.faces)} f, "
      f"vol={mesh.volume:.0f}, watertight={mesh.is_watertight}")
validate(mesh, reg, spec, ideal=(Hf, Ff))
