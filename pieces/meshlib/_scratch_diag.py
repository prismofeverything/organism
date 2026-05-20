"""fast diagnostic: one remesh pass on a wedge, report worst triangles."""
import sys, time
import numpy as np
from scipy.interpolate import LinearNDInterpolator, NearestNDInterpolator
from domain import load_region, round_corners
from mesh2d import _infer_boundary
from field import membrane_field, smooth_scalar
from profile import height
from symmetry import symmetrize_region, wedge_mesh_star, replicate2d, wedge_boundary_star_3d
from remesh_graph import remesh_graph, min_angles_3d, edge_len_3d, _unique_edges, worst_tris
import build


def make_Hfn(V2, H):
    li = LinearNDInterpolator(V2, H); ne = NearestNDInterpolator(V2, H)
    def f(P):
        P = np.atleast_2d(np.asarray(P, float)); z = li(P); nan = np.isnan(z)
        if nan.any(): z[nan] = ne(P[nan])
        return z
    return f


def solve_full(V2, T, zmax, shape, wall, sm):
    bnd = _infer_boundary(V2, T); u = membrane_field(V2, T, bnd)
    H = height(u, zmax, shape, wall)
    if sm: H = smooth_scalar(V2, T, H, bnd, iters=sm)
    return H * (zmax / H.max())


name = sys.argv[1] if len(sys.argv) > 1 else "GROW"
TARGET = 0.85
svg, zmax, shape, wall, sm = build.SPECS[name]; fold = build.FOLD[name]
reg = round_corners(symmetrize_region(load_region(build.ROOT/"inputs"/svg, name), fold), radius=build.CORNER_RADIUS)
Vw, Tw, _ = wedge_mesh_star(reg, fold, edge=0.6)
Vc, Tc, _ = replicate2d(Vw, Tw, fold)
Hfn = make_Hfn(Vc, solve_full(Vc, Tc, zmax, shape, wall, sm))

t0 = time.time()
Vb, seg = wedge_boundary_star_3d(reg, fold, Hfn, TARGET)
Vwq, Twq = remesh_graph(Vb, seg, Hfn, TARGET, verbose=True)
ang = min_angles_3d(Vwq, Twq, Hfn); L = edge_len_3d(Vwq, _unique_edges(Twq), Hfn)
print(f"WEDGE {len(Vwq)}v {len(Twq)}t  min3Dang {ang.min():.2f}  maxedge {L.max():.2f}  "
      f"p95/med {np.percentile(L,95)/np.median(L):.2f}  ({time.time()-t0:.1f}s)")
print("  worst triangles (boundary loop = first %d verts; center = vert 0):" % len(Vb))
print(worst_tris(Vwq, Twq, len(Vb), Hfn, 10))
print(f"  angle pctls: 1%={np.percentile(ang,1):.1f} 5%={np.percentile(ang,5):.1f} "
      f"median={np.percentile(ang,50):.1f}")
print(f"  tris < 15deg: {(ang<15).sum()}/{len(ang)};  < 5deg: {(ang<5).sum()}")
