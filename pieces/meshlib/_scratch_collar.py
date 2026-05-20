"""scratch: collar+cap top + independent flat bottom, metric outer-loop."""
import sys, time
import numpy as np
from scipy.interpolate import LinearNDInterpolator, NearestNDInterpolator
import trimesh
from domain import load_region, round_corners
from mesh2d import _infer_boundary
from field import membrane_field, smooth_scalar
from profile import height
from solid import build_solid_split
from symmetry import symmetrize_region, wedge_mesh_star, replicate2d
from collar import build_wedge_collar_cap, build_bottom_wedge
from invariants import Spec, validate
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
    H *= zmax / H.max()
    return H, height(u, zmax, "parabola", wall)


def min_ang(V3, F):
    P = V3[F]; out = []
    for i in range(3):
        a = P[:, (i+1) % 3] - P[:, i]; b = P[:, (i+2) % 3] - P[:, i]
        cs = np.einsum("ij,ij->i", a, b)/(np.linalg.norm(a, axis=1)*np.linalg.norm(b, axis=1)+1e-12)
        out.append(np.degrees(np.arccos(np.clip(cs, -1, 1))))
    return np.min(out, axis=0)


name = sys.argv[1] if len(sys.argv) > 1 else "GROW"
TARGET = 0.7
svg, zmax, shape, wall, sm = build.SPECS[name]; fold = build.FOLD[name]
reg = round_corners(symmetrize_region(load_region(build.ROOT/"inputs"/svg, name), fold), radius=build.CORNER_RADIUS)
Vw, Tw, _ = wedge_mesh_star(reg, fold, edge=0.6)
Vc0, Tc0, _ = replicate2d(Vw, Tw, fold)
Hc, _ = solve_full(Vc0, Tc0, zmax, shape, wall, sm)
Hfn = make_Hfn(Vc0, Hc)

ring0 = None
Vt = Tt = Ht = Ff = None
for outer in range(3):
    t0 = time.time()
    Vtw, Ttw, ring0 = build_wedge_collar_cap(reg, fold, Hfn, wall*zmax, zmax, TARGET)
    Vt, Tt, _ = replicate2d(Vtw, Ttw, fold)
    Ht, Ff = solve_full(Vt, Tt, zmax, shape, wall, sm)
    print(f"outer {outer}: top wedge {len(Vtw)}v -> full {len(Vt)}v {len(Tt)}t ({time.time()-t0:.1f}s)")
    Hfn = make_Hfn(Vt, Ht)

Vbw, Tbw = build_bottom_wedge(fold, ring0, TARGET)
Vb, Tb, _ = replicate2d(Vbw, Tbw, fold)
from solid import boundary_loops
print("top rim verts", len(boundary_loops([list(t) for t in Tt])[0]),
      " bottom rim verts", len(boundary_loops([list(t) for t in Tb])[0]))
V3, F = build_solid_split(Vt, Tt, Ht, Vb, Tb, edge=TARGET)
mesh = trimesh.Trimesh(vertices=V3, faces=F, process=True)
trimesh.repair.fix_normals(mesh)
spec = Spec(name, zmax, shape, wall, build.TARGET_EDGE, fold=fold)
print(f"=== {name} === {len(mesh.vertices)}v {len(mesh.faces)}f vol={mesh.volume:.0f} watertight={mesh.is_watertight}")
ok, _ = validate(mesh, reg, spec, ideal=(Ht, Ff))

ang = min_ang(mesh.vertices, mesh.faces); cen = mesh.vertices[mesh.faces].mean(1)
for ti in np.argsort(ang)[:2]:
    f = mesh.faces[ti]; vs = mesh.vertices[f]
    print(f"   worst ang {ang[ti]:5.2f}  verts:")
    for v in vs:
        print(f"       ({v[0]:7.3f},{v[1]:7.3f},{v[2]:7.3f})  r={np.hypot(v[0],v[1]):.3f}")
E = mesh.edges_unique; L = mesh.edges_unique_length
emid = mesh.vertices[E].mean(1)
print("longest edges (len, r, z):")
for ei in np.argsort(L)[::-1][:10]:
    print(f"   len {L[ei]:.2f}  r={np.hypot(emid[ei,0],emid[ei,1]):5.1f}  z={emid[ei,2]:5.1f}")
