"""
Invariants — standalone (trimesh/numpy, no Blender) validation oracle.

Each check is a function (mesh, region, spec) -> Result, collected in INVARIANTS
and run by validate(). This is the regression gate: a recipe is "good" iff every
FAIL-severity invariant passes. Run from build.py or pytest.

The six original invariants, re-expressed for the height-field method:
  1. manifold      — watertight, consistent winding, no degenerate faces
  2. silhouette    — top-down projection == target region R
  3. no_overhang   — every vertex projects inside R (moldable; pinpoints offenders)
  4. uniform_tris  — all triangles, edge lengths clustered, min angle healthy
  5. no_long_edges — every edge <= 1.5 * target_edge
  6. smooth        — no sharp body-interior dihedrals (rim/apex exempt)

(Side-profile is guaranteed by construction via the membrane field + transfer
function; it is checked at build time, not re-derived from the mesh here.)
"""
from __future__ import annotations
from dataclasses import dataclass
import numpy as np
from matplotlib.path import Path as MplPath


@dataclass
class Spec:
    name: str
    z_max: float
    shape: str = "parabola"
    wall_frac: float = 0.0
    target_edge: float = 1.0
    fold: int = 1                      # rotational symmetry order (1 = none)


@dataclass
class Result:
    name: str
    passed: bool
    detail: str = ""
    severity: str = "FAIL"


# ----------------------------------------------------------------- helpers
def _region_path(region):
    return MplPath(region.outer)


def _dist_to_outline(pts, ring):
    """Min distance from each point to the closed polyline `ring`."""
    a = ring
    ab = np.roll(ring, -1, axis=0) - a
    L2 = np.einsum("ij,ij->i", ab, ab) + 1e-12
    d = np.empty(len(pts))
    for k, p in enumerate(pts):
        t = np.clip(np.einsum("ij,ij->i", p - a, ab) / L2, 0, 1)
        proj = a + ab * t[:, None]
        d[k] = np.sqrt(((p - proj) ** 2).sum(1).min())
    return d


def _near_boundary(mids_xy, region, band=2.5):
    """True for creases within `band` mm of the silhouette boundary — the rim /
    shoulder / arm-edge / corner region, where surface sharpness reflects the
    SVG's edge features (intended) rather than a body defect. The wide body
    interior stays checked. (Boundary is finely resampled, so nearest-vertex
    distance approximates distance-to-outline.)"""
    from scipy.spatial import cKDTree
    pts = np.vstack(list(region.all_rings))
    return cKDTree(pts).query(np.asarray(mids_xy))[0] < band


# ----------------------------------------------------------------- invariants
def inv_manifold(mesh, region, spec):
    tiny = int((mesh.area_faces < 1e-4).sum())
    ok = bool(mesh.is_watertight and mesh.is_winding_consistent and tiny == 0)
    return Result("manifold", ok,
                  f"watertight={mesh.is_watertight} winding={mesh.is_winding_consistent} "
                  f"euler={mesh.euler_number} tiny_faces={tiny}")


def inv_silhouette(mesh, region, spec, grid=160, tol_frac=0.02):
    x0, y0 = region.outer.min(0) - 1
    x1, y1 = region.outer.max(0) + 1
    gx = np.linspace(x0, x1, grid)
    gy = np.linspace(y0, y1, grid)
    GX, GY = np.meshgrid(gx, gy)
    pts = np.column_stack([GX.ravel(), GY.ravel()])
    inside = _region_path(region).contains_points(pts).reshape(grid, grid)

    # rasterize the mesh's projected triangles -> coverage
    cov = np.zeros((grid, grid), bool)
    tris = mesh.vertices[mesh.faces][:, :, :2]
    for (p0, p1, p2) in tris:
        mnx, mny = np.minimum.reduce([p0, p1, p2])
        mxx, mxy = np.maximum.reduce([p0, p1, p2])
        i0, i1 = np.searchsorted(gx, mnx), np.searchsorted(gx, mxx) + 1
        j0, j1 = np.searchsorted(gy, mny), np.searchsorted(gy, mxy) + 1
        if i1 <= i0 or j1 <= j0:
            continue
        X, Y = np.meshgrid(gx[i0:i1], gy[j0:j1])
        d = (p1[1] - p2[1]) * (p0[0] - p2[0]) + (p2[0] - p1[0]) * (p0[1] - p2[1])
        if abs(d) < 1e-12:
            continue
        a = ((p1[1] - p2[1]) * (X - p2[0]) + (p2[0] - p1[0]) * (Y - p2[1])) / d
        b = ((p2[1] - p0[1]) * (X - p2[0]) + (p0[0] - p2[0]) * (Y - p2[1])) / d
        c = 1 - a - b
        cov[j0:j1, i0:i1] |= (a >= -1e-9) & (b >= -1e-9) & (c >= -1e-9)

    disagree = (cov != inside).sum() / inside.size
    return Result("silhouette", disagree <= tol_frac,
                  f"{100*disagree:.2f}% of cells disagree (tol {100*tol_frac:.0f}%)")


def inv_no_overhang(mesh, region, spec, tol=0.3):
    xy = mesh.vertices[:, :2]
    inside = _region_path(region).contains_points(xy)
    out = np.where(~inside)[0]
    bad = 0
    worst = 0.0
    if len(out):
        d = _dist_to_outline(xy[out], region.outer)
        bad = int((d > tol).sum())
        worst = float(d.max())
    return Result("no_overhang", bad == 0,
                  f"{bad} verts >{tol}mm outside R (worst {worst:.2f}mm)")


def inv_uniform_tris(mesh, region, spec, spread_tol=3.0, angle_min=15.0):
    L = mesh.edges_unique_length
    med, p95 = np.median(L), np.percentile(L, 95)
    ratio = p95 / med if med > 0 else 1e9
    ang = _min_angles(mesh).min()
    ok = ratio <= spread_tol and ang >= angle_min
    return Result("uniform_tris", bool(ok),
                  f"p95/median={ratio:.2f} (median {med:.2f}mm), min angle {ang:.1f}deg "
                  f"(tol ratio<={spread_tol:.0f}, angle>={angle_min:.0f})")


def inv_no_long_edges(mesh, region, spec):
    limit = 1.5 * spec.target_edge
    L = mesh.edges_unique_length
    n = int((L > limit).sum())
    return Result("no_long_edges", n == 0,
                  f"{n}/{len(L)} edges > {limit:.1f}mm (worst {L.max():.2f}mm)")


def inv_smooth(mesh, region, spec, min_radius=0.5):
    """A 'crease' is a fold sharper than `min_radius` mm, measured as the local
    radius of curvature = (shared-edge length) / (dihedral angle). This is
    resolution-independent and distinguishes intended organic curvature (large
    radius) from construction seams (tiny radius). Rim and apex are exempt."""
    z = mesh.vertices[:, 2]
    zmin, zmax = z.min(), z.max(); span = zmax - zmin + 1e-9
    ev = mesh.face_adjacency_edges
    ov = mesh.face_adjacency_unshared            # opposite vertex of each face
    elen = np.linalg.norm(mesh.vertices[ev[:, 0]] - mesh.vertices[ev[:, 1]], axis=1)
    radius = elen / (mesh.face_adjacency_angles + 1e-9)
    ez = z[ev]
    # exempt the rim, the apex, and CREST/SPINE folds. A convex-up fold (both
    # opposite verts below the shared edge) is a ridge crest — the spiral's
    # apex generalized from a point to a curve; allowed to be sharp like a peak.
    is_rim = (ez.max(1) - zmin) < 0.05 * span
    is_apex = (zmax - ez.min(1)) < 0.03 * span
    is_crest = z[ov].max(1) < ez.max(1)
    is_edge = _near_boundary(mesh.vertices[ev].mean(1)[:, :2], region)
    body = ~is_rim & ~is_apex & ~is_crest & ~is_edge
    crease = body & (radius < min_radius)
    rmin = radius[body].min() if body.any() else 9.9
    return Result("smooth", int(crease.sum()) == 0,
                  f"{int(crease.sum())} interior-body folds < {min_radius}mm radius "
                  f"(tightest {rmin:.2f}mm; rim/apex/crest/edge exempt)")


def _min_angles(mesh):
    P = mesh.vertices[mesh.faces]
    out = []
    for i in range(3):
        a = P[:, (i + 1) % 3] - P[:, i]
        b = P[:, (i + 2) % 3] - P[:, i]
        cos = np.einsum("ij,ij->i", a, b) / (
            np.linalg.norm(a, axis=1) * np.linalg.norm(b, axis=1) + 1e-12)
        out.append(np.degrees(np.arccos(np.clip(cos, -1, 1))))
    return np.min(np.array(out), axis=0)


def inv_additive(mesh, region, spec, ideal=None, tol=1.0):
    """Deviations from the spec'd profile must be ADDITIVE: the built height field
    may sit ABOVE the ideal profile (inflate) but must not be carved BELOW it.
    `ideal` = (H_actual, H_floor) sampled at the build vertices; checks
    H_actual >= H_floor - tol everywhere."""
    if ideal is None:
        return Result("additive", True, "no ideal profile supplied", severity="WARN")
    H_actual, H_floor = (np.asarray(a, float) for a in ideal)
    deficit = H_floor - H_actual
    n = int((deficit > tol).sum())
    worst = float(deficit.max()) if len(deficit) else 0.0
    return Result("additive", n == 0,
                  f"{n} verts carved >{tol}mm below the spec profile (worst {worst:.2f}mm)")


def inv_symmetry(mesh, region, spec, tol=0.3):
    """Full N-fold symmetry of the TRIANGULATION: rotating the mesh by 2*pi/fold
    must map its vertex set onto itself (every rotated vertex lands on a vertex).
    A symmetric base stays symmetric under symmetric sculpting; asymmetry would
    compound. fold=1 -> skipped."""
    fold = getattr(spec, "fold", 1)
    if not fold or fold < 2:
        return Result("symmetry", True, "no fold specified", severity="WARN")
    from scipy.spatial import cKDTree
    V = mesh.vertices
    a = 2 * np.pi / fold
    c, s = np.cos(a), np.sin(a)
    Vr = np.column_stack([V[:, 0]*c - V[:, 1]*s, V[:, 0]*s + V[:, 1]*c, V[:, 2]])
    d = cKDTree(V).query(Vr)[0]
    n = int((d > tol).sum())
    return Result("symmetry", n == 0,
                  f"{n}/{len(V)} verts don't map onto a vertex under 2pi/{fold} "
                  f"rotation (max {d.max():.2f}mm)")


INVARIANTS = [inv_manifold, inv_silhouette, inv_no_overhang, inv_uniform_tris,
              inv_no_long_edges, inv_smooth, inv_additive, inv_symmetry]


def validate(mesh, region, spec, ideal=None, verbose=True):
    results = []
    for inv in INVARIANTS:
        try:
            kw = {"ideal": ideal} if inv is inv_additive else {}
            results.append(inv(mesh, region, spec, **kw))
        except Exception as exc:
            results.append(Result(inv.__name__, False, f"ERROR: {exc}"))
    ok = all(r.passed for r in results if r.severity == "FAIL")
    if verbose:
        print(f"  --- invariants [{spec.name}] ---")
        for r in results:
            print(f"    [{'PASS' if r.passed else r.severity:>4}] {r.name:<14} {r.detail}")
        print(f"    {'ALL CLEAR' if ok else 'FAILED'} "
              f"({sum(r.passed for r in results)}/{len(results)})")
    return ok, results
