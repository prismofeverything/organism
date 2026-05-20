"""
================================================================================
 INVARIANTS  —  executable validation of the six piece-design invariants
================================================================================

Each invariant is a FUNCTION ON A MESH that returns a structured pass/fail
result. They are collected in the INVARIANTS registry and run together by
validate(); the runner prints a per-invariant report and an overall ALL CLEAR /
FAILED verdict.

To ADD a new invariant: write a function with the signature

    def invariant_xxx(bm, spec, h_axis) -> InvariantResult

and append it to the INVARIANTS list at the bottom. Nothing else changes — it
will automatically run and appear in the report.

The six invariants (see project_piece_design_method.md for full statements):
  1. manifold        — 0 boundary edges, 0 non-manifold edges, 0 tiny faces
  2. silhouette      — top-down projection == target polygon interior
  3. profile         — max-radius vs height follows the chosen shape curve
  4. uniform_tris    — triangle edge lengths cluster around target_edge
  5. smooth          — no sharp dihedrals on the body interior (rim/apex exempt)
  6. no_long_edges   — every edge <= 1.5 * target_edge

Usage (standalone):
    blender --background --python invariants.py -- EAT.obj
    blender --background --python invariants.py            # all four pieces

Usage (imported):
    from invariants import PieceSpec, validate
    ok, results = validate(bm, spec)
================================================================================
"""
import math
from dataclasses import dataclass, field

try:
    import bmesh
    from mathutils import Vector
    from mathutils.bvhtree import BVHTree
except ImportError:
    bmesh = None  # allows import outside Blender for inspection


# ------------------------------------------------------------------------------
# Spec: the piece's INTENDED shape — invariants compare the mesh against this.
# ------------------------------------------------------------------------------
@dataclass
class PieceSpec:
    name: str
    z_max: float                       # body height (mm)
    shape: str = "parabola"            # parabola | hemisphere | shallow | cosine
    cyl_top: float = 0.0               # fraction of height that is vertical wall
    fold: int = 1                      # rotational symmetry (5 EAT, 3 MOVE, 4 GROW)
    target_edge: float = 1.0           # target triangle edge length (mm)
    polygon: list = field(default_factory=list)  # [(x, y), ...] target footprint

    def scale_at_h(self, h):
        """Intended radial scale at height fraction h in [0, 1] — must match the
        scale_at_h used by the builders in pieces_v2.py."""
        h = max(0.0, min(1.0, h))
        if self.shape == "cylinder":   return 1.0   # constant radius (FOOD disc)
        if h <= self.cyl_top:
            return 1.0
        t = (h - self.cyl_top) / max(1e-6, 1.0 - self.cyl_top)
        if self.shape == "parabola":   return math.sqrt(1 - t)
        if self.shape == "hemisphere": return math.sqrt(max(0, 1 - t * t))
        if self.shape == "shallow":    return (1 - t) ** 0.25
        if self.shape == "cosine":     return math.cos(t * math.pi / 2)
        raise ValueError(f"unknown shape {self.shape!r}")


@dataclass
class InvariantResult:
    name: str
    passed: bool
    detail: str = ""
    severity: str = "FAIL"   # FAIL = must hold; WARN = advisory


# ------------------------------------------------------------------------------
# Shared helpers
# ------------------------------------------------------------------------------
def detect_height_axis(bm):
    """The body's height axis. OBJ round-trip swizzles axes (height often ends
    up on +Y after import), so detect it: the axis with the most positive bias
    (min ~= 0, max ~= z_max) while the others are roughly symmetric about 0."""
    best, best_bias = 1, -1e9
    for ax in range(3):
        vals = [v.co[ax] for v in bm.verts]
        lo, hi = min(vals), max(vals)
        if hi - lo < 1e-3:
            continue
        bias = (lo + hi) / 2
        if bias > best_bias:
            best_bias, best = bias, ax
    return best


def _horizontal_axes(h_axis):
    return [a for a in range(3) if a != h_axis]


def point_in_polygon(px, py, polygon):
    """Crossing-number test for a 2D point against a closed polygon."""
    n = len(polygon)
    inside = False
    j = n - 1
    for i in range(n):
        xi, yi = polygon[i]
        xj, yj = polygon[j]
        if ((yi > py) != (yj > py)) and \
           (px < (xj - xi) * (py - yi) / (yj - yi) + xi):
            inside = not inside
        j = i
    return inside


# ------------------------------------------------------------------------------
# Invariant 1: manifold topology
# ------------------------------------------------------------------------------
def invariant_manifold(bm, spec, h_axis):
    boundary = sum(1 for e in bm.edges if len(e.link_faces) == 1)
    nonmanifold = sum(1 for e in bm.edges if len(e.link_faces) > 2)
    tiny = sum(1 for f in bm.faces if f.calc_area() < 1e-3)
    passed = boundary == 0 and nonmanifold == 0 and tiny == 0
    return InvariantResult(
        "manifold", passed,
        f"boundary={boundary}, non-manifold={nonmanifold}, tiny={tiny}")


# ------------------------------------------------------------------------------
# Invariant 2: top-down silhouette == target polygon interior
# ------------------------------------------------------------------------------
def invariant_silhouette(bm, spec, h_axis, grid=120, tol_frac=0.02):
    """Raycast a grid straight down the height axis. A cell is 'body' if a ray
    hits the mesh. Compare that mask against point-in-polygon of the target
    footprint. Report the fraction of cells that disagree."""
    if not spec.polygon:
        return InvariantResult("silhouette", True,
                               "no target polygon supplied — skipped",
                               severity="WARN")
    h1, h2 = _horizontal_axes(h_axis)
    xs = [v.co[h1] for v in bm.verts]
    ys = [v.co[h2] for v in bm.verts]
    hs = [v.co[h_axis] for v in bm.verts]
    half = max(max(xs) - min(xs), max(ys) - min(ys)) / 2 + 1
    ray_h = max(hs) + 10.0

    bvh = BVHTree.FromBMesh(bm)
    direction = Vector((0, 0, 0)); direction[h_axis] = -1.0

    disagree = 0
    total = 0
    for j in range(grid):
        for i in range(grid):
            a = -half + 2 * half * (i + 0.5) / grid
            b = -half + 2 * half * (j + 0.5) / grid
            origin = Vector((0, 0, 0))
            origin[h1] = a; origin[h2] = b; origin[h_axis] = ray_h
            hit = bvh.ray_cast(origin, direction)[0] is not None
            inside = point_in_polygon(a, b, spec.polygon)
            if hit != inside:
                disagree += 1
            total += 1
    frac = disagree / total
    return InvariantResult(
        "silhouette", frac <= tol_frac,
        f"{100*frac:.1f}% of cells disagree with target (tol {100*tol_frac:.0f}%)")


# ------------------------------------------------------------------------------
# Invariant 3: rotational profile follows the chosen curve
# ------------------------------------------------------------------------------
def invariant_profile(bm, spec, h_axis, n_bands=20, tol_frac=0.05):
    """Bucket vertices by height; in each band the MAX radius from the axis
    should equal r_max * scale_at_h(h) within tolerance. Skips the top few
    bands where discrete sampling necessarily diverges as r -> 0."""
    h1, h2 = _horizontal_axes(h_axis)
    hs = [v.co[h_axis] for v in bm.verts]
    h_min, h_max = min(hs), max(hs)
    span = h_max - h_min
    if span < 1e-6:
        return InvariantResult("profile", False, "degenerate height span")

    # r_max from the base band
    base = [v for v in bm.verts if (v.co[h_axis] - h_min) < span * 0.05]
    r_max = max(math.hypot(v.co[h1], v.co[h2]) for v in base)

    bands = [0.0] * n_bands
    for v in bm.verts:
        t = (v.co[h_axis] - h_min) / span
        bi = min(n_bands - 1, int(t * n_bands))
        r = math.hypot(v.co[h1], v.co[h2])
        if r > bands[bi]:
            bands[bi] = r

    worst = 0.0
    worst_h = 0.0
    # Ignore the top 15% (apex region) where ideal r -> 0 amplifies rel. error
    for bi in range(int(n_bands * 0.85)):
        h = (bi + 0.5) / n_bands
        ideal = r_max * spec.scale_at_h(h)
        if ideal < 0.5:
            continue
        rel = abs(bands[bi] - ideal) / ideal
        if rel > worst:
            worst, worst_h = rel, h
    return InvariantResult(
        "profile", worst <= tol_frac,
        f"max deviation {100*worst:.1f}% at h={worst_h:.2f} (tol {100*tol_frac:.0f}%)")


# ------------------------------------------------------------------------------
# Invariant 4: uniform triangle size
# ------------------------------------------------------------------------------
def invariant_uniform_tris(bm, spec, h_axis, spread_tol=3.0):
    """Edge lengths should cluster around target_edge. Report the ratio of the
    95th-percentile edge to the median; a tight mesh has ratio near 1, a few
    long bridges push it up. (Long edges also caught by invariant 6.)"""
    non_tri = sum(1 for f in bm.faces if len(f.verts) != 3)
    lengths = sorted(e.calc_length() for e in bm.edges)
    if not lengths:
        return InvariantResult("uniform_tris", False, "no edges")
    median = lengths[len(lengths) // 2]
    p95 = lengths[int(len(lengths) * 0.95)]
    ratio = p95 / median if median > 0 else 999
    passed = non_tri == 0 and ratio <= spread_tol
    return InvariantResult(
        "uniform_tris", passed,
        f"non-tri faces={non_tri}, p95/median edge ratio={ratio:.1f} "
        f"(median {median:.2f}mm, tol {spread_tol:.0f})")


# ------------------------------------------------------------------------------
# Invariant 5: smooth by construction (no body-interior creases)
# ------------------------------------------------------------------------------
def invariant_smooth(bm, spec, h_axis, sharp_deg=45.0):
    """Sharp dihedrals between adjacent faces indicate creases. The bottom rim
    (90 deg, intentional) and the apex pole (intentional) are exempt; only
    BODY-INTERIOR sharp dihedrals count as failures."""
    bm.normal_update()
    hs = [v.co[h_axis] for v in bm.verts]
    h_min, h_max = min(hs), max(hs)
    apex_band = 0.02 * (h_max - h_min)
    thresh = math.radians(sharp_deg)

    body_sharp = 0
    for e in bm.edges:
        if len(e.link_faces) != 2:
            continue
        n1, n2 = e.link_faces[0].normal, e.link_faces[1].normal
        d = max(-1.0, min(1.0, n1.dot(n2)))
        if math.acos(d) <= thresh:
            continue
        ev_h = [v.co[h_axis] for v in e.verts]
        is_rim = (max(ev_h) - h_min) < 0.05 and \
                 (abs(n1[h_axis]) > 0.9 or abs(n2[h_axis]) > 0.9)
        is_apex = any((h_max - h) < apex_band for h in ev_h)
        if not is_rim and not is_apex:
            body_sharp += 1
    return InvariantResult(
        "smooth", body_sharp == 0,
        f"body-interior sharp dihedrals (>{sharp_deg:.0f} deg) = {body_sharp}")


# ------------------------------------------------------------------------------
# Invariant 6: no long edges
# ------------------------------------------------------------------------------
def invariant_no_long_edges(bm, spec, h_axis):
    limit = 1.5 * spec.target_edge
    long_edges = [e.calc_length() for e in bm.edges if e.calc_length() > limit]
    worst = max(long_edges) if long_edges else 0.0
    return InvariantResult(
        "no_long_edges", len(long_edges) == 0,
        f"{len(long_edges)} edges > {limit:.1f}mm (worst {worst:.1f}mm)")


# ------------------------------------------------------------------------------
# Registry — append new invariants here and they run automatically.
# ------------------------------------------------------------------------------
INVARIANTS = [
    invariant_manifold,
    invariant_silhouette,
    invariant_profile,
    invariant_uniform_tris,
    invariant_smooth,
    invariant_no_long_edges,
]


def validate(bm, spec, verbose=True):
    """Run every registered invariant. Returns (all_clear, [InvariantResult]).
    all_clear is True iff every FAIL-severity invariant passed."""
    h_axis = detect_height_axis(bm)
    results = []
    for inv in INVARIANTS:
        try:
            results.append(inv(bm, spec, h_axis))
        except Exception as exc:
            results.append(InvariantResult(inv.__name__, False, f"ERROR: {exc}"))

    all_clear = all(r.passed for r in results if r.severity == "FAIL")
    if verbose:
        print(f"\n  === INVARIANTS [{spec.name}] (height axis = {'XYZ'[h_axis]}) ===")
        for r in results:
            mark = "PASS" if r.passed else (r.severity)
            print(f"    [{mark:>4}] {r.name:<14} {r.detail}")
        print(f"    {'ALL CLEAR' if all_clear else 'FAILED'}"
              f" — {sum(r.passed for r in results)}/{len(results)} checks passed")
    return all_clear, results


# ------------------------------------------------------------------------------
# Standalone entry point
# ------------------------------------------------------------------------------
def _load_obj_bmesh(path):
    import bpy
    bpy.ops.wm.read_factory_settings(use_empty=True)
    try:
        bpy.ops.wm.obj_import(filepath=str(path),
                              forward_axis='NEGATIVE_Z', up_axis='Y')
    except AttributeError:
        bpy.ops.import_scene.obj(filepath=str(path))
    obj = [o for o in bpy.context.scene.objects if o.type == 'MESH'][0]
    bm = bmesh.new(); bm.from_mesh(obj.data)
    bm.edges.ensure_lookup_table(); bm.faces.ensure_lookup_table()
    return bm


def _specs_from_pieces_v2():
    """Build PieceSpecs by extracting the same SVG polygons pieces_v2.py uses,
    so silhouette/profile checks compare against the true intended footprints."""
    import importlib.util
    from pathlib import Path
    p2_path = Path(__file__).parent / "pieces_v2.py"
    spec = importlib.util.spec_from_file_location("pieces_v2", p2_path)
    p2 = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(p2)
    _, polygons = p2.extract_silhouettes()

    # NOTE: FOOD is intentionally excluded — it's already designed (shallow
    # parabolic dish + matching bottom dome, built by build_food) and is not
    # subject to the six body invariants (it isn't a tapering single-apex body).
    return {
        "EAT":  PieceSpec("EAT",  p2.EAT_HEIGHT,  "hemisphere", cyl_top=7/8,
                          fold=5, polygon=polygons["EAT"]),
        "MOVE": PieceSpec("MOVE", p2.MOVE_HEIGHT, "parabola",   cyl_top=0.0,
                          fold=3, polygon=polygons["MOVE"]),
        "GROW": PieceSpec("GROW", p2.GROW_HEIGHT, "hemisphere", cyl_top=0.5,
                          fold=4, polygon=polygons["GROW"]),
    }


def main():
    import sys
    from pathlib import Path
    argv = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
    here = Path(__file__).parent
    specs = _specs_from_pieces_v2()
    # FOOD is already designed and not subject to the body invariants.
    names = [argv[0].replace(".obj", "")] if argv else ["EAT", "MOVE", "GROW"]

    overall = True
    for name in names:
        obj_path = here / f"{name}.obj"
        if not obj_path.exists():
            print(f"  (skip {name}: {obj_path} not found)")
            continue
        bm = _load_obj_bmesh(obj_path)
        ok, _ = validate(bm, specs[name])
        bm.free()
        overall = overall and ok
    print(f"\n  OVERALL: {'ALL CLEAR' if overall else 'FAILED'}")


if __name__ == "__main__":
    main()
