"""
================================================================================
 ORGANISM BOARD-GAME PIECE GENERATOR  (pieces_v2.py)
================================================================================

Generates the three player pieces — EAT, MOVE, GROW — as sculpt-ready OBJ
meshes from a single 2D SVG of silhouettes. Each piece is built by a PARAMETRIC
CONSTRUCTION designed to satisfy six topology invariants (see below)
simultaneously, so the meshes are clean enough to import into Blender for
sculpting and eventually optimize for injection molding.

(FOOD is NOT generated here — it is hand-managed as pieces/FOOD.obj, converted
from FoodUniversal.stl, and already carries its own connector.)

--------------------------------------------------------------------------------
 HOW TO RUN
--------------------------------------------------------------------------------
Requires Blender 5.1.x (uses the bundled Python + bpy/bmesh/mathutils). No pip
installs needed — everything is in Blender's Python. The ONLY external input is
the SVG of silhouettes (see SVG path constant below).

    blender --background --python pieces_v2.py

(Substitute the path to your Blender binary. During development this was
 ~/Downloads/blender-5.1.1-linux-x64/blender on Linux.)

Outputs, all written next to this script:
    EAT.obj  MOVE.obj  GROW.obj             — the meshes
    connector_meta.json                     — per-piece connector placement
    pieces.blend  pieces.glb                — combined viewing scenes
    renders/*.png                           — workbench renders + topology audit

See README.md in this directory for full clone-from-scratch setup.

--------------------------------------------------------------------------------
 THE SIX INVARIANTS  (every accepted piece must satisfy ALL of them)
--------------------------------------------------------------------------------
 1. Continuous manifold topology — 0 boundary edges, 0 non-manifold edges,
    0 degenerate faces.
 2. Top-down silhouette == SVG — orthographic projection from above exactly
    matches the design's 2D outline.
 3. Rotational profile — viewed from any side, the body's profile follows the
    chosen curve (parabola/hemisphere/shallow). The curve rises from the
    polygon's FULL width at each angle (no inscribed-circle "shoulder").
 4. All triangles, roughly uniform SIZE (~1mm).
 5. Smooth by construction — sample a smooth analytic function on a uniform
    grid; never project points onto a faceted approximation.
 6. No long edges — every edge <= ~1.5x target. A long edge "bridges" across a
    design feature that shouldn't be connected and breaks invariants 2 & 5.

(Full statements live in the project memory at
 .claude/.../memory/project_piece_design_method.md)

--------------------------------------------------------------------------------
 THE CONSTRUCTION METHOD  (a small algebra of composable operators)
--------------------------------------------------------------------------------
Two base builders turn a footprint + a height curve into a closed body:

  parametric_body(polygon_r_func, z_max, shape, fold, cyl_top, ...)
      For STAR-SHAPED polygons. Samples a (theta, h) grid where
      polygon_r_func(theta) gives the outline radius at each angle.
      Used by EAT (fold=5) and GROW (fold=4).

  parametric_body_polygon(polygon_pts, z_max, shape, fold, ...)
      For NON-STAR polygons (MOVE's spiral with inward hook curls). Samples by
      ARC LENGTH around the outline so multi-valued r(theta) regions survive.
      Used by MOVE (fold=3).

Operators (each a smooth function applied per-vertex inside the sampling loop;
they COMPOSE and individually preserve the invariants):
  - scale_at_h(h)        side profile (parabola / hemisphere / shallow / cosine)
  - cyl_top              vertical-wall lower section before the taper begins
  - twist(r)             per-radius rotation about z (polygon-only spiral)
  - twist_h(h)           per-height rotation about z (corkscrews the body)
  - hook_fade(h)         Gaussian-smooth the outline along arc length with height
  - apex coarsening      halve ring vertex count via convergence belts so the
                         apex isn't a sliver-pole (see build_convergence_belt)
  - build_inward_disc    structured concentric-ring bottom for star polygons
  - strict_ear_clip      bottom triangulation that respects non-convex outlines

Per-piece configuration (current):
  EAT  : cylinder to 7/8 + small hemisphere cap;  fold=5, star polygon
  GROW : cylinder to 1/2 + hemisphere top;        fold=4, star polygon
  MOVE : pure parabola;                           fold=3, NON-star polygon

Every build is followed by check_topology(), which reports the six invariants
(boundary/non-manifold/tiny/sliver/long-edge/self-intersection) and classifies
sharp dihedrals into body-interior (real creases) vs intentional rim/apex.

--------------------------------------------------------------------------------
 THE OPEN PROBLEM:  MOVE's silhouette (invariant 2) vs profile (invariant 3)
--------------------------------------------------------------------------------
STATUS: UNSOLVED with the current method. EAT and GROW fully satisfy all six
invariants. MOVE does not — and the reason is fundamental, not a bug.

MOVE's silhouette is a 3-fold spiral whose arms END IN INWARD HOOK CURLS. As a
polygon this is NON-STAR-SHAPED: a ray from the origin crosses the outline more
than twice (it enters the central body, exits into a gap, re-enters the outer
hook, exits again). Equivalently: the polygon's OUTER ENVELOPE (max radius per
angle) is much fatter than its INTERIOR — the hooks reach outward at the same
angles where the slender shape has a gap.

Our body is built by extruding the polygon and tapering it to an apex via
uniform scaling toward the origin. For a star polygon this works: every scaled
copy nests inside the base outline, so the top-down silhouette == the polygon
interior. For a NON-STAR polygon it FAILS: scaling a hook point toward the
origin moves it into a gap angle, so the union of all scaled rings (= the
silhouette) becomes the OUTER ENVELOPE, not the interior. Result: MOVE's
top-down silhouette fills the spiral gaps — fat 3-lobes instead of the slender
spiral.

WHAT WE TRIED (all rejected — see project memory for details):
  - twist_h body rotation         -> smears arms tangentially, also breaks #2
  - hook_fade (smoothing w/ h)     -> shrinks max-r, breaks #3 (profile sags
                                      11-22% below the parabola)
  - apex coarsening on MOVE        -> convergence belts merge arc-adjacent but
                                      angularly-distant verts -> 29mm bridging
                                      edges that fill gaps (invariant #6)
  - tessellate_polygon bottom      -> 30% of triangles span concavities (gaps)
  - bmesh triangle_fill / EAR_CLIP -> same spanning problem
  - custom strict ear clip         -> respects outline but degenerate slivers
  - no bottom face                 -> silhouette STILL fat (proves the side wall
                                      itself, via uniform scaling, is the cause)

THE TRILEMMA (for a non-star polygon, pick at most 2 of 3):
  (a) single connected body with single apex
  (b) single parabolic side profile (invariant #3)
  (c) top-down silhouette == SVG interior (invariant #2)

  - Current MOVE keeps (a)+(b), loses (c).
  - "3 tendrils" keeps (a-ish)+(c), loses (b).
  - "height field / medial axis" keeps (a)+(c), loses (b).
  - "tunnels through the body" might keep all three but needs a fundamentally
    different construction (carve vertical voids where the gaps are) and raises
    mold-release questions.

DECISION (user): do NOT compromise any invariant. A NEW METHOD is required for
non-star polygons — likely one that extrudes the polygon's INTERIOR REGION
(not its outline scaled toward a point) so that every cross-section stays a
subset of the base interior while still tapering to a clean apex and holding a
parabolic profile. That method has not been designed yet. Until then MOVE is
left in its current (invariant-#2-violating) state as a placeholder.

NOTE (2026-05): this "UNSOLVED/placeholder" verdict is about THIS uniform-scaling parametric
method only. A separate non-parametric method (meshlib/, a height-field build -- see DECISIONS.md)
DID produce a valid MOVE spiral body that keeps the silhouette; that is the MOVE we now
sculpt/graft/print (MOVE_connected.obj, a real watertight genus-3 spiral). "Placeholder" here
means THIS recipe can't make MOVE, not that no MOVE body exists.

--------------------------------------------------------------------------------
 OLD APPROACH (pre-parametric, now removed)
--------------------------------------------------------------------------------
Earlier versions imported a hand-made EAT.07.obj, lofted MOVE from a cage with a
60-degree twist and hollow leg arches, etc. Those are gone; a few dead
constants below (ORIG_EAT_OBJ, N_THETA, N_Z, MOVE_TWIST_TOTAL, MOVE_LEG_*)
remain only as historical breadcrumbs and are not referenced by the current
pipeline.
"""
import bpy, bmesh, math
from math import pi, cos, sin, hypot, atan2
from pathlib import Path
from mathutils import Vector

OUT = Path(__file__).parent
RENDERS = OUT / "renders"
RENDERS.mkdir(exist_ok=True)

SVG = Path.home()/"Downloads/01_organism-elements_wyn_02-01.svg"
ORIG_EAT_OBJ = Path.home()/"code/elephantlaboratories/resources/public/tts/organism/EAT.07.obj"

# === Connector spec (parabolic dome + outer ridge) ===
DOME_DIA       = 3.8
DOME_HEIGHT    = 4.3
RIDGE_OD       = 12.8
RIDGE_ID       = 8.3
RIDGE_HEIGHT   = 2.75
RIDGE_PEAK_W   = 2.0
CLEARANCE      = 0.15      # nominal slip-fit gap (peg <-> socket)
IM_TOLERANCE   = 0.05      # extra allowance for injection-molding dimensional tolerance
SOCKET_GAP     = CLEARANCE + IM_TOLERANCE   # total socket oversize
EDGE_FILLET    = 0.5
DOME_SEGS      = 64
DOME_RES       = 24

PLATEAU_DIA = max(RIDGE_OD + 4.0, 16.0)
PLATEAU_R   = PLATEAU_DIA / 2

# === Piece sizing ===
FOOTPRINT = 37.0           # canonical 37×37 footprint
N_THETA   = 192            # high-res loft for final (with-connector) output
N_Z       = 64
N_THETA_CAGE = 64          # cage angular res — fine enough to preserve SVG silhouette features
N_Z_CAGE     = 24

EAT_HEIGHT  = 48.0
MOVE_HEIGHT = 60.0
GROW_HEIGHT = 36.0

FOOD_DIA    = 33.5         # mm, matches original FOOD.13
FOOD_HEIGHT = 9.0
FOOD_EDGE_FILLET = 1.5     # softens the disc's rim

MOVE_TWIST_TOTAL = math.radians(60)

# Where the legs end (z height of the "ceiling" of the leg arches)
MOVE_LEG_HEIGHT_FRAC = 0.30
MOVE_LEG_ARCH_RADIUS = 14.0   # radius of cutout between legs at z=0


def reset():
    bpy.ops.wm.read_factory_settings(use_empty=True)


def smoothstep(t):
    t = max(0.0, min(1.0, t))
    return t*t*(3 - 2*t)


# ===================================================================
# SVG silhouette extraction
# ===================================================================

SIL_BINS = 256   # angular bins for the resampled silhouette

class Silhouette:
    """Angular profile r(theta) sampled from an SVG outline. The outline is
    walked in TRAVERSAL ORDER (using the mesh edge graph) and each angular
    bin records the actual outline radius via interpolation between walk
    points. For shapes where multiple walk segments cross the same bin
    (rare — happens with hook curls), MIN is taken so concavities and
    inward-curling features are preserved."""

    def __init__(self, name, bin_radii):
        self.name = name
        self.bins = bin_radii

    @classmethod
    def from_points(cls, name, points_theta_r):
        """Bin (theta, r) pairs by angle, take MAX r per bin (outer envelope),
        fill empty bins by neighbor interpolation, lightly smooth."""
        bins = [None] * SIL_BINS
        for th, r in points_theta_r:
            t = ((th + pi) % (2*pi)) - pi
            idx = int((t + pi) / (2*pi) * SIL_BINS) % SIL_BINS
            if bins[idx] is None or r > bins[idx]:
                bins[idx] = r
        # Fill empty bins forward and backward (wrap once)
        last = None
        for i in range(SIL_BINS * 2):
            j = i % SIL_BINS
            if bins[j] is not None: last = bins[j]
            elif last is not None: bins[j] = last
        last = None
        for i in range(SIL_BINS * 2 - 1, -1, -1):
            j = i % SIL_BINS
            if bins[j] is not None: last = bins[j]
            elif last is not None: bins[j] = last
        # Sigma=2 → ~2.8° smoothing. Preserves all macro features (star points
        # 72° apart, quatrefoil lobes 90° apart, spiral arms 120° apart) while
        # damping single-bin spikes from Bezier control point clustering.
        smoothed = cls._gaussian_smooth(bins, sigma=2.0)
        return cls(name, smoothed)

    @staticmethod
    def _gaussian_smooth(arr, sigma=2.0):
        n = len(arr)
        half = max(1, int(sigma * 3))
        weights = [math.exp(-(i*i)/(2*sigma*sigma)) for i in range(-half, half+1)]
        wsum = sum(weights)
        weights = [w/wsum for w in weights]
        out = [0.0] * n
        for i in range(n):
            s = 0.0
            for k, w in enumerate(weights):
                j = (i + k - half) % n
                s += arr[j] * w
            out[i] = s
        return out

    def r_at(self, theta):
        t = ((theta + pi) % (2*pi)) - pi
        x = (t + pi) / (2*pi) * SIL_BINS
        i0 = int(x) % SIL_BINS
        i1 = (i0 + 1) % SIL_BINS
        frac = x - int(x)
        return self.bins[i0] * (1 - frac) + self.bins[i1] * frac

    def amplified(self, amp):
        """Return a new Silhouette with modulation around the mean amplified.
        amp=1.0 → unchanged, amp=1.5 → spikes 50% longer and valleys 50% deeper,
        amp<1.0 → smoothed toward a circle."""
        avg = sum(self.bins) / len(self.bins)
        new_bins = [max(0.5, avg + amp * (b - avg)) for b in self.bins]
        return Silhouette(self.name + "_amp", new_bins)


def extract_silhouettes():
    """Import the SVG, convert curves to meshes, return a dict
    {name: Silhouette} for 'EAT', 'MOVE', 'GROW'."""
    reset()
    try:
        bpy.ops.wm.svg_import(filepath=str(SVG))
    except AttributeError:
        bpy.ops.import_curve.svg(filepath=str(SVG))

    curves = [o for o in bpy.context.scene.objects if o.type == 'CURVE']
    # Convert each to mesh & collect (centroid_x, vertex_world_positions)
    meshes = []
    for c in curves:
        bpy.ops.object.select_all(action='DESELECT')
        c.select_set(True); bpy.context.view_layer.objects.active = c
        bpy.ops.object.convert(target='MESH')
        verts = [c.matrix_world @ v.co for v in c.data.vertices]
        if not verts: continue
        cx = sum(v.x for v in verts)/len(verts)
        cy = sum(v.y for v in verts)/len(verts)
        meshes.append((cx, cy, verts, len(verts)))

    # Sort: identify by position in viewBox (200x180):
    #   EAT = top-left  (smallest x, larger y)
    #   GROW = top-right (largest x, larger y)
    #   MOVE = bottom    (smallest y)
    # In Blender coords, SVG Y is flipped (negative going down on page).
    # So Blender's "larger y" = SVG's smaller y (top) and vice versa.
    # Our observed centroids (from earlier preview):
    #   curve0 (0.01, 0.04) → EAT
    #   curve1 (0.03, 0.01) → MOVE  (smallest Y)
    #   curve2 (0.04, 0.04) → GROW  (largest X)
    meshes.sort(key=lambda m: (m[1], m[0]))  # primary: y asc, secondary: x asc
    # Now meshes[0] has smallest y → MOVE, meshes[1] and meshes[2] depend on x
    move_data = meshes[0]
    rest = sorted(meshes[1:], key=lambda m: m[0])  # by x asc
    eat_data  = rest[0]   # smallest x → EAT
    grow_data = rest[1]   # largest x → GROW

    silhouettes = {}
    polygons    = {}
    TARGET_PTS = 128

    for name, info in [('EAT', eat_data), ('MOVE', move_data), ('GROW', grow_data)]:
        cx, cy, verts, _ = info
        # Find the converted mesh in the scene to read its filled polygon's loop
        # — that gives outline vertices in TRAVERSAL ORDER (unlike raw vertex array).
        target_obj = None
        for o in bpy.context.scene.objects:
            if o.type != 'MESH' or not o.data.vertices: continue
            mverts = [o.matrix_world @ v.co for v in o.data.vertices]
            ocx = sum(v.x for v in mverts)/len(mverts)
            ocy = sum(v.y for v in mverts)/len(mverts)
            if abs(ocx - cx) < 1e-4 and abs(ocy - cy) < 1e-4:
                target_obj = o; break

        ordered = None
        if target_obj:
            ordered = walk_outline_edges(target_obj)
            print(f"    [{name}] walked outline: {len(ordered)} verts (raw mesh had {len(target_obj.data.vertices)})")
        if not ordered or len(ordered) < 8:
            print(f"    [{name}] walk failed/too few verts, falling back to raw vertex order")
            ordered = verts

        # Center polygon at the CENTROID OF AREA, not the mean of vertex
        # positions. For a polygon with hook curls or other features that
        # cluster many vertices in one region (like MOVE's spiral), the
        # vertex-mean is biased toward where the vertices are dense, shifting
        # the polygon away from its true visual center. The centroid-of-area
        # is the true center of mass of the filled shape.
        pts = [(v.x, v.y) for v in ordered]
        n_pts = len(pts)
        # Shoelace area and centroid-of-area formulas
        A2 = 0.0     # 2 * area (signed)
        cx_num = 0.0
        cy_num = 0.0
        for i in range(n_pts):
            x1, y1 = pts[i]
            x2, y2 = pts[(i+1) % n_pts]
            cross = x1*y2 - x2*y1
            A2 += cross
            cx_num += (x1 + x2) * cross
            cy_num += (y1 + y2) * cross
        if abs(A2) > 1e-9:
            ocx = cx_num / (3 * A2)
            ocy = cy_num / (3 * A2)
        else:
            ocx = sum(v.x for v in ordered) / n_pts
            ocy = sum(v.y for v in ordered) / n_pts
        centered = [(v.x - ocx, v.y - ocy) for v in ordered]
        print(f"    [{name}] centroid-of-area: ({ocx:.2f}, {ocy:.2f})  (vs vertex mean: ({sum(v.x for v in ordered)/n_pts:.2f}, {sum(v.y for v in ordered)/n_pts:.2f}))")

        # Enforce strict N-fold rotational symmetry on the polygon.
        # MOVE's spiral hooks are sensitive to symmetrization — both averaging
        # and arm-tip slicing distort them. Skip symmetrization for MOVE;
        # accept approximate (not strict) 3-fold for the sake of preserving
        # the spiral character. (User cares more about silhouette fidelity
        # than perfect sculpt-radial-symmetry for MOVE specifically.)
        fold_N = SYMMETRY_FOLD[name]
        if name == 'MOVE':
            symm = centered
            print(f"    [{name}] using raw polygon (skipping symmetrization to preserve hooks)")
        else:
            symm = symmetrize_polygon(centered, fold_N)

        # Now subsample (if still very dense) and scale to FOOTPRINT
        if len(symm) > TARGET_PTS * fold_N:    # keep div-by-N
            stride = max(1, len(symm) // (TARGET_PTS * fold_N // fold_N * fold_N))
            symm = [symm[i] for i in range(0, len(symm), stride)]
        max_r = max(hypot(x, y) for x, y in symm)
        scale = (FOOTPRINT/2) / max_r
        polygon = [(x*scale, y*scale) for x, y in symm]
        polygons[name] = polygon

        # Diagnostic: dump polygon as SVG so we can verify its shape directly
        svg_path = RENDERS / f"{name}_polygon.svg"
        vb = FOOTPRINT * 0.55
        with open(svg_path, 'w') as f:
            f.write(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="{-vb} {-vb} {2*vb} {2*vb}">\n')
            f.write('  <path d="')
            for i, (x, y) in enumerate(polygon):
                cmd = 'M' if i == 0 else 'L'
                f.write(f'{cmd}{x:.3f},{y:.3f} ')
            f.write('Z" fill="#999" stroke="#000" stroke-width="0.05"/>\n')
            f.write('</svg>\n')

        # Diagnostic: catch sector-boundary jumps (long edges) or near-duplicate verts
        max_edge = 0.0
        min_edge = float('inf')
        for i in range(len(polygon)):
            a = polygon[i]; b = polygon[(i+1) % len(polygon)]
            d = hypot(b[0]-a[0], b[1]-a[1])
            if d > max_edge: max_edge = d
            if d < min_edge: min_edge = d
        avg_edge = sum(hypot(polygon[(i+1)%len(polygon)][0]-polygon[i][0],
                             polygon[(i+1)%len(polygon)][1]-polygon[i][1])
                       for i in range(len(polygon))) / len(polygon)
        print(f"    [{name}] symmetrized to {len(polygon)} pts ({fold_N}-fold), "
              f"edge len: min={min_edge:.3f}, avg={avg_edge:.3f}, max={max_edge:.3f} mm")

        # Also build the radial Silhouette (backward compat — kept for FOOD/legacy paths)
        raw_pts = [(atan2(y, x), hypot(x, y)) for x, y in polygon]
        sil = Silhouette.from_points(name, raw_pts)
        silhouettes[name] = sil
        print(f"  {name}: polygon {len(polygon)} pts, r=[{min(sil.bins):.2f}, {max(sil.bins):.2f}] mm")

    return silhouettes, polygons


# ===================================================================
# Connector primitives (parabolic dome + outer ridge)
# ===================================================================

def build_parabolic_dome(name, r_base, height, segments=DOME_SEGS, n_z=DOME_RES, z_offset=0):
    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()
    rings = []
    for iz in range(n_z + 1):
        t = iz / n_z
        z = t * height
        r = r_base * math.sqrt(max(0.0, 1.0 - t))
        if r < 1e-4:
            apex = bm.verts.new((0, 0, z_offset + z))
            rings.append(("apex", apex))
            break
        ring = [bm.verts.new((r*cos(2*pi*ith/segments), r*sin(2*pi*ith/segments), z_offset + z))
                for ith in range(segments)]
        rings.append(("ring", ring))
    for i in range(len(rings)-1):
        ak, a = rings[i]; bk, b = rings[i+1]
        if ak=="ring" and bk=="ring":
            for ith in range(segments):
                j = (ith+1) % segments
                bm.faces.new([a[ith], a[j], b[j], b[ith]])
        elif ak=="ring" and bk=="apex":
            for ith in range(segments):
                j = (ith+1) % segments
                bm.faces.new([a[ith], a[j], b])
    bm.faces.new(rings[0][1][::-1])
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj; obj.select_set(True)
    return obj


def build_ridge_ring(name, od, id_, height, peak_w, segments=DOME_SEGS, z_offset=0):
    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()
    n_profile = 12
    profile = []
    for ip in range(n_profile+1):
        u = ip / n_profile
        r = id_/2 + (od/2 - id_/2) * u
        u_centered = abs(2*u - 1)
        center_frac = peak_w / (od - id_)
        edge_frac = max(1 - center_frac, 1e-6)
        if u_centered < center_frac:
            bump = 1.0
        else:
            t = (u_centered - center_frac) / edge_frac
            bump = 1.0 - t*t
        profile.append((r, height * bump))
    rings_top = []
    for r, z in profile:
        ring = [bm.verts.new((r*cos(2*pi*ith/segments), r*sin(2*pi*ith/segments), z_offset + z))
                for ith in range(segments)]
        rings_top.append(ring)
    bot_inner = [bm.verts.new(((id_/2)*cos(2*pi*ith/segments), (id_/2)*sin(2*pi*ith/segments), z_offset))
                 for ith in range(segments)]
    bot_outer = [bm.verts.new(((od/2)*cos(2*pi*ith/segments), (od/2)*sin(2*pi*ith/segments), z_offset))
                 for ith in range(segments)]
    for i in range(len(rings_top)-1):
        a = rings_top[i]; b = rings_top[i+1]
        for ith in range(segments):
            j = (ith+1) % segments
            bm.faces.new([a[ith], a[j], b[j], b[ith]])
    for ith in range(segments):
        j = (ith+1) % segments
        bm.faces.new([bot_inner[ith], bot_outer[ith], bot_outer[j], bot_inner[j]])
    inner_top = rings_top[0]; outer_top = rings_top[-1]
    for ith in range(segments):
        j = (ith+1) % segments
        bm.faces.new([inner_top[ith], inner_top[j], bot_inner[j], bot_inner[ith]])
        bm.faces.new([outer_top[j], outer_top[ith], bot_outer[ith], bot_outer[j]])
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj; obj.select_set(True)
    return obj


def add_connector_peg(parent_obj, top_z):
    dome  = build_parabolic_dome(parent_obj.name+"_Dome",  DOME_DIA/2,    DOME_HEIGHT,  z_offset=top_z)
    ridge = build_ridge_ring(parent_obj.name+"_Ridge", RIDGE_OD, RIDGE_ID, RIDGE_HEIGHT, RIDGE_PEAK_W, z_offset=top_z)
    for child in (dome, ridge):
        bpy.context.view_layer.objects.active = parent_obj
        m = parent_obj.modifiers.new(f"U_{child.name}", 'BOOLEAN')
        m.operation='UNION'; m.object=child; m.solver='EXACT'
        bpy.ops.object.modifier_apply(modifier=f"U_{child.name}")
        bpy.data.objects.remove(child, do_unlink=True)


def add_connector_socket(parent_obj):
    # Cavity opens at the bottom face (z=0) and narrows upward into the body, so the
    # peg of the piece below seats into it. SOCKET_GAP = fit clearance + IM tolerance.
    dome  = build_parabolic_dome("CavDome",  DOME_DIA/2 + SOCKET_GAP,  DOME_HEIGHT + SOCKET_GAP,  z_offset=0)
    ridge = build_ridge_ring("CavRidge", RIDGE_OD + 2*SOCKET_GAP, RIDGE_ID - 2*SOCKET_GAP,
                             RIDGE_HEIGHT + SOCKET_GAP, RIDGE_PEAK_W, z_offset=0)
    for child in (dome, ridge):
        bpy.context.view_layer.objects.active = parent_obj
        m = parent_obj.modifiers.new(f"C_{child.name}", 'BOOLEAN')
        m.operation='DIFFERENCE'; m.object=child; m.solver='EXACT'
        bpy.ops.object.modifier_apply(modifier=f"C_{child.name}")
        bpy.data.objects.remove(child, do_unlink=True)


# ===================================================================
# Generic loft builder
# ===================================================================

# Symmetry fold per piece — used to enforce strict N-fold rotational symmetry
# on the polygon so the sculpt mesh works correctly with Blender's Radial Z = N.
SYMMETRY_FOLD = {'EAT': 5, 'MOVE': 3, 'GROW': 4}


def symmetrize_polygon(polygon, fold_N):
    """Return a polygon with strict N-fold rotational symmetry around the origin.

    Strategy: find the N strongest local-maxima-of-radius (the "arm tips") to
    use as natural sector boundaries. Take the polygon segment between two
    consecutive arm tips as the canonical arm. Rotate-copy it N times.

    This gives:
    - exact N-fold symmetry by construction
    - seamless closure (the polygon's last vertex is one step before the first
      vertex, naturally)
    - no averaging artifacts at sector boundaries

    Assumes the polygon is centered at the origin and is APPROXIMATELY N-fold
    to begin with (otherwise this loses information about non-symmetric arms).
    """
    n = len(polygon)
    if n < 3 * fold_N:
        return polygon

    radii = [hypot(x, y) for x, y in polygon]
    # Local maxima of r (arm tips). Use strict > to avoid flat plateaus.
    maxima = [i for i in range(n)
              if radii[i] > radii[(i-1) % n] and radii[i] > radii[(i+1) % n]]
    # Take the N highest-r maxima as the actual arm tips
    if len(maxima) < fold_N:
        # Fallback: not enough clean maxima — use averaging method instead
        return _symmetrize_via_averaging(polygon, fold_N)
    arm_tips = sorted(maxima, key=lambda i: radii[i], reverse=True)[:fold_N]
    arm_tips.sort()  # back into cyclic order

    # Take the polygon segment between arm_tips[0] and arm_tips[1] as canonical
    start, end = arm_tips[0], arm_tips[1]
    if end > start:
        canonical = polygon[start:end]
    else:
        canonical = polygon[start:] + polygon[:end]

    # Rotate-copy the canonical arm N times to build the full polygon
    result = []
    for k in range(fold_N):
        ang = k * 2*pi / fold_N
        c = cos(ang); s = sin(ang)
        for px, py in canonical:
            result.append((px*c - py*s, px*s + py*c))
    return result


def _symmetrize_via_averaging(polygon, fold_N):
    """Fallback symmetrization via N-sector averaging (used when arm-tip detection
    can't find enough local maxima — e.g. for nearly circular silhouettes)."""
    n = len(polygon)
    pts_per_arm = n // fold_N
    if pts_per_arm == 0:
        return polygon
    polygon = polygon[:pts_per_arm * fold_N]
    canonical = []
    for i in range(pts_per_arm):
        x_sum = 0.0; y_sum = 0.0
        for k in range(fold_N):
            ang = -k * 2*pi / fold_N
            c = cos(ang); s = sin(ang)
            px, py = polygon[k * pts_per_arm + i]
            x_sum += px*c - py*s
            y_sum += px*s + py*c
        canonical.append((x_sum / fold_N, y_sum / fold_N))
    result = []
    for k in range(fold_N):
        ang = k * 2*pi / fold_N
        c = cos(ang); s = sin(ang)
        for px, py in canonical:
            result.append((px*c - py*s, px*s + py*c))
    return result


def walk_outline_edges(mesh_obj):
    """Walk the mesh's edge graph along BOUNDARY edges (edges in ≤1 face) to
    extract outline vertices in traversal order. Filled n-gons get triangulated
    on import, which adds interior edges that would derail a naive walk; ignoring
    them keeps the walk on the actual outline."""
    bm = bmesh.new()
    bm.from_mesh(mesh_obj.data)
    adj = {v.index: [] for v in bm.verts}
    for e in bm.edges:
        if len(e.link_faces) <= 1:   # boundary (1 face) or wire (0 faces) → on the outline
            v0, v1 = e.verts[0].index, e.verts[1].index
            adj[v0].append(v1)
            adj[v1].append(v0)
    coord = {v.index: mesh_obj.matrix_world @ v.co for v in bm.verts}
    bm.free()

    # Start at any degree-2 outline vertex
    start = next((i for i, n in adj.items() if len(n) == 2), None)
    if start is None:
        # No clean outline — fall back to any vertex with at least one neighbor
        start = next((i for i, n in adj.items() if n), 0)
    visited = {start}
    order = [start]
    prev = -1
    curr = start
    while True:
        next_v = None
        for n in adj[curr]:
            if n != prev and n not in visited:
                next_v = n; break
        if next_v is None: break
        visited.add(next_v)
        order.append(next_v)
        prev = curr
        curr = next_v
    return [coord[i] for i in order]


def build_polygon_dome(name, polygon, z_max, shape='parabola', slab_h=2.0,
                        n_dome_rings=12, n_theta=64):
    """Polygon footprint at the BASE + rotationally symmetric dome rising from a
    central circle. No polygon ridges propagate up the dome — the dome itself is
    purely circular, so subsurf-smoothing doesn't create pleats.

    Construction:
      1. Bottom face: tessellated polygon at z=0
      2. Slab: polygon outline at z=0 morphs to a CIRCLE at z=slab_h (inside the
         polygon's inscribed circle). This is the "shoulder" transition.
      3. Dome: concentric circular rings from z=slab_h up to apex at z=z_max,
         radii determined by inverting the shape function.

    All boundaries are circles above z=slab_h → dome is rotationally symmetric.
    """
    from mathutils import Vector
    from mathutils.geometry import tessellate_polygon

    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()

    n_pts = len(polygon)
    r_inscribed = min(math.hypot(x, y) for x, y in polygon) * 0.95   # safely inside polygon

    def r_at_z(z):
        """Inverse of the shape function: at height z above slab, what's the dome radius?"""
        if z <= slab_h: return r_inscribed
        if z >= z_max:  return 0
        dome_h = z_max - slab_h
        z_norm = (z - slab_h) / dome_h
        if shape == 'parabola':
            return r_inscribed * math.sqrt(max(0, 1 - z_norm))
        elif shape == 'hemisphere':
            return r_inscribed * math.sqrt(max(0, 1 - z_norm*z_norm))
        elif shape == 'shallow':
            return r_inscribed * (1 - z_norm)**0.25
        elif shape == 'cosine':
            return r_inscribed * math.cos(z_norm * math.pi / 2)
        else:
            raise ValueError(f"unknown shape {shape!r}")

    # 1. Bottom polygon at z=0
    bot_polygon = [bm.verts.new((x, y, 0)) for x, y in polygon]

    # 2. Slab top: a CIRCLE of n_pts vertices at angles matching the polygon vertices
    polygon_angles = [math.atan2(py, px) for px, py in polygon]
    slab_circle = [bm.verts.new((r_inscribed * math.cos(a),
                                  r_inscribed * math.sin(a),
                                  slab_h))
                   for a in polygon_angles]

    # 3. Dome rings (concentric circles, all at n_theta uniformly-spaced angles)
    # The first dome ring uses the SAME vertices as slab_circle to avoid duplicate poles.
    # Wait — slab_circle has n_pts (irregular angles), dome rings have n_theta (uniform).
    # We need a transition layer. Simplest: have the slab_circle's outermost match a
    # uniformly-distributed circle at z=slab_h that we use as the dome's base.
    # To keep it simple, use the same vertex count throughout (n_pts) with the
    # polygon's angles to avoid topology mismatch.
    dome_rings = [slab_circle]
    for ri in range(1, n_dome_rings):
        # z progresses linearly from slab_h to z_max
        z = slab_h + (z_max - slab_h) * (ri / n_dome_rings)
        r = r_at_z(z)
        if r < 0.01: r = 0.01  # avoid degenerate
        ring = [bm.verts.new((r * math.cos(a),
                              r * math.sin(a),
                              z))
                for a in polygon_angles]
        dome_rings.append(ring)
    apex = bm.verts.new((0, 0, z_max))

    # 4. Bottom face (tessellated polygon, reversed for -Z normal)
    polygon_3d = [Vector((x, y, 0)) for x, y in polygon]
    for i1, i2, i3 in tessellate_polygon([polygon_3d]):
        bm.faces.new([bot_polygon[i3], bot_polygon[i2], bot_polygon[i1]])

    # 5. Slab side walls (polygon at z=0 → circle at z=slab_h, quads connecting 1-to-1)
    for i in range(n_pts):
        j = (i + 1) % n_pts
        bm.faces.new([bot_polygon[i], bot_polygon[j], slab_circle[j], slab_circle[i]])

    # 6. Dome ring-to-ring quads
    for ri in range(n_dome_rings - 1):
        a, b = dome_rings[ri], dome_rings[ri + 1]
        for i in range(n_pts):
            j = (i + 1) % n_pts
            bm.faces.new([a[i], a[j], b[j], b[i]])

    # 7. Apex fan (last dome ring to apex)
    last = dome_rings[-1]
    for i in range(n_pts):
        j = (i + 1) % n_pts
        bm.faces.new([last[i], last[j], apex])

    bmesh.ops.triangulate(bm, faces=list(bm.faces), quad_method='BEAUTY', ngon_method='BEAUTY')
    bmesh.ops.recalc_face_normals(bm, faces=list(bm.faces))
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj; obj.select_set(True)
    return obj


def strict_ear_clip_indices(polygon_pts_2d):
    """Strict ear clipping triangulation of a simple polygon.

    Returns list of (i, j, k) triples indexing into polygon_pts_2d, where each
    triangle has all three vertices on the polygon outline AND its interior
    is entirely inside the polygon (no spanning across concavities).

    Blender's bmesh.ops.triangulate('EAR_CLIP') and bmesh.ops.triangle_fill
    both produce spanning triangles for non-convex polygons; this is a
    strict implementation that respects the outline.
    """
    n = len(polygon_pts_2d)
    if n < 3: return []
    pts = polygon_pts_2d

    def cross(o, a, b):
        return (a[0]-o[0])*(b[1]-o[1]) - (a[1]-o[1])*(b[0]-o[0])

    def point_in_tri(p, a, b, c):
        d1 = cross(a, b, p); d2 = cross(b, c, p); d3 = cross(c, a, p)
        has_neg = d1 < 0 or d2 < 0 or d3 < 0
        has_pos = d1 > 0 or d2 > 0 or d3 > 0
        return not (has_neg and has_pos)

    # Determine winding via signed area; ear clipping assumes CCW
    area2 = 0.0
    for i in range(n):
        x0, y0 = pts[i]; x1, y1 = pts[(i+1) % n]
        area2 += (x1 - x0) * (y1 + y0)
    ccw = area2 < 0       # shoelace for CCW gives negative sum here

    indices = list(range(n))
    if not ccw:
        indices.reverse()

    triangles = []
    failsafe = 0
    while len(indices) > 3 and failsafe < n * n:
        failsafe += 1
        found = False
        for k in range(len(indices)):
            ia = indices[(k-1) % len(indices)]
            ib = indices[k]
            ic = indices[(k+1) % len(indices)]
            a, b, c = pts[ia], pts[ib], pts[ic]
            # Must be a CONVEX vertex (cross > 0 for CCW)
            if cross(a, b, c) <= 0:
                continue
            # Must contain no other polygon vertex
            ok = True
            for im in indices:
                if im in (ia, ib, ic): continue
                if point_in_tri(pts[im], a, b, c):
                    ok = False
                    break
            if not ok: continue
            triangles.append((ia, ib, ic))
            indices.pop(k)
            found = True
            break
        if not found:
            break
    if len(indices) == 3:
        triangles.append(tuple(indices))
    return triangles


def build_convergence_belt(bm, lower_ring, upper_ring):
    """Triangulate the strip between two rings where len(lower) == 2*len(upper).
    Each upper vertex T_i 'merges' lower vertices B_{2i} and B_{2i+1}. Produces
    exactly 3N triangles for N = len(upper). N-fold symmetric.

    Pattern per upper vertex i:
      1. (B_{2i},   B_{2i+1}, T_i)               — merge triangle
      2. (B_{2i+1}, B_{2i+2}, T_i)               — bottom-edge to next pair
      3. (B_{2i+2}, T_{i+1},  T_i)               — bridge to next upper vertex
    """
    N = len(upper_ring)
    M = len(lower_ring)
    assert M == 2 * N, f"convergence belt needs lower 2x upper, got {M} vs {N}"
    for i in range(N):
        t  = upper_ring[i]
        tn = upper_ring[(i + 1) % N]
        b0 = lower_ring[(2 * i)     % M]
        b1 = lower_ring[(2 * i + 1) % M]
        b2 = lower_ring[(2 * i + 2) % M]
        bm.faces.new([b0, b1, t])
        bm.faces.new([b1, b2, t])
        bm.faces.new([b2, tn, t])


def _gaussian_smooth_cyclic(pts, sigma_idx):
    """Smooth a cyclic point sequence in-place with Gaussian kernel in index units.
    Topology-preserving — each input index maps to one output index."""
    if sigma_idx <= 0.5:
        return list(pts)
    n = len(pts)
    half = max(1, int(sigma_idx * 3))
    weights = [math.exp(-(i*i)/(2*sigma_idx*sigma_idx)) for i in range(-half, half+1)]
    wsum = sum(weights)
    weights = [w/wsum for w in weights]
    out = []
    for i in range(n):
        sx = sy = 0.0
        for k, w in enumerate(weights):
            j = (i + k - half) % n
            sx += pts[j][0] * w
            sy += pts[j][1] * w
        out.append((sx, sy))
    return out


def build_inward_disc(bm, outer_ring, fold, z=0.0, target_edge=1.0):
    """Triangulate the disc inside outer_ring with concentric scaled-inward
    rings ending in a small N-gon + center vertex. Topology is N-fold
    symmetric iff len(outer_ring) is a multiple of fold.

    Inner rings get progressively Gaussian-smoothed so non-star outlines
    (like MOVE's hooks) don't crash into themselves at small scales. This
    is the same topology-preserving smoothing as hook_fade.
    Winding: when z=0 (body bottom), faces have downward (-z) normals.
    """
    n_theta = len(outer_ring)
    r_max = max(math.hypot(v.co.x, v.co.y) for v in outer_ring)
    # Pick ring count so radial spacing ≈ target_edge
    n_rings = max(3, int(math.ceil(r_max / target_edge)))
    rings = [outer_ring]
    base_pts_2d = [(v.co.x, v.co.y) for v in outer_ring]
    for i in range(1, n_rings):
        t = i / n_rings
        scale = 1.0 - t
        # Progressively smooth inner rings — at t=0 no smoothing, at t=1 max
        # smoothing. Sigma in index units, capped so small inner rings collapse
        # toward roughly circular (avoids self-intersection from non-star hooks).
        sigma_idx = (t * t) * (n_theta * 0.15)
        ring_2d = _gaussian_smooth_cyclic(base_pts_2d, sigma_idx)
        ring = [bm.verts.new((x * scale, y * scale, z)) for (x, y) in ring_2d]
        rings.append(ring)
    center = bm.verts.new((0, 0, z))

    bottom_winding = (z < 1e-3)   # for body's bottom face, normal points -z
    for i in range(len(rings) - 1):
        a, b = rings[i], rings[i+1]
        for j in range(n_theta):
            k = (j + 1) % n_theta
            if bottom_winding:
                bm.faces.new([a[j], b[j], b[k], a[k]])
            else:
                bm.faces.new([a[j], a[k], b[k], b[j]])
    last = rings[-1]
    for j in range(n_theta):
        k = (j + 1) % n_theta
        if bottom_winding:
            bm.faces.new([last[j], center, last[k]])
        else:
            bm.faces.new([last[j], last[k], center])


def parametric_body(name, polygon_r_func, z_max, shape='parabola',
                     n_theta=None, n_h=None, twist_func=None,
                     twist_deriv_func=None, target_edge=1.0, fold=1,
                     cyl_top=0.0):
    """Single continuous parametric construction. Samples the surface
    function (theta, h) → (x, y, z) on a uniform grid and triangulates directly.

    polygon_r_func: callable(theta) → distance from origin to polygon outline.
      Must be defined and continuous for all theta. For star-shaped polygons.
    shape: 'parabola' | 'hemisphere' | 'shallow' — selects the height curve.
    twist_func: optional callable(r) → angle to rotate around z-axis at radius r.
      Used to apply spiral to MOVE without changing the underlying star polygon.
    twist_deriv_func: optional callable(r) → dα/dr. Required if twist_func is
      given AND adaptive grid sizing is enabled (so the worst-case stretch can
      be solved for). If twist_func is given but this is None, falls back to
      a numerical derivative.
    target_edge: target maximum triangle edge length in mm. When n_theta/n_h
      are None, they are computed automatically to keep all triangles ≤ this
      size even at the highest-stretch location (the arm tip near the base,
      where r is largest, |s'(h)| is largest, and σ(r) = √(1 + r²α'(r)²) is
      largest). Pre-densifies in the radial direction to anticipate the
      tangential shear introduced by the twist operator.

    All five invariants satisfied by construction:
      1. Manifold topology (uniform grid → consistent connectivity)
      2. Top-down silhouette = polygon outline (sampled from polygon_r_func)
      3. Side profile = shape function (radial, rotationally symmetric in r)
      4. All triangles, uniform size (grid spacing controls size; adaptive
         n_theta/n_h pre-compensates for twist stretch)
      5. Smooth surface (adjacent grid points → adjacent vertices, by construction)
    """
    from mathutils.geometry import tessellate_polygon

    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()

    def scale_at_h(h):
        """Inverse of shape function: at height fraction h ∈ [0,1], what
        fraction of full radius? cyl_top extends the cylinder section (scale=1)
        from h=0 up to h=cyl_top, then applies the chosen shape function over
        the remaining [cyl_top, 1] range."""
        h = max(0.0, min(1.0, h))
        if h <= cyl_top:
            return 1.0
        t = (h - cyl_top) / max(1e-6, 1.0 - cyl_top)
        if shape == 'parabola':   return math.sqrt(1 - t)
        if shape == 'hemisphere': return math.sqrt(max(0, 1 - t*t))
        if shape == 'shallow':    return (1 - t) ** 0.25
        if shape == 'cosine':     return math.cos(t * math.pi / 2)
        raise ValueError(f"unknown shape {shape!r}")

    def transform(theta, r):
        """Apply twist operator: rotate (r, theta) by twist_func(r) around z-axis."""
        if twist_func is None:
            return theta
        return theta + twist_func(r)

    # Sample r_polygon over many theta to find the global max (arm tip radius)
    THETA_PROBE = 720
    r_samples = [polygon_r_func(2*math.pi*i/THETA_PROBE) for i in range(THETA_PROBE)]
    r_max = max(r_samples)
    r_min = min(r_samples)

    def twist_deriv(r):
        if twist_deriv_func is not None:
            return twist_deriv_func(r)
        if twist_func is None:
            return 0.0
        eps = 0.01
        return (twist_func(r + eps) - twist_func(r - eps)) / (2 * eps)

    # Slope of scale function at h=0 (where r is largest and edges are most stretched)
    def scale_slope_at(h):
        eps = 1e-4
        h0 = max(eps, min(1-eps, h))
        return abs((scale_at_h(h0 + eps) - scale_at_h(h0 - eps)) / (2 * eps))

    sigma_max = math.sqrt(1 + (r_max * twist_deriv(r_max))**2)
    s_slope = scale_slope_at(0.01)        # near base, where r is largest
    dr_dh   = r_max * s_slope             # change in radius per unit h at arm tip
    dz_dh   = z_max                       # change in z per unit h
    vertical_per_h = math.sqrt((dr_dh * sigma_max)**2 + dz_dh**2)

    # Auto-size n_theta and n_h to keep all edges ≤ target_edge
    if n_theta is None:
        n_theta = max(24, int(math.ceil(2 * math.pi * r_max / target_edge)))
    if n_h is None:
        n_h = max(8, int(math.ceil(vertical_per_h / target_edge)))
    # Round n_theta UP to fold × 2^k so apex coarsening can halve cleanly
    # all the way down to `fold` vertices. This is stricter than just being
    # a multiple of fold — it must be a power-of-2 multiple.
    if fold > 1:
        k = max(0, math.ceil(math.log2(max(1, n_theta) / fold)))
        n_theta = fold * (2 ** k)

    horizontal_at_arm = (2 * math.pi * r_max) / n_theta
    vertical_at_arm   = vertical_per_h / (n_h - 1)
    print(f"  [{name}] grid {n_theta}×{n_h}, r_max={r_max:.2f}, σ_max={sigma_max:.3f}")
    print(f"  [{name}] arm-tip edges: horizontal={horizontal_at_arm:.2f}mm, "
          f"vertical={vertical_at_arm:.2f}mm (target {target_edge}mm)")

    def make_ring(h, n_verts):
        """Build a ring of n_verts at height h, sampling polygon_r_func at
        uniformly-spaced thetas. Returns list of bmesh verts."""
        z = z_max * h
        s = scale_at_h(h)
        ring = []
        for ti in range(n_verts):
            theta = 2 * math.pi * ti / n_verts
            r_polygon = polygon_r_func(theta)
            r = r_polygon * s
            theta_eff = transform(theta, r)
            ring.append(bm.verts.new((r * math.cos(theta_eff),
                                       r * math.sin(theta_eff),
                                       z)))
        return ring

    # ── APEX COARSENING ───────────────────────────────────────────────────
    # Plan halvings: when scale halves, halve n_theta. This keeps angular
    # spacing 2π·s·r_max/n_theta roughly constant as we approach the apex.
    # Without this, triangles near the apex become long thin slivers and the
    # apex pole creates O(n_theta) sharp dihedrals.
    # Build a ring schedule: list of (h, n_theta_at_this_ring). Each ring
    # sits at its own h. At halving boundaries, the n_theta of the next ring
    # is halved — that ring pair becomes a convergence belt with the same
    # vertical extent as a regular quad strip (no horizontal "plate" rings).
    MAX_HALVINGS = 2
    do_coarsen = fold > 1 and n_theta >= fold * (2 ** MAX_HALVINGS)
    if do_coarsen:
        # Halvings live inside the TAPERING section [cyl_top, 1]. In the
        # cylinder section the body width is constant so vertex count should
        # not change. Powers of (2/3) progressively place halvings closer to
        # apex within the tapering region.
        halving_h = [cyl_top + (1 - cyl_top) * (1 - (2/3) ** (k + 1))
                     for k in range(MAX_HALVINGS)]
    else:
        halving_h = []
    print(f"  [{name}] {'coarsening' if do_coarsen else 'no coarsening'}, "
          f"cyl_top={cyl_top:.2f}, halvings at "
          f"h={[f'{h:.2f}' for h in halving_h] if halving_h else 'none'}")

    delta_h = max(target_edge / vertical_per_h, 1.0 / (n_h * 4))
    ring_schedule = []
    h = 0.0
    current_n = n_theta
    next_halving_idx = 0
    while h < 1.0 - delta_h / 2:
        ring_schedule.append((h, current_n))
        h += delta_h
        # Apply halving if we crossed a halving height
        if next_halving_idx < len(halving_h) and h >= halving_h[next_halving_idx]:
            current_n = max(fold, current_n // 2)
            next_halving_idx += 1
    # Last ring just below apex — use whatever n_theta the schedule ended at
    # (could be > fold if MAX_HALVINGS didn't fully reduce; that's OK, apex
    # cap just has more triangles).
    ring_schedule.append((1.0 - delta_h / 2, current_n))

    # Build all rings
    rings = [make_ring(h, n) for (h, n) in ring_schedule]

    # Connect consecutive rings: same n → quad strip; 2N → N → convergence belt
    for i in range(len(rings) - 1):
        a, b = rings[i], rings[i + 1]
        if len(a) == len(b):
            n_seg = len(a)
            for ti in range(n_seg):
                tj = (ti + 1) % n_seg
                bm.faces.new([a[ti], a[tj], b[tj], b[ti]])
        elif len(a) == 2 * len(b):
            build_convergence_belt(bm, a, b)
        else:
            raise ValueError(f"adjacent rings must have equal or 2:1 vertex counts, "
                             f"got {len(a)} → {len(b)}")

    # Apex cap from final ring (fold verts) to single apex point
    apex = bm.verts.new((0, 0, z_max))
    last = rings[-1]
    n_final = len(last)
    for ti in range(n_final):
        tj = (ti + 1) % n_final
        bm.faces.new([last[ti], last[tj], apex])

    # Bottom face: structured inward-ring triangulation (N-fold symmetric)
    build_inward_disc(bm, rings[0], fold=fold, z=0.0, target_edge=target_edge)

    bmesh.ops.triangulate(bm, faces=list(bm.faces), quad_method='BEAUTY', ngon_method='BEAUTY')
    bmesh.ops.recalc_face_normals(bm, faces=list(bm.faces))
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj
    obj.select_set(True)
    return obj


def parametric_body_polygon(name, polygon_pts, z_max, shape='parabola',
                             target_edge=1.0, twist_func=None,
                             twist_deriv_func=None,
                             twist_h_func=None, hook_fade_func=None,
                             envelope_r_func=None, fold=1,
                             bottom_strategy='rings', cyl_top=0.0,
                             apex_coarsen=True):
    """Parametric construction from a polygon point list (not r(θ) function).
    Supports non-star polygons (like MOVE's spiral with hook curls). The
    polygon is resampled at uniform arc length, then swept upward with
    radial scaling toward origin; optional twist is applied per-vertex
    as a post-transform.

    For star polygons, this is functionally equivalent to parametric_body
    but indexes by arc length instead of theta. For non-star polygons (hooks),
    arc-length sweep preserves the full outline including concavities.

    All five invariants still satisfied by construction:
      1. Continuous manifold topology
      2. Top-down silhouette = polygon at h=0 (exact)
      3. Side profile = shape function (radial scaling toward origin)
      4. Triangle uniformity controlled by target_edge
      5. Smooth surface (arc-length sampling avoids polar clustering)
    """
    from mathutils.geometry import tessellate_polygon

    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()

    # Compute cumulative arc length around the polygon
    n = len(polygon_pts)
    edge_lens = []
    for i in range(n):
        x1, y1 = polygon_pts[i]
        x2, y2 = polygon_pts[(i+1) % n]
        edge_lens.append(math.hypot(x2-x1, y2-y1))
    total_perim = sum(edge_lens)
    cum = [0.0]
    for L in edge_lens:
        cum.append(cum[-1] + L)

    def sample_at_s(s):
        """Return (x, y) on the polygon at arc length s ∈ [0, total_perim)."""
        s = s % total_perim
        # Binary search for which edge
        lo, hi = 0, n
        while lo < hi - 1:
            mid = (lo + hi) // 2
            if cum[mid] <= s: lo = mid
            else: hi = mid
        i = lo
        frac = (s - cum[i]) / edge_lens[i] if edge_lens[i] > 0 else 0.0
        x1, y1 = polygon_pts[i]
        x2, y2 = polygon_pts[(i+1) % n]
        return (x1 + frac*(x2-x1), y1 + frac*(y2-y1))

    r_max = max(math.hypot(x, y) for x, y in polygon_pts)

    def scale_at_h(h):
        """cyl_top extends a cylinder section (scale=1) from h=0 to h=cyl_top,
        then applies the chosen shape function over [cyl_top, 1]."""
        h = max(0.0, min(1.0, h))
        if h <= cyl_top:
            return 1.0
        t = (h - cyl_top) / max(1e-6, 1.0 - cyl_top)
        if shape == 'parabola':   return math.sqrt(1 - t)
        if shape == 'hemisphere': return math.sqrt(max(0, 1 - t*t))
        if shape == 'shallow':    return (1 - t) ** 0.25
        if shape == 'cosine':     return math.cos(t * math.pi / 2)
        raise ValueError(f"unknown shape {shape!r}")

    def twist_deriv(r):
        if twist_deriv_func is not None: return twist_deriv_func(r)
        if twist_func is None: return 0.0
        eps = 0.01
        return (twist_func(r+eps) - twist_func(r-eps)) / (2*eps)

    sigma_max = math.sqrt(1 + (r_max * twist_deriv(r_max))**2)
    # Per-h vertical edge length at the worst-case (arm tip, base)
    eps = 1e-4
    s_slope = abs((scale_at_h(eps) - scale_at_h(eps*3)) / (2*eps))
    vertical_per_h = math.sqrt((r_max * s_slope * sigma_max)**2 + z_max**2)

    n_s = max(48, int(math.ceil(total_perim / target_edge)))
    n_h = max(8, int(math.ceil(vertical_per_h / target_edge)))
    # Round n_s UP to fold × 2^k so apex coarsening halves cleanly all the way
    # down to `fold` arc samples.
    if fold > 1:
        k = max(0, math.ceil(math.log2(max(1, n_s) / fold)))
        n_s = fold * (2 ** k)

    print(f"  [{name}] polygon sweep: {n_s} arc-length samples × {n_h} rings, "
          f"perimeter={total_perim:.1f}mm, r_max={r_max:.2f}, σ_max={sigma_max:.3f}, "
          f"cyl_top={cyl_top:.2f}")

    def make_ring_at(h, n):
        """Build a ring of n arc-length samples at height h, with all
        post-transforms (smooth, scale, twist, twist_h) applied."""
        z = z_max * h
        s_factor = scale_at_h(h)
        twist_h_val = twist_h_func(h) if twist_h_func is not None else 0.0
        cH, sH = math.cos(twist_h_val), math.sin(twist_h_val)
        fade = hook_fade_func(h) if hook_fade_func is not None else 0.0
        ring_pts = [sample_at_s((si / n) * total_perim) for si in range(n)]
        sigma_idx = fade * (n * 0.08)
        ring_pts = _gaussian_smooth_cyclic(ring_pts, sigma_idx)
        ring = []
        for (x, y) in ring_pts:
            x *= s_factor; y *= s_factor
            if twist_func is not None:
                r = math.hypot(x, y)
                a = twist_func(r)
                ca, sa = math.cos(a), math.sin(a)
                x, y = x*ca - y*sa, x*sa + y*ca
            if twist_h_func is not None:
                x, y = x*cH - y*sH, x*sH + y*cH
            ring.append(bm.verts.new((x, y, z)))
        return ring

    # Ring schedule with apex coarsening (same pattern as parametric_body).
    # For NON-STAR polygons (with hooks etc.), coarsening's convergence belts
    # MERGE arc-length-neighboring vertices that may be far apart angularly —
    # the merge triangle spans the polygon's gap regions and fills them in the
    # top-down silhouette. Disable apex coarsening to preserve gap invariant.
    MAX_HALVINGS = 2
    do_coarsen = apex_coarsen and fold > 1 and n_s >= fold * (2 ** MAX_HALVINGS)
    if do_coarsen:
        halving_h = [cyl_top + (1 - cyl_top) * (1 - (2/3) ** (k + 1))
                     for k in range(MAX_HALVINGS)]
    else:
        halving_h = []

    delta_h = max(target_edge / vertical_per_h, 1.0 / (n_h * 4))
    ring_schedule = []
    h = 0.0
    current_n = n_s
    next_halving_idx = 0
    while h < 1.0 - delta_h / 2:
        ring_schedule.append((h, current_n))
        h += delta_h
        if next_halving_idx < len(halving_h) and h >= halving_h[next_halving_idx]:
            current_n = max(fold, current_n // 2)
            next_halving_idx += 1
    ring_schedule.append((1.0 - delta_h / 2, current_n))

    rings = [make_ring_at(h, n) for (h, n) in ring_schedule]

    for i in range(len(rings) - 1):
        a, b = rings[i], rings[i + 1]
        if len(a) == len(b):
            n_seg = len(a)
            for si in range(n_seg):
                sj = (si + 1) % n_seg
                bm.faces.new([a[si], a[sj], b[sj], b[si]])
        elif len(a) == 2 * len(b):
            build_convergence_belt(bm, a, b)
        else:
            raise ValueError(f"adjacent rings must have equal or 2:1 vertex counts, "
                             f"got {len(a)} → {len(b)}")

    apex = bm.verts.new((0, 0, z_max))
    last = rings[-1]
    n_final = len(last)
    for si in range(n_final):
        sj = (si + 1) % n_final
        bm.faces.new([last[si], last[sj], apex])

    # Bottom face: choose strategy.
    # 'rings'      = structured concentric inward rings; works for star polygons.
    # 'fill'       = bmesh.ops.triangle_fill — respects the polygon outline,
    #                doesn't bridge across non-star concavities. Use for MOVE
    #                where tessellate_polygon was creating 29mm edges spanning
    #                the spiral's gap regions and filling them in the silhouette.
    # 'fan' (legacy) = mathutils.tessellate_polygon — bridges concavities, breaks
    #                invariant 6 on non-star polygons.
    if bottom_strategy == 'rings':
        build_inward_disc(bm, rings[0], fold=fold, z=0.0, target_edge=target_edge)
    elif bottom_strategy == 'fill':
        # Strict ear clipping (custom): every triangle has its centroid AND
        # interior strictly inside the polygon. Avoids the spanning-triangle
        # issue of triangle_fill / EAR_CLIP on non-convex polygons.
        base_pts_2d = [(v.co.x, v.co.y) for v in rings[0]]
        tris = strict_ear_clip_indices(base_pts_2d)
        for (i, j, k) in tris:
            try:
                bm.faces.new([rings[0][k], rings[0][j], rings[0][i]])
            except ValueError:
                pass
    elif bottom_strategy == 'none':
        # Leave the body open at z=0 — no bottom face. Useful for verifying
        # the top-down silhouette without bottom-face spanning triangles
        # confusing the raycast.
        pass
    else:
        from mathutils.geometry import tessellate_polygon
        bot_3d = [v.co for v in rings[0]]
        for i1, i2, i3 in tessellate_polygon([bot_3d]):
            bm.faces.new([rings[0][i3], rings[0][i2], rings[0][i1]])

    bmesh.ops.triangulate(bm, faces=list(bm.faces), quad_method='BEAUTY', ngon_method='BEAUTY')
    bmesh.ops.recalc_face_normals(bm, faces=list(bm.faces))
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj
    obj.select_set(True)
    return obj


def icosphere_radial_project(target_obj, subdivisions=5):
    """Create an icosphere centered on the target's bounding box, then for each
    icosphere vertex, ray-cast FROM the body center OUTWARD along that vertex's
    direction to find the body surface intersection. Each vertex lands at a
    unique point on the surface (no clustering, unlike Shrinkwrap's
    nearest-surface-point which collapses many vertices onto the same spot).

    Works perfectly for star-shaped bodies (where every ray from center hits
    the surface exactly once). For non-star-shaped surfaces (e.g. MOVE's hook
    curl pockets), the ray skips over the concavity and hits the outer arm
    surface — so the hook pocket detail is smoothed away. Trade-off.
    """
    from mathutils import Vector
    from mathutils.bvhtree import BVHTree

    # Build BVH of target in world coords
    bm_t = bmesh.new()
    bm_t.from_object(target_obj, bpy.context.evaluated_depsgraph_get())
    bm_t.transform(target_obj.matrix_world)
    bvh = BVHTree.FromBMesh(bm_t)

    # Body bounding box → center for radial projection origin
    bb = [v.co for v in bm_t.verts]
    cx = (min(v.x for v in bb) + max(v.x for v in bb)) / 2
    cy = (min(v.y for v in bb) + max(v.y for v in bb)) / 2
    cz = (min(v.z for v in bb) + max(v.z for v in bb)) / 2
    center = Vector((cx, cy, cz))
    bm_t.free()

    # Create icosphere centered there, with bounding radius slightly larger than the body
    bounding_r = max(math.hypot(v.x-cx, v.y-cy) + abs(v.z-cz) for v in bb) + 2.0
    bpy.ops.mesh.primitive_ico_sphere_add(subdivisions=subdivisions,
                                           radius=bounding_r,
                                           location=(cx, cy, cz))
    ico = bpy.context.active_object
    ico.name = target_obj.name + "_ico"

    # For each icosphere vertex: ray from center in vertex's outward direction
    # → first hit on body surface
    EPSILON = 0.0001
    for v in ico.data.vertices:
        world_pos = ico.matrix_world @ v.co
        direction = (world_pos - center)
        d_len = direction.length
        if d_len < EPSILON:
            continue
        direction = direction / d_len
        hit, _, _, _ = bvh.ray_cast(center, direction)
        if hit is not None:
            # Move vertex to the hit point (in local space)
            v.co = ico.matrix_world.inverted() @ hit

    # Smoothing pass: blend each vertex toward the average of its neighbors. This
    # evens out the projection-induced wobble (icosphere vertices snap to slightly-
    # different-curvature body surface points, creating visible ridge feathering).
    # After smoothing, RE-PROJECT each vertex back onto the body surface for accuracy.
    bm = bmesh.new()
    bm.from_mesh(ico.data)
    for _ in range(3):
        bmesh.ops.smooth_vert(bm, verts=list(bm.verts), factor=0.5,
                               use_axis_x=True, use_axis_y=True, use_axis_z=True)
        # Re-project smoothed vertices back onto body surface
        for v in bm.verts:
            world_pos = ico.matrix_world @ v.co
            direction = (world_pos - center)
            d_len = direction.length
            if d_len < EPSILON: continue
            direction = direction / d_len
            hit, _, _, _ = bvh.ray_cast(center, direction)
            if hit is not None:
                v.co = ico.matrix_world.inverted() @ hit
    bmesh.ops.triangulate(bm, faces=list(bm.faces), quad_method='BEAUTY', ngon_method='BEAUTY')
    bmesh.ops.recalc_face_normals(bm, faces=list(bm.faces))
    bm.to_mesh(ico.data)
    bm.free()
    return ico


def voxel_remesh_uniform(obj, voxel_size=0.4):
    """Remesh the object's mesh at a uniform voxel resolution. Result: all triangles,
    roughly equal size (~voxel_size mm edge length). Manifold by construction.
    Voxel remesh produces quads, so we triangulate after."""
    obj.data.remesh_voxel_size = voxel_size
    obj.data.remesh_voxel_adaptivity = 0.0
    obj.data.use_remesh_fix_poles = True
    obj.data.use_remesh_preserve_volume = True
    obj.data.use_remesh_preserve_attributes = False
    bpy.context.view_layer.objects.active = obj
    bpy.ops.object.voxel_remesh()
    # Triangulate the quads from voxel remesh
    bm = bmesh.new()
    bm.from_mesh(obj.data)
    bmesh.ops.triangulate(bm, faces=list(bm.faces), quad_method='BEAUTY', ngon_method='BEAUTY')
    bm.to_mesh(obj.data)
    bm.free()


def build_polygon_paraboloid(name, polygon, z_max, shape='parabola', subsurf_levels=2):
    """Body = solid with polygon footprint and a rotationally-symmetric dome on top.

    shape selects the dome curve (centered at origin, r_max = polygon's farthest point):
      - 'parabola':   z = z_max × (1 − r²/r_max²)        (pointy spire — sharp apex)
      - 'hemisphere': z = z_max × √(1 − r²/r_max²)        (rounded dome — vertical edge)
      - 'shallow':    z = z_max × (1 − (r/r_max)⁴)         (flat-topped, sharp edge)

    Construction: simple cage of boundary + apex with fan triangulation. Then SUBDIVIDE
    via Catmull-Clark Subdivision Surface modifier (smooths apex from cone to dome),
    then PROJECT each vertex back onto the exact shape function for accuracy.

    This produces a smooth dome AND preserves topology cleanliness (subsurf is
    consistent across the entire mesh — no T-junctions).
    """
    from mathutils import Vector
    from mathutils.geometry import tessellate_polygon

    r_max = max(math.hypot(x, y) for x, y in polygon) * 1.0001

    def z_of(x, y):
        r = math.hypot(x, y)
        ratio = min(1.0, r / r_max)
        if shape == 'parabola':
            return z_max * (1 - ratio*ratio)
        elif shape == 'hemisphere':
            return z_max * math.sqrt(max(0.0, 1 - ratio*ratio))
        elif shape == 'shallow':
            return z_max * (1 - ratio**4)
        elif shape == 'cosine':
            return z_max * math.cos(ratio * math.pi / 2)
        else:
            raise ValueError(f"unknown shape {shape!r}")

    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()

    n_poly = len(polygon)
    bot_boundary = [bm.verts.new((x, y, 0)) for x, y in polygon]
    top_boundary = [bm.verts.new((x, y, z_of(x, y))) for x, y in polygon]
    apex = bm.verts.new((0, 0, z_max))

    # Bottom face: tessellated polygon, reversed normal points -Z
    polygon_3d = [Vector((x, y, 0)) for x, y in polygon]
    for i1, i2, i3 in tessellate_polygon([polygon_3d]):
        bm.faces.new([bot_boundary[i3], bot_boundary[i2], bot_boundary[i1]])

    # Top: fan from apex to consecutive boundary vertices
    for i in range(n_poly):
        j = (i + 1) % n_poly
        bm.faces.new([top_boundary[i], top_boundary[j], apex])

    # Side walls: bot ↔ top boundary
    for i in range(n_poly):
        j = (i + 1) % n_poly
        bm.faces.new([bot_boundary[i], bot_boundary[j], top_boundary[j], top_boundary[i]])

    bmesh.ops.triangulate(bm, faces=list(bm.faces), quad_method='BEAUTY', ngon_method='BEAUTY')
    bmesh.ops.recalc_face_normals(bm, faces=list(bm.faces))
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj
    obj.select_set(True)

    # Apply Subdivision Surface to smooth the apex cone into a rounded dome.
    # Subsurf preserves manifold topology automatically. Higher levels = smoother.
    # Note: subsurf approximates the parabolic curve via Catmull-Clark smoothing,
    # producing a NURBS-like surface that's close to but not exactly the chosen
    # shape function. For our purposes (visual distinction between parabola/dome/
    # shallow) this is good enough.
    if subsurf_levels > 0:
        mod = obj.modifiers.new("Subsurf", 'SUBSURF')
        mod.levels = subsurf_levels
        mod.render_levels = subsurf_levels
        bpy.ops.object.modifier_apply(modifier="Subsurf")

    # Re-triangulate after subsurf (subsurf produces quads — invariant 4 needs tris)
    bm2 = bmesh.new()
    bm2.from_mesh(obj.data)
    bmesh.ops.triangulate(bm2, faces=list(bm2.faces), quad_method='BEAUTY', ngon_method='BEAUTY')
    bm2.to_mesh(obj.data)
    bm2.free()

    return obj


def compute_inward_normals(polygon):
    """For each polygon vertex, return the unit inward normal (perpendicular
    to outline, pointing toward the polygon's interior). Detects polygon
    winding automatically via signed area."""
    n = len(polygon)
    # Signed area: positive = CCW, negative = CW
    a2 = sum(polygon[i][0]*polygon[(i+1)%n][1] - polygon[(i+1)%n][0]*polygon[i][1] for i in range(n))
    ccw = a2 > 0
    normals = []
    for i in range(n):
        prev = polygon[(i-1) % n]
        curr = polygon[i]
        nxt  = polygon[(i+1) % n]
        ex1, ey1 = curr[0] - prev[0], curr[1] - prev[1]
        ex2, ey2 = nxt[0]  - curr[0], nxt[1]  - curr[1]
        # Inward normal: rotate edge by +90° (CCW) or -90° (CW)
        if ccw:
            nx1, ny1 = -ey1, ex1
            nx2, ny2 = -ey2, ex2
        else:
            nx1, ny1 = ey1, -ex1
            nx2, ny2 = ey2, -ex2
        l1 = math.hypot(nx1, ny1); l2 = math.hypot(nx2, ny2)
        if l1 > 1e-9: nx1 /= l1; ny1 /= l1
        if l2 > 1e-9: nx2 /= l2; ny2 /= l2
        nx, ny = (nx1 + nx2)/2, (ny1 + ny2)/2
        l = math.hypot(nx, ny)
        if l > 1e-9: normals.append((nx/l, ny/l))
        else:        normals.append((nx1, ny1))
    return normals


def offset_polygon(polygon, d, inward_normals):
    """Offset polygon inward by distance d along per-vertex inward normals.
    Preserves polygon topology (including hook curl pockets) as long as
    d < the polygon's critical pocket-collapse distance."""
    return [(p[0] + n[0]*d, p[1] + n[1]*d) for p, n in zip(polygon, inward_normals)]


def build_offset_prism(name, outline_pts_2d, height, max_offset,
                        n_z=12, offset_curve='parabolic'):
    """Tapered prism via INWARD OFFSET (not uniform scaling). The polygon
    shrinks inward as we go up; max_offset is the inward distance at z=height.
    Polygon topology (including hook curl pockets) is preserved because each
    point moves perpendicular to the outline, not toward the origin.

    No apex point — the top is a smaller version of the polygon, closed by
    a tessellated face."""
    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()

    inward_normals = compute_inward_normals(outline_pts_2d)
    n_pts = len(outline_pts_2d)

    rings = []
    for iz in range(n_z + 1):
        t = iz / n_z   # 0 at base, 1 at top
        if offset_curve == 'parabolic':
            # Slow at start, fast at end — like a parabolic spire taper
            d = max_offset * (t * t)
        elif offset_curve == 'linear':
            d = max_offset * t
        else:
            d = max_offset * t  # default linear
        z = t * height
        offset_poly = offset_polygon(outline_pts_2d, d, inward_normals)
        ring = [bm.verts.new((p[0], p[1], z)) for p in offset_poly]
        rings.append(ring)

    # Side quads
    for i in range(len(rings) - 1):
        a, b = rings[i], rings[i+1]
        for j in range(n_pts):
            k = (j + 1) % n_pts
            bm.faces.new([a[j], a[k], b[k], b[j]])

    # Bottom face (tessellate the z=0 polygon)
    from mathutils.geometry import tessellate_polygon
    bot_3d = [v.co for v in rings[0]]
    for i1, i2, i3 in tessellate_polygon([bot_3d]):
        bm.faces.new([rings[0][i3], rings[0][i2], rings[0][i1]])

    # Top face (tessellate the offsetted polygon at z=height)
    top_3d = [v.co for v in rings[-1]]
    for i1, i2, i3 in tessellate_polygon([top_3d]):
        bm.faces.new([rings[-1][i1], rings[-1][i2], rings[-1][i3]])

    bmesh.ops.triangulate(bm, faces=list(bm.faces), quad_method='BEAUTY', ngon_method='BEAUTY')
    bmesh.ops.recalc_face_normals(bm, faces=list(bm.faces))
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj; obj.select_set(True)
    return obj


def get_outline_polygon(curve_obj, target_pts=128):
    """Convert SVG curve to mesh, return outline vertices in TRAVERSAL ORDER
    by reading the filled polygon's vertex loop. Subsample to ~target_pts to
    keep the resulting cage cage manageable for sculpting."""
    bpy.ops.object.select_all(action='DESELECT')
    curve_obj.select_set(True); bpy.context.view_layer.objects.active = curve_obj
    bpy.ops.object.convert(target='MESH')
    m = curve_obj.data
    if len(m.polygons) == 0:
        # No filled polygon → use vertex array order (sometimes works for closed paths)
        ordered = [curve_obj.matrix_world @ v.co for v in m.vertices]
    else:
        poly = max(m.polygons, key=lambda p: len(p.vertices))
        ordered = [curve_obj.matrix_world @ m.vertices[v].co for v in poly.vertices]
    n = len(ordered)
    if n > target_pts:
        stride = n // target_pts
        ordered = [ordered[i] for i in range(0, n, stride)]
    return ordered


def build_swept_extrusion(name, outline_pts_2d, body_height, top_shape, top_param,
                          n_z_body=10, n_z_top=10):
    """Sweep a 2D polygon outline straight up to body_height, then scale it down
    over the cap (varying z by top_shape). Preserves the polygon's exact shape
    (including non-star-shaped features like MOVE's spiral hooks)."""
    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()
    n_pts = len(outline_pts_2d)

    # Body rings — outline at constant scale, varying z.
    # If body_height is 0, skip the body section (single base ring; cap takes
    # over the full piece) — this is what MOVE uses for a pure-parabola spire.
    body_rings = []
    if body_height <= 1e-6:
        ring = [bm.verts.new((p[0], p[1], 0)) for p in outline_pts_2d]
        body_rings.append(ring)
    else:
        for iz in range(n_z_body + 1):
            z = (iz / n_z_body) * body_height
            ring = [bm.verts.new((p[0], p[1], z)) for p in outline_pts_2d]
            body_rings.append(ring)

    # Top cap rings — outline scaled toward 0, z varies with cap shape
    top_rings = []
    for iz in range(1, n_z_top):
        t = iz / n_z_top
        scale = (1 - t)
        if top_shape == 'bowl':
            z_offset = -top_param * (1 - (1-t)**2)
        elif top_shape == 'parabola':
            z_offset =  top_param * (1 - (1-t)**2)
        elif top_shape == 'hemisphere':
            z_offset =  top_param * math.sqrt(max(0.0, 1 - (1-t)**2))
        else:
            raise ValueError(f"unknown top_shape {top_shape!r}")
        z = body_height + z_offset
        ring = [bm.verts.new((p[0]*scale, p[1]*scale, z)) for p in outline_pts_2d]
        top_rings.append(ring)

    apex_z = body_height + (-top_param if top_shape == 'bowl' else top_param)
    apex = bm.verts.new((0, 0, apex_z))

    all_rings = body_rings + top_rings

    # Quads between rings
    for i in range(len(all_rings) - 1):
        a, b = all_rings[i], all_rings[i+1]
        for j in range(n_pts):
            k = (j + 1) % n_pts
            bm.faces.new([a[j], a[k], b[k], b[j]])

    # Apex fan (unavoidable triangles converging at apex)
    last = all_rings[-1]
    for j in range(n_pts):
        k = (j + 1) % n_pts
        bm.faces.new([last[j], last[k], apex])

    # Bottom — explicitly tessellate the 2D polygon via mathutils, which
    # handles arbitrary non-convex (including concave/spiral) polygons correctly.
    # Using bm.faces.new(ring) for a deeply non-convex polygon can silently
    # be auto-corrected to the convex hull.
    from mathutils.geometry import tessellate_polygon
    polygon_3d = [v.co for v in body_rings[0]]
    triangles = tessellate_polygon([polygon_3d])
    for i1, i2, i3 in triangles:
        # Reverse winding so the bottom face's normal points DOWN (out of the body)
        bm.faces.new([body_rings[0][i3], body_rings[0][i2], body_rings[0][i1]])

    # Triangulate all quads explicitly so each triangle has a well-defined,
    # consistent normal. Non-planar quads (which the cap has — corners at
    # different z and different scales) get auto-triangulated by the renderer
    # with potentially-inconsistent diagonals, and a triangle can end up
    # back-facing. Doing it ourselves with consistent diagonals avoids that.
    bmesh.ops.triangulate(bm, faces=list(bm.faces), quad_method='BEAUTY', ngon_method='BEAUTY')

    # Recalc normals so every face points OUTWARD regardless of polygon winding.
    bmesh.ops.recalc_face_normals(bm, faces=list(bm.faces))

    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj
    obj.select_set(True)
    return obj


def build_extrusion_piece(name, silhouette, body_height, top_shape, top_param,
                          n_theta=64, n_z_body=10, n_z_top=10):
    """Silhouette extruded straight up to body_height, then a curved top cap.

    top_shape:
      'bowl'       — top dips IN (concave bowl). apex_z = body_height - top_param
      'parabola'   — top rises in a parabolic dome. apex_z = body_height + top_param
      'hemisphere' — top rises as a hemisphere-like dome. apex_z = body_height + top_param

    Result: clean all-quads body + cap that tapers to a single apex vertex.
    No connector. No booleans. No interior geometry. Sculpt-ready by construction.
    """
    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()

    # Body rings — silhouette extruded straight up
    body_rings = []
    for iz in range(n_z_body + 1):
        z = (iz / n_z_body) * body_height
        ring = []
        for i in range(n_theta):
            theta = 2*pi*i/n_theta
            r = silhouette.r_at(theta)
            ring.append(bm.verts.new((r*cos(theta), r*sin(theta), z)))
        body_rings.append(ring)

    # Top cap rings — silhouette shrinks toward 0 as we approach apex
    top_rings = []
    for iz in range(1, n_z_top):
        t = iz / n_z_top    # 0 just above rim → 1 at apex
        r_scale = (1 - t)
        if top_shape == 'bowl':
            z_offset = -top_param * (1 - (1-t)**2)
        elif top_shape == 'parabola':
            z_offset =  top_param * (1 - (1-t)**2)
        elif top_shape == 'hemisphere':
            z_offset =  top_param * math.sqrt(max(0.0, 1 - (1-t)**2))
        else:
            raise ValueError(f"unknown top_shape {top_shape!r}")
        z = body_height + z_offset
        ring = []
        for i in range(n_theta):
            theta = 2*pi*i/n_theta
            r = silhouette.r_at(theta) * r_scale
            ring.append(bm.verts.new((r*cos(theta), r*sin(theta), z)))
        top_rings.append(ring)

    # Apex vertex
    if top_shape == 'bowl':
        apex_z = body_height - top_param
    else:
        apex_z = body_height + top_param
    apex = bm.verts.new((0, 0, apex_z))

    all_rings = body_rings + top_rings

    # Quads between consecutive rings
    for i in range(len(all_rings) - 1):
        a, b = all_rings[i], all_rings[i+1]
        for ith in range(n_theta):
            j = (ith+1) % n_theta
            bm.faces.new([a[ith], a[j], b[j], b[ith]])

    # Triangles from last ring to apex
    last = all_rings[-1]
    for ith in range(n_theta):
        j = (ith+1) % n_theta
        bm.faces.new([last[ith], last[j], apex])

    # Flat bottom cap (single n-gon — fine for sculpting, becomes quads on subdivide)
    bm.faces.new(body_rings[0][::-1])

    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj
    obj.select_set(True)
    return obj


def build_loft(name, height, profile_fn, n_theta=N_THETA, n_z=N_Z, cap_bottom=True,
               z_bottom_fn=None):
    """profile_fn(theta, z_norm) → r at that angular & vertical location.
    Optional z_bottom_fn(theta) → z value for the bottom ring at each angle;
    this is how leg arches are encoded (bottom ring lifts off the ground at
    between-leg angles, touches z=0 at leg angles), without requiring booleans.
    """
    me = bpy.data.meshes.new(name)
    obj = bpy.data.objects.new(name, me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()
    rings = []
    for iz in range(n_z+1):
        z_norm = iz/n_z
        ring = []
        for ith in range(n_theta):
            theta = 2*pi * ith / n_theta
            # Bottom-ring z varies with theta if z_bottom_fn provided.
            # Higher rings inherit a fraction of this lift, fading to 0 by mid-height.
            if z_bottom_fn is not None:
                lift = z_bottom_fn(theta)
                # Smoothly blend the lift away as we move up
                lift_blend = (1 - z_norm)**2 if z_norm < 1 else 0
                z = lift * lift_blend + z_norm * (height - lift * lift_blend)
            else:
                z = z_norm * height
            r = profile_fn(theta, z_norm)
            ring.append(bm.verts.new((r*cos(theta), r*sin(theta), z)))
        rings.append(ring)
    for iz in range(n_z):
        a = rings[iz]; b = rings[iz+1]
        for ith in range(n_theta):
            j = (ith+1) % n_theta
            bm.faces.new([a[ith], a[j], b[j], b[ith]])
    if cap_bottom:
        bm.faces.new(rings[0][::-1])
    bm.faces.new(rings[-1])
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj; obj.select_set(True)
    return obj


def shade_smooth(obj):
    bpy.ops.object.select_all(action='DESELECT'); obj.select_set(True)
    bpy.context.view_layer.objects.active = obj
    bpy.ops.object.shade_smooth()


def fillet_edges(obj, width, segments=3, angle_deg=20):
    bpy.context.view_layer.objects.active = obj
    m = obj.modifiers.new("Bevel", 'BEVEL')
    m.width = width; m.segments = segments
    m.limit_method = 'ANGLE'; m.angle_limit = math.radians(angle_deg)
    bpy.ops.object.modifier_apply(modifier="Bevel")


def voxel_remesh(obj, voxel_size=0.7):
    """Cleans boolean artifacts and produces uniform sculpt-ready topology.
    Output is manifold, single-shell, ~uniform face density."""
    bpy.ops.object.select_all(action='DESELECT'); obj.select_set(True)
    bpy.context.view_layer.objects.active = obj
    # Use the mesh's voxel_size property + remesh operator (Blender 3+/4+/5)
    obj.data.remesh_voxel_size = voxel_size
    obj.data.remesh_voxel_adaptivity = 0.0
    obj.data.use_remesh_fix_poles = True
    obj.data.use_remesh_preserve_volume = True
    bpy.ops.object.voxel_remesh()
    n_polys = len(obj.data.polygons)
    print(f"    voxel_remesh: {n_polys} polys at voxel_size={voxel_size}mm")


# ===================================================================
# Pieces
# ===================================================================

def _subsample(polygon, target_n=64):
    """Stride-subsample a polygon to reduce vertex count. Preserves the overall
    shape while reducing the number of "fan spokes" that propagate to the apex
    in the paraboloid construction, dramatically reducing the visible pleat count."""
    if len(polygon) <= target_n:
        return polygon
    indices = sorted({int(i * len(polygon) / target_n) for i in range(target_n)})
    return [polygon[i] for i in indices]


def build_eat(polygon, silhouette):
    """5-fold star + shallow dome. SVG-direct polygon (perfect silhouette match)."""
    print("\n=== EAT (SVG polygon, cylinder 7/8 + small hemispherical cap) ===")
    obj = parametric_body("EAT", silhouette.r_at, z_max=EAT_HEIGHT,
                           shape='hemisphere', fold=5, cyl_top=7/8)
    shade_smooth(obj)
    export(obj, "EAT.obj")
    save_connector_meta("EAT", EAT_HEIGHT, has_socket=False)
    return obj, EAT_HEIGHT


def build_move(polygon, silhouette):
    """3-fold spiral with hook curls + parabolic spire. Uses the SVG-extracted
    polygon directly (with all hook curl points) and sweeps it by arc length —
    handles the non-star outline that simple r(θ) parametrization cannot.

    No additional twist applied: the polygon already encodes the spiral
    character. The body is a vertical sweep of the spiral footprint with
    parabolic scaling toward an apex at z=MOVE_HEIGHT.
    """
    print("\n=== MOVE (SVG polygon + twist_h + hook_fade + parabolic spire) ===")
    # Pure parabola from base to apex — the side profile follows the parabolic
    # curve at every viewing angle (as much as the polygon hook indents allow).
    # Legs/ridges (polygon shape) propagate from base through the body via the
    # corkscrew twist; hooks fade with height to keep the surface continuous.
    # No twist_h: the SVG polygon already encodes the spiral character via its
    # curving arms + hook curls. Adding a body-twist on top makes higher rings
    # rotate around z, which from above sweeps the arms tangentially and breaks
    # the top-down silhouette invariant. Pure parabolic sweep keeps the
    # silhouette equal to the SVG at every projection angle.
    # No hook_fade — hooks scale uniformly with the body, max-r follows the
    # parabola scale exactly at every height.
    obj = parametric_body_polygon("MOVE", polygon, z_max=MOVE_HEIGHT,
                                   shape='parabola', target_edge=1.0,
                                   envelope_r_func=silhouette.r_at,
                                   fold=3, bottom_strategy='none',
                                   cyl_top=0.0,
                                   apex_coarsen=False)
    shade_smooth(obj)
    export(obj, "MOVE.obj")
    save_connector_meta("MOVE", MOVE_HEIGHT, has_socket=True)
    return obj, MOVE_HEIGHT


def build_grow(polygon, silhouette):
    """4-fold quatrefoil + hemisphere dome. SVG-direct polygon."""
    print("\n=== GROW (SVG polygon, cylinder 1/2 + hemisphere top half) ===")
    obj = parametric_body("GROW", silhouette.r_at, z_max=GROW_HEIGHT,
                           shape='hemisphere', fold=4, cyl_top=0.5)
    shade_smooth(obj)
    export(obj, "GROW.obj")
    save_connector_meta("GROW", GROW_HEIGHT, has_socket=True)
    return obj, GROW_HEIGHT


def check_topology(obj_path, name, sharp_dihedral_deg=45.0,
                    aspect_threshold=8.0, render_diag=True):
    """Continuity audit. Imports an OBJ and checks for:
      - boundary edges (holes in surface)
      - non-manifold edges (edge shared by >2 faces)
      - tiny faces (degenerate area)
      - sliver triangles (aspect ratio > threshold = thin, render-poor)
      - sharp dihedral angles (creases — adjacent face normals diverge sharply)
      - self-intersections (BVH self-overlap)

    Renders a diagnostic PNG with problem edges highlighted in red when
    render_diag is True, alongside a topology pass/fail summary.
    """
    import bmesh as _bmesh
    from mathutils.bvhtree import BVHTree

    reset()
    try:
        bpy.ops.wm.obj_import(filepath=str(obj_path),
                              forward_axis='NEGATIVE_Z', up_axis='Y')
    except AttributeError:
        bpy.ops.import_scene.obj(filepath=str(obj_path))
    obj = [o for o in bpy.context.scene.objects if o.type == 'MESH'][0]

    bm = _bmesh.new()
    bm.from_mesh(obj.data)
    bm.edges.ensure_lookup_table()
    bm.faces.ensure_lookup_table()

    n_verts = len(bm.verts)
    n_faces = len(bm.faces)
    boundary = [e for e in bm.edges if len(e.link_faces) == 1]
    nonmanifold = [e for e in bm.edges if len(e.link_faces) > 2]
    tiny = [f for f in bm.faces if f.calc_area() < 1e-3]

    # Sliver triangles: aspect ratio = (longest edge)² / (4√3 × area)
    # Equilateral triangle has aspect=1; thin slivers have aspect >> 1
    slivers = []
    for f in bm.faces:
        if len(f.verts) != 3: continue
        a = f.calc_area()
        if a < 1e-6: continue
        longest = max(e.calc_length() for e in f.edges)
        aspect = (longest * longest) / (4 * math.sqrt(3) * a)
        if aspect > aspect_threshold:
            slivers.append((f, aspect))

    # Invariant 6: no long edges. An edge much longer than the target spans
    # across a design feature that shouldn't be bridged (silhouette-breaking).
    target_edge = 1.0
    long_edge_threshold = 1.5 * target_edge
    long_edges = [(e, e.calc_length()) for e in bm.edges
                  if e.calc_length() > long_edge_threshold]

    # Sharp dihedral: angle between adjacent face normals.
    # The OBJ axis swizzle puts the body's height on the +Y axis after import,
    # not Z. Detect the height axis dynamically: it's the one with the most
    # positive bias (min ≈ 0, max ≈ z_max for our pieces).
    bm.normal_update()
    axis_ranges = []
    for ax in range(3):
        vals = [v.co[ax] for v in bm.verts]
        lo, hi = min(vals), max(vals)
        # Score = positive bias (asymmetry around 0)
        bias = (lo + hi) / 2 if (hi - lo) > 1e-3 else 0
        axis_ranges.append((ax, lo, hi, bias))
    # Height axis = most positive bias (others are roughly symmetric around 0)
    h_axis, h_min, h_max_w, _ = max(axis_ranges, key=lambda x: x[3])
    apex_band = 0.02 * (h_max_w - h_min)        # top 2% by height
    sharp_edges = []          # all sharp edges (legacy total)
    sharp_body = []
    sharp_rim = []
    sharp_apex = []
    threshold_rad = math.radians(sharp_dihedral_deg)
    for e in bm.edges:
        if len(e.link_faces) != 2: continue
        f1, f2 = e.link_faces
        n1, n2 = f1.normal, f2.normal
        d = max(-1.0, min(1.0, n1.dot(n2)))
        angle = math.acos(d)
        if angle <= threshold_rad: continue
        sharp_edges.append((e, math.degrees(angle)))
        # Classify against the detected height axis
        ev_h = [v.co[h_axis] for v in e.verts]
        is_rim = (max(ev_h) - h_min) < 0.05 and (abs(n1[h_axis]) > 0.9 or abs(n2[h_axis]) > 0.9)
        is_apex = any((h_max_w - h) < apex_band for h in ev_h)
        if is_rim:
            sharp_rim.append((e, math.degrees(angle)))
        elif is_apex:
            sharp_apex.append((e, math.degrees(angle)))
        else:
            sharp_body.append((e, math.degrees(angle)))

    # Self-intersection via BVH self-overlap
    bvh = BVHTree.FromBMesh(bm)
    self_overlaps = bvh.overlap(bvh)
    # Filter out trivial face-with-itself and adjacent (shared-vertex) pairs
    real_overlaps = []
    for i, j in self_overlaps:
        if i >= j: continue
        fi = bm.faces[i]; fj = bm.faces[j]
        if set(fi.verts) & set(fj.verts): continue   # share a vert → adjacent
        real_overlaps.append((i, j))

    # Print report
    lines = []
    lines.append(f"  TOPOLOGY [{name}]: {n_verts}v {n_faces}f")
    lines.append(f"    boundary edges:     {len(boundary):>6}  {'OK' if not boundary else 'FAIL'}")
    lines.append(f"    non-manifold edges: {len(nonmanifold):>6}  {'OK' if not nonmanifold else 'FAIL'}")
    lines.append(f"    tiny faces (<1e-3): {len(tiny):>6}  {'OK' if not tiny else 'WARN'}")
    lines.append(f"    sliver triangles (aspect>{aspect_threshold}): {len(slivers):>6}  {'OK' if not slivers else 'WARN'}")
    lines.append(f"    long edges (>{long_edge_threshold:.1f}mm = 1.5×target): {len(long_edges):>6}  {'OK' if not long_edges else 'FAIL — bridging features'}")
    if long_edges:
        worst = sorted(long_edges, key=lambda x: -x[1])[:3]
        lines.append(f"      worst edge lengths: " + ", ".join(f"{L:.1f}mm" for _, L in worst))
    lines.append(f"    sharp dihedrals (>{sharp_dihedral_deg}°): {len(sharp_edges):>6}  total")
    lines.append(f"      ├─ body interior:    {len(sharp_body):>6}  {'OK' if not sharp_body else 'WARN'}  ← real surface creases")
    lines.append(f"      ├─ bottom rim (intentional 90°): {len(sharp_rim):>6}")
    lines.append(f"      └─ apex pole (intentional):       {len(sharp_apex):>6}")
    lines.append(f"    self-intersections: {len(real_overlaps):>6}  {'OK' if not real_overlaps else 'FAIL'}")
    if sharp_edges:
        worst = sorted(sharp_edges, key=lambda x: -x[1])[:3]
        lines.append(f"      worst dihedrals: " + ", ".join(f"{a:.0f}°" for _, a in worst))
    if slivers:
        worst = sorted(slivers, key=lambda x: -x[1])[:3]
        lines.append(f"      worst aspect ratios: " + ", ".join(f"{a:.0f}" for _, a in worst))
    for line in lines:
        print(line)

    # Diagnostic render: highlight problem regions
    if render_diag:
        # Mark problem faces with a vertex color, then render
        bm_diag = _bmesh.new()
        bm_diag.from_mesh(obj.data)
        bm_diag.faces.ensure_lookup_table()
        # Make a copy mesh where problem faces get isolated → render with red overlay
        problem_face_idxs = set()
        for e, _ in sharp_edges:
            for f in e.link_faces:
                problem_face_idxs.add(f.index)
        for i, j in real_overlaps:
            problem_face_idxs.add(i); problem_face_idxs.add(j)
        for f, _ in slivers:
            problem_face_idxs.add(f.index)

        # Tag problem faces by separating them into a second object
        if problem_face_idxs:
            problem_bm = _bmesh.new()
            problem_verts = {}
            for fi in problem_face_idxs:
                f = bm_diag.faces[fi]
                vs = []
                for v in f.verts:
                    if v.index not in problem_verts:
                        problem_verts[v.index] = problem_bm.verts.new(v.co)
                    vs.append(problem_verts[v.index])
                try:
                    problem_bm.faces.new(vs)
                except Exception:
                    pass
            prob_me = bpy.data.meshes.new(f"{name}_problems")
            problem_bm.to_mesh(prob_me); problem_bm.free()
            prob_obj = bpy.data.objects.new(f"{name}_problems", prob_me)
            bpy.context.collection.objects.link(prob_obj)
            # Push problem mesh outward slightly so it renders on top
            mat = bpy.data.materials.new(f"{name}_red")
            mat.diffuse_color = (1, 0.15, 0.15, 1)
            prob_obj.data.materials.append(mat)

        bm_diag.free()

        # Workbench render iso view
        s = bpy.context.scene
        s.render.engine = 'BLENDER_WORKBENCH'
        sh = s.display.shading
        sh.light='STUDIO'; sh.color_type='OBJECT'; sh.show_cavity=True
        s.render.film_transparent = False
        s.render.resolution_x = 800; s.render.resolution_y = 800

        bb = [obj.matrix_world @ v.co for v in obj.data.vertices]
        zs = [v.z for v in bb]; xs = [v.x for v in bb]; ys = [v.y for v in bb]
        h = max(zs) - min(zs); cx = (max(xs)+min(xs))/2; cy = (max(ys)+min(ys))/2
        span = max(40, h) * 1.4
        cam_data = bpy.data.cameras.new("diag_cam"); cam_data.type='ORTHO'
        cam_data.ortho_scale = span; cam_data.clip_start=0.1; cam_data.clip_end=5000
        cam = bpy.data.objects.new("diag_cam", cam_data)
        bpy.context.collection.objects.link(cam)
        cam.location = (cx + span, cy - span, (max(zs)+min(zs))/2 + h*0.4)
        look = Vector((cx-cam.location.x, cy-cam.location.y, (max(zs)+min(zs))/2 - cam.location.z))
        cam.rotation_euler = look.to_track_quat('-Z','Y').to_euler()
        s.camera = cam
        out = RENDERS / f"{name}_topology.png"
        s.render.filepath = str(out)
        bpy.ops.render.render(write_still=True)
        print(f"    → {out.name}  ({len(problem_face_idxs)} problem faces highlighted in red)")

    bm.free()
    return {
        'boundary': len(boundary),
        'nonmanifold': len(nonmanifold),
        'tiny': len(tiny),
        'slivers': len(slivers),
        'sharp': len(sharp_edges),
        'self_intersect': len(real_overlaps),
    }


def export(obj, fname):
    bpy.ops.object.select_all(action='DESELECT')
    obj.select_set(True); bpy.context.view_layer.objects.active = obj
    bpy.ops.wm.obj_export(filepath=str(OUT/fname), export_selected_objects=True,
                          forward_axis='NEGATIVE_Z', up_axis='Y', export_materials=False)
    print(f"  → {fname}")


_CONNECTOR_META = {}

def save_connector_meta(name, peg_top_z, has_socket=True):
    """Record where the connector goes per-piece so graft_connector.py can re-attach
    after the user sculpts. peg_top_z is z of the plateau where the dome BASE sits."""
    _CONNECTOR_META[name] = {"peg_top_z": float(peg_top_z), "has_socket": bool(has_socket)}


def write_connector_meta():
    import json
    p = OUT / "connector_meta.json"
    with open(p, 'w') as f:
        json.dump(_CONNECTOR_META, f, indent=2)
    print(f"  → connector_meta.json")


# ===================================================================
# Rendering
# ===================================================================

def setup_workbench():
    s = bpy.context.scene
    s.render.engine = 'BLENDER_WORKBENCH'
    sh = s.display.shading
    sh.light='STUDIO'; sh.color_type='SINGLE'
    sh.single_color = (0.78, 0.74, 0.68)
    sh.show_cavity = True; sh.show_shadows = True
    sh.show_backface_culling = False
    # Opaque colored background so any "transparent" pixels are clearly background-leaked
    s.render.film_transparent = False
    sh.background_type = 'VIEWPORT'
    sh.background_color = (1.0, 1.0, 1.0)
    s.render.resolution_x = 900
    s.render.resolution_y = 900


def add_cam(loc, look, scale, name):
    d = bpy.data.cameras.new(name); d.type='ORTHO'; d.ortho_scale=scale
    d.clip_start=0.1; d.clip_end=5000
    c = bpy.data.objects.new(name, d); bpy.context.collection.objects.link(c)
    c.location = loc
    dirv = Vector((look[0]-loc[0], look[1]-loc[1], look[2]-loc[2]))
    c.rotation_euler = dirv.to_track_quat('-Z','Y').to_euler()
    return c


def render_piece_views(name, height, suffix="V2"):
    setup_workbench()
    s = bpy.context.scene
    span = max(40, height) * 1.4
    side = add_cam((0, -span*2, height/2),        (0, 0, height/2), span,        f"{name}_s")
    top  = add_cam((0, 0, height + span*2),       (0, 0, height/2), span,        f"{name}_t")
    iso  = add_cam((span, -span, height*0.9),     (0, 0, height*0.45), span*1.2, f"{name}_i")
    low  = add_cam((span*1.2, -span*1.2, 8),      (0, 0, 4),        span*0.8,    f"{name}_l")
    under= add_cam((0, 0, -span),                 (0, 0, 0),        span,        f"{name}_u")
    for cam, suf in [(side, "side"), (top, "top"), (iso, "iso"), (low, "low"), (under, "under")]:
        s.camera = cam
        s.render.filepath = str(RENDERS/f"{name}_{suffix}_{suf}.png")
        bpy.ops.render.render(write_still=True)

    # Diagnostic: top render with FLAT shading + PINK background.
    sh = s.display.shading
    sh.light = 'FLAT'
    sh.show_cavity = False
    sh.show_shadows = False
    sh.background_type = 'VIEWPORT'
    sh.background_color = (1.0, 0.4, 0.7)
    s.camera = top
    s.render.filepath = str(RENDERS / f"{name}_{suffix}_top_flat.png")
    bpy.ops.render.render(write_still=True)

    # Diagnostic: x-ray top render
    sh.show_xray = True
    sh.xray_alpha = 0.4
    s.render.filepath = str(RENDERS / f"{name}_{suffix}_top_xray.png")
    bpy.ops.render.render(write_still=True)
    sh.show_xray = False

    # Diagnostic: render with EEVEE engine (different backface handling than Workbench)
    s.render.engine = 'BLENDER_EEVEE_NEXT' if 'BLENDER_EEVEE_NEXT' in {e.identifier for e in bpy.types.RenderSettings.bl_rna.properties['engine'].enum_items} else 'BLENDER_EEVEE'
    s.render.filepath = str(RENDERS / f"{name}_{suffix}_top_eevee.png")
    bpy.ops.render.render(write_still=True)
    s.render.engine = 'BLENDER_WORKBENCH'

    # Restore
    sh.light = 'STUDIO'
    sh.show_cavity = True
    sh.show_shadows = True
    sh.background_type = 'THEME'


def render_cage_views(name, height):
    """Render the CAGE: iso, top-down, AND from below.
    The from-below view shows the actual silhouette unambiguously because
    we're looking through the base directly."""
    setup_workbench()
    s = bpy.context.scene
    sh = s.display.shading
    sh.show_object_outline = True
    span = max(40, height) * 1.4
    iso   = add_cam((span, -span, height*0.9), (0, 0, height*0.45), span*1.2, f"{name}_cage_i")
    top   = add_cam((0, 0, height + span*2),   (0, 0, height/2),    span,    f"{name}_cage_t")
    # Below: camera below the piece, looking up. Sees the base silhouette directly.
    below = add_cam((0, 0, -span),             (0, 0, height/4),    span,    f"{name}_cage_b")
    for cam, suf in [(iso, "iso"), (top, "top"), (below, "below")]:
        s.camera = cam
        s.render.filepath = str(RENDERS/f"{name}_cage_{suf}.png")
        bpy.ops.render.render(write_still=True)


# ===================================================================
# Main
# ===================================================================

def build_show_scene():
    """Load all three exported OBJs side-by-side and save as .blend + .glb
    so the user can open and rotate them interactively."""
    reset()
    SPACING = 60.0   # mm between pieces, centered
    positions = {
        "EAT.obj":  (-SPACING, 0, 0),
        "MOVE.obj": (0, 0, 0),
        "GROW.obj": ( SPACING, 0, 0),
    }
    for fname, (x, y, z) in positions.items():
        path = OUT / fname
        if not path.exists():
            print(f"  ! skipping {fname}: not found")
            continue
        bpy.ops.wm.obj_import(filepath=str(path), forward_axis='NEGATIVE_Z', up_axis='Y')
        # The OBJ import creates an object; locate it (most recently added mesh)
        obj = bpy.context.view_layer.objects.active
        if obj is None or obj.type != 'MESH':
            meshes = [o for o in bpy.context.scene.objects if o.type == 'MESH']
            obj = meshes[-1] if meshes else None
        if obj:
            obj.location = (x, y, z)
            bpy.ops.object.transform_apply(location=True)
            print(f"  + placed {fname} at x={x}")

    # Save .blend
    blend_path = OUT / "pieces.blend"
    bpy.ops.wm.save_as_mainfile(filepath=str(blend_path))
    print(f"  → {blend_path}")

    # Export GLB for any web viewer (drag into https://gltf-viewer.donmccurdy.com/ etc.)
    glb_path = OUT / "pieces.glb"
    bpy.ops.export_scene.gltf(filepath=str(glb_path), export_format='GLB',
                              use_selection=False)
    print(f"  → {glb_path}")


def render_silhouette_comparison(polygons):
    """Render each polygon two ways:
    (a) as a filled n-gon at zoom matching the piece's actual top render
    (b) as a wireframe (edges only) at the same zoom.
    Same camera settings as MOVE_V2_top so visual comparison is apples-to-apples."""
    import bmesh as bm_mod
    for name, polygon in polygons.items():
        # (a) Filled n-gon (single face)
        reset()
        me = bpy.data.meshes.new(f"{name}_svg_filled")
        obj = bpy.data.objects.new(f"{name}_svg_filled", me)
        bpy.context.collection.objects.link(obj)
        bm = bm_mod.new()
        verts = [bm.verts.new((x, y, 0)) for x, y in polygon]
        bm.faces.new(verts)
        bm.normal_update()
        bm.to_mesh(me); bm.free()
        setup_workbench()
        s = bpy.context.scene
        # Match the piece's top-view camera settings (span 84mm matching MOVE_V2_top)
        cam = add_cam((0, 0, 100), (0, 0, 0), 84, "svg_top")
        s.camera = cam
        s.render.filepath = str(RENDERS / f"{name}_SVG_silhouette.png")
        bpy.ops.render.render(write_still=True)

        # (b) Render the polygon TESSELLATED THE SAME WAY THE BODY USES IT
        # (mathutils.tessellate_polygon, individual triangle faces). If this looks
        # different from (a) above, then mathutils' tessellation handles the
        # polygon's hook curls differently than Blender's n-gon fill.
        reset()
        me = bpy.data.meshes.new(f"{name}_svg_tess")
        obj = bpy.data.objects.new(f"{name}_svg_tess", me)
        bpy.context.collection.objects.link(obj)
        bm = bm_mod.new()
        verts = [bm.verts.new((x, y, 0)) for x, y in polygon]
        from mathutils.geometry import tessellate_polygon
        polygon_3d = [v.co for v in verts]
        triangles = tessellate_polygon([polygon_3d])
        for i1, i2, i3 in triangles:
            bm.faces.new([verts[i1], verts[i2], verts[i3]])
        bm.to_mesh(me); bm.free()
        setup_workbench()
        s = bpy.context.scene
        cam = add_cam((0, 0, 100), (0, 0, 0), 84, "tess_top")
        s.camera = cam
        s.render.filepath = str(RENDERS / f"{name}_SVG_tessellated.png")
        bpy.ops.render.render(write_still=True)

        # (c) Wireframe outline only
        reset()
        me = bpy.data.meshes.new(f"{name}_svg_wire")
        obj = bpy.data.objects.new(f"{name}_svg_wire", me)
        bpy.context.collection.objects.link(obj)
        bm = bm_mod.new()
        verts = [bm.verts.new((x, y, 0)) for x, y in polygon]
        for i in range(len(polygon)):
            bm.edges.new([verts[i], verts[(i+1) % len(polygon)]])
        bm.to_mesh(me); bm.free()
        # Use Workbench WIREFRAME shading mode
        s = bpy.context.scene
        s.render.engine = 'BLENDER_WORKBENCH'
        s.display.shading.type = 'WIREFRAME'
        s.render.resolution_x = 900
        s.render.resolution_y = 900
        cam = add_cam((0, 0, 30), (0, 0, 0), 45, "wire_top")
        s.camera = cam
        s.render.filepath = str(RENDERS / f"{name}_SVG_wireframe.png")
        bpy.ops.render.render(write_still=True)
        # Restore SOLID shading for subsequent renders
        s.display.shading.type = 'SOLID'


def main():
    print("Extracting silhouettes + outline polygons from SVG...")
    silhouettes, polygons = extract_silhouettes()

    render_silhouette_comparison(polygons)

    reset()
    eat, eat_h = build_eat(polygons['EAT'], silhouettes['EAT'])
    render_piece_views("EAT", eat_h)

    reset()
    move, move_h = build_move(polygons['MOVE'], silhouettes['MOVE'])
    render_piece_views("MOVE", move_h)

    reset()
    grow, grow_h = build_grow(polygons['GROW'], silhouettes['GROW'])
    render_piece_views("GROW", grow_h)

    # FOOD is hand-managed (pieces/FOOD.obj, from FoodUniversal.stl) — this
    # script operates only on EAT/MOVE/GROW and never writes FOOD.obj.

    # Save the per-piece connector metadata for graft_connector.py
    write_connector_meta()

    # Topology continuity audit on every exported piece
    print("\n=== Topology continuity audit ===")
    for name in ['EAT', 'MOVE', 'GROW']:
        check_topology(OUT / f"{name}.obj", name)

    # Step 3: build a "show" scene with the three pieces side-by-side
    print("\nBuilding interactive show scene...")
    build_show_scene()

    print("\nDone. Renders in", RENDERS)
    print("Open pieces.blend in Blender GUI, or drag pieces.glb into any web 3D viewer.")
    print("Sculpt-ready (body only): *_sculpt.obj files. After sculpting, run graft_connector.py.")


if __name__ == "__main__":
    main()
