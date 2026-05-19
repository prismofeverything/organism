"""V2 piece generator:
- Top-down silhouette is an INVARIANT: each piece's z=0 outline matches the SVG.
- EAT: import the original EAT.07.obj as the body, add a small landing plateau + connector.
- MOVE: loft from SVG silhouette, twist 60° CCW, hollow underside (3 leg arches).
- GROW: loft from SVG silhouette, monotonic dome, deeper lobes.
- All three get the parabolic dome + outer ridge connector on top.

Run:
  ~/Downloads/blender-5.1.1-linux-x64/blender --background --python pieces_v2.py
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
DOME_DIA       = 6.0
DOME_HEIGHT    = 3.0
RIDGE_OD       = 12.0
RIDGE_ID       = 9.0
RIDGE_HEIGHT   = 0.6
RIDGE_PEAK_W   = 1.0
CLEARANCE      = 0.10
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

        # Center polygon at origin BEFORE symmetrization (rotational symmetry is
        # about the origin). Don't subsample first — symmetrize requires the full
        # walk so we can average corresponding points across sectors.
        ocx = sum(v.x for v in ordered) / len(ordered)
        ocy = sum(v.y for v in ordered) / len(ordered)
        centered = [(v.x - ocx, v.y - ocy) for v in ordered]

        # Enforce strict N-fold rotational symmetry on the polygon
        fold_N = SYMMETRY_FOLD[name]
        symm = symmetrize_polygon(centered, fold_N)

        # Now subsample (if still very dense) and scale to FOOTPRINT
        if len(symm) > TARGET_PTS * fold_N:    # keep div-by-N
            stride = max(1, len(symm) // (TARGET_PTS * fold_N // fold_N * fold_N))
            symm = [symm[i] for i in range(0, len(symm), stride)]
        max_r = max(hypot(x, y) for x, y in symm)
        scale = (FOOTPRINT/2) / max_r
        polygon = [(x*scale, y*scale) for x, y in symm]
        polygons[name] = polygon

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
    dome  = build_parabolic_dome("CavDome",  DOME_DIA/2 + CLEARANCE,  DOME_HEIGHT + CLEARANCE,  z_offset=0)
    dome.scale.z = -1; bpy.context.view_layer.objects.active = dome
    bpy.ops.object.transform_apply(scale=True)
    ridge = build_ridge_ring("CavRidge", RIDGE_OD + 2*CLEARANCE, RIDGE_ID - 2*CLEARANCE,
                             RIDGE_HEIGHT + CLEARANCE, RIDGE_PEAK_W, z_offset=0)
    ridge.scale.z = -1; bpy.context.view_layer.objects.active = ridge
    bpy.ops.object.transform_apply(scale=True)
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

    Splits the input into N equal-length sectors, rotates each back to sector 0,
    averages corresponding points → canonical 1/N arm, then rotate-copies N times.
    After this, rotating the result by 360°/N around the origin gives a vertex-
    identical permutation: vertex i maps to vertex (i + pts_per_arm) mod N*pts_per_arm.

    Assumes the polygon is centered at the origin (which it is after extract_silhouettes
    does its centering pass) and that the SVG outline is approximately N-fold to begin
    with — averaging will smooth out hand-drawn inconsistencies.
    """
    n = len(polygon)
    pts_per_arm = n // fold_N
    if pts_per_arm == 0:
        return polygon
    polygon = polygon[:pts_per_arm * fold_N]   # truncate to a multiple of fold_N

    # Canonical arm: average of the N sectors rotated back to sector 0
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

    # Reconstitute full polygon by rotating canonical arm N times
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

    # Body rings — outline at constant scale, varying z
    body_rings = []
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

def build_eat(polygon):
    """5-fold star polygon swept up to body height, then a SHALLOW dome on top
    (hemisphere-style cap, like GROW but much shallower)."""
    print("\n=== EAT (swept polygon + shallow dome top) ===")
    DOME_HEIGHT = 4.0   # shallow — rim is the prominent feature, dome just softens the top
    obj = build_swept_extrusion("EAT", polygon,
                                body_height=EAT_HEIGHT - DOME_HEIGHT,
                                top_shape='hemisphere', top_param=DOME_HEIGHT)
    shade_smooth(obj)
    export(obj, "EAT.obj")
    save_connector_meta("EAT", EAT_HEIGHT, has_socket=False)
    return obj, EAT_HEIGHT


def build_move(polygon):
    """3-fold spiral polygon (with the inward-curling hooks intact) swept up,
    then parabolic dome top. Swept extrusion preserves the actual outline
    even where it self-intersects radially — radial loft can't do that."""
    print("\n=== MOVE (swept polygon + parabola top) ===")
    PARABOLA_HEIGHT = 10.0
    obj = build_swept_extrusion("MOVE", polygon,
                                body_height=MOVE_HEIGHT - PARABOLA_HEIGHT,
                                top_shape='parabola', top_param=PARABOLA_HEIGHT)
    shade_smooth(obj)
    export(obj, "MOVE.obj")
    save_connector_meta("MOVE", MOVE_HEIGHT, has_socket=True)
    return obj, MOVE_HEIGHT


def build_grow(polygon):
    """4-fold quatrefoil polygon swept up, then hemisphere top."""
    print("\n=== GROW (swept polygon + hemisphere top) ===")
    HEMISPHERE_HEIGHT = 14.0
    obj = build_swept_extrusion("GROW", polygon,
                                body_height=GROW_HEIGHT - HEMISPHERE_HEIGHT,
                                top_shape='hemisphere', top_param=HEMISPHERE_HEIGHT)
    shade_smooth(obj)
    export(obj, "GROW.obj")
    save_connector_meta("GROW", GROW_HEIGHT, has_socket=True)
    return obj, GROW_HEIGHT


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

def build_food():
    """Simple disc — clean cylinder with slight rim rounding. No connector."""
    print("\n=== FOOD (simple disc) ===")
    me = bpy.data.meshes.new("FOOD")
    obj = bpy.data.objects.new("FOOD", me)
    bpy.context.collection.objects.link(obj)
    bm = bmesh.new()
    n_theta = 64
    rings = []
    for iz in range(5):
        z = (iz / 4) * FOOD_HEIGHT
        ring = [bm.verts.new(((FOOD_DIA/2)*cos(2*pi*i/n_theta),
                              (FOOD_DIA/2)*sin(2*pi*i/n_theta), z))
                for i in range(n_theta)]
        rings.append(ring)
    for i in range(len(rings) - 1):
        a, b = rings[i], rings[i+1]
        for ith in range(n_theta):
            j = (ith+1) % n_theta
            bm.faces.new([a[ith], a[j], b[j], b[ith]])
    bm.faces.new(rings[0][::-1])
    bm.faces.new(rings[-1])
    bm.normal_update()
    bm.to_mesh(me); bm.free()
    bpy.context.view_layer.objects.active = obj
    obj.select_set(True)
    fillet_edges(obj, FOOD_EDGE_FILLET, segments=4, angle_deg=25)
    shade_smooth(obj)
    export(obj, "FOOD.obj")
    save_connector_meta("FOOD", FOOD_HEIGHT, has_socket=True)
    return obj, FOOD_HEIGHT


def build_show_scene():
    """Load all three exported OBJs side-by-side and save as .blend + .glb
    so the user can open and rotate them interactively."""
    reset()
    SPACING = 60.0   # mm between pieces, centered
    positions = {
        "EAT.obj":  (-SPACING*1.5, 0, 0),
        "MOVE.obj": (-SPACING*0.5, 0, 0),
        "GROW.obj": ( SPACING*0.5, 0, 0),
        "FOOD.obj": ( SPACING*1.5, 0, 0),
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
    (a) as a filled n-gon (what bm.faces.new produces) and
    (b) as a wireframe (edges only, no fill — shows the unambiguous polygon shape).
    Helps diagnose whether non-convex polygons are being mis-rendered."""
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
        cam = add_cam((0, 0, 30), (0, 0, 0), 45, "svg_top")
        s.camera = cam
        s.render.filepath = str(RENDERS / f"{name}_SVG_silhouette.png")
        bpy.ops.render.render(write_still=True)

        # (b) Wireframe outline only — no face. Workbench Wireframe mode shows edges.
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
    eat, eat_h = build_eat(polygons['EAT'])
    render_piece_views("EAT", eat_h)

    reset()
    move, move_h = build_move(polygons['MOVE'])
    render_piece_views("MOVE", move_h)

    reset()
    grow, grow_h = build_grow(polygons['GROW'])
    render_piece_views("GROW", grow_h)

    reset()
    food, food_h = build_food()
    render_piece_views("FOOD", food_h)

    # Save the per-piece connector metadata for graft_connector.py
    write_connector_meta()

    # Step 3: build a "show" scene with all four side-by-side
    print("\nBuilding interactive show scene...")
    build_show_scene()

    print("\nDone. Renders in", RENDERS)
    print("Open pieces.blend in Blender GUI, or drag pieces.glb into any web 3D viewer.")
    print("Sculpt-ready (body only): *_sculpt.obj files. After sculpting, run graft_connector.py.")


if __name__ == "__main__":
    main()
