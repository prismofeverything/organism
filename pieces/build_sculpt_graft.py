"""Graft the universal connector onto a HAND-SCULPTED piece (e.g. EAT_sculpt.obj
from /home/youdonotexist/blender/organism/era-three/EAT1.obj).

Unlike build_graft (which re-builds the body parametrically from the SVG), a
sculpt has artistic features (the 5 splayed arms of EAT_sculpt, for example)
that must be PRESERVED above the connector seat. The sculpt becomes a "chalice"
that cradles the connector.

Method (all in Blender, with the universal connector dimensions matching
meshlib/connector_field.py):
  1. Import sculpt (Z-up).
  2. Build connector dome as a solid-of-revolution: meridian = peg_height(d)
     for d in [0, R_SEAT], closed with seat circle at z=0. Solid above z=0,
     pointed apex at z=DOME_HEIGHT.
  3. Subtract a cylinder (R<=R_SEAT, z>=SEAT_Z) from the sculpt — removes the
     sculpt's material where the connector goes, leaving a seat annulus + the
     surrounding arms.
  4. Union the dome (translated so its base is at SEAT_Z) into the sculpt.
  5. Optionally also build the mirror socket cavity below the seat (FOOD seats
     by pegging into pieces — sockets only matter if FOOD ever sits BELOW a
     piece; default skip for player pieces).
  6. Export OBJ + STL.

Run:
  ~/Downloads/blender-5.1.2-linux-x64/blender --background --python build_sculpt_graft.py

Env:
  SCULPT_IN    input OBJ path (default: out/EAT_sculpt.obj)
  SCULPT_OUT   output OBJ path (default: out/EAT_sculpt_graft.obj)
  SEAT_Z       z (mm) where the seat annulus sits (default: 35.0 for EAT_sculpt)
"""
import bpy, bmesh, os, math

HERE = os.path.dirname(os.path.abspath(__file__))
SCULPT_IN  = os.environ.get("SCULPT_IN",  f"{HERE}/out/EAT_sculpt.obj")
SCULPT_OUT = os.environ.get("SCULPT_OUT", f"{HERE}/out/EAT_sculpt_graft.obj")
# SEAT_Z: env override; if unset, AUTODETECT the highest z where the body's central
# pillar still fully supports R=R_SEAT (its valley_R, the min outer-surface radius, is
# >= R_SEAT). That's the natural "top of the model where the connector fits" — no divot
# in the valleys. Each sculpt has its own value (EAT≈41.24 for the current 5-arm).
SEAT_Z_ENV = os.environ.get("SEAT_Z", "")
SEAT_Z = float(SEAT_Z_ENV) if SEAT_Z_ENV else None     # filled in below if None
# TRUNCATE: chops everything above SEAT_Z from the body, giving a flat top for the dome.
# Required for tapering pieces (MOVE-like spikes); EAT-style preserves features above.
TRUNCATE = os.environ.get("TRUNCATE", "false").lower() in ("true", "1", "yes")
# CLIP_TO_SILHOUETTE: when true, intersects the dome+pedestal with the body's
# cross-section at SEAT_Z (extruded vertically as a prism). The connector outline
# matches the body silhouette — for spiral pieces whose arm-gap shapes cut into the
# connector's radius, this clips the connector to fit. Independent of TRUNCATE.
CLIP_TO_SILHOUETTE = os.environ.get("CLIP_TO_SILHOUETTE", "false").lower() in ("true", "1", "yes")

# Universal connector dims (mirror meshlib/connector_field.py — keep in sync)
DOME_HEIGHT  = 4.3
DOME_R       = 3.8 / 2          # 1.90
RIDGE_OD     = 12.8
RIDGE_ID     = 8.3
RIDGE_OR     = RIDGE_OD / 2     # 6.40
RIDGE_IR     = RIDGE_ID / 2     # 4.15
RIDGE_HEIGHT = 2.75
RIDGE_PEAK_W = 2.0
SEAT_MARGIN  = 0.0     # dropped (was 0.6); R_SEAT = RIDGE_OR. See connector_field.py.
R_SEAT       = RIDGE_OR                   # 6.40  ridge outer edge


def peg_height(d):
    """Universal connector top surface height above the seat plane (z=0 local),
    single-valued in d (radial distance). Mirror of cf.peg_height."""
    if d <= DOME_R:
        return DOME_HEIGHT * (1.0 - (d / DOME_R) ** 2)
    if d >= RIDGE_IR and d <= RIDGE_OR:
        # ridge bump with flat-top width RIDGE_PEAK_W; quadratic shoulders
        u = (d - RIDGE_IR) / (RIDGE_OR - RIDGE_IR)
        uc = abs(2 * u - 1)
        cf = RIDGE_PEAK_W / (RIDGE_OD - RIDGE_ID)
        ef = max(1.0 - cf, 1e-6)
        if uc <= cf:
            return RIDGE_HEIGHT
        sh = (uc - cf) / ef
        return RIDGE_HEIGHT * (1.0 - sh ** 2)
    return 0.0


# ----- setup
bpy.ops.wm.read_factory_settings(use_empty=True)
print(f"sculpt in: {SCULPT_IN}")
if SEAT_Z is None:
    print("seat z:    AUTO (will compute after sculpt loads)")
else:
    print(f"seat z:    {SEAT_Z}  (from env)")
print(f"truncate:  {TRUNCATE}  (chop body above seat z)" if TRUNCATE else
      f"truncate:  False (preserve arms / features above seat z)")
print(f"clip:      {CLIP_TO_SILHOUETTE}  (intersect connector with body silhouette)")

# ----- import sculpt (it was written Z-up by clean_eat1.py, but Blender's OBJ
# importer interprets files as Y-up by default; up_axis='Z' undoes that)
before = set(bpy.data.objects)
bpy.ops.wm.obj_import(filepath=SCULPT_IN, up_axis='Z', forward_axis='Y')
sculpt = next(o for o in bpy.data.objects if o not in before and o.type == 'MESH')
sculpt.name = "sculpt"
bpy.ops.object.select_all(action='DESELECT'); sculpt.select_set(True)
bpy.context.view_layer.objects.active = sculpt
bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
zmin = min(v.co.z for v in sculpt.data.vertices)
sculpt.location.z = -zmin
bpy.ops.object.transform_apply(location=True)
zmax_sculpt_initial = max(v.co.z for v in sculpt.data.vertices)
print(f"sculpt: {len(sculpt.data.vertices)} verts, z[0, {zmax_sculpt_initial:.2f}]")

# autodetect SEAT_Z: scan z from top down, find the highest z where every direction
# of the cross-section has outer-surface R >= R_SEAT (i.e. the central pillar still
# fully wraps the connector seat). Bin verts within a thin z-slab and check min R.
if SEAT_Z is None:
    # Two autodetect modes:
    #   - default (cradle): find highest z where body fully wraps R_SEAT — every angular
    #     sector has the outer surface at R >= R_SEAT (the central pillar is intact).
    #   - CLIP_TO_SILHOUETTE (spiral/MOVE): find highest z where the arm tips JUST reach
    #     R_SEAT — max_R (the body's widest direction) is at least R_SEAT. The connector
    #     will be clipped to the body's silhouette so the arm-gap shapes are cut out.
    import bisect
    coords = [(v.co.x, v.co.y, v.co.z) for v in sculpt.data.vertices]
    coords.sort(key=lambda p: p[2])
    zs = [p[2] for p in coords]
    N_SECT = 24                                                # ~15deg per sector
    R_MARGIN = 0.2                                             # cushion past R_SEAT for clean boolean
    z_probe = zmax_sculpt_initial - 0.5
    SEAT_Z = None
    while z_probe > 1.0:
        lo = bisect.bisect_left(zs, z_probe - 0.15)
        hi = bisect.bisect_right(zs, z_probe + 0.15)
        if hi - lo >= 8:
            sect_min_r = [math.inf] * N_SECT
            r_max = 0.0
            for i in range(lo, hi):
                x, y, _ = coords[i]
                r = math.hypot(x, y)
                if r > r_max: r_max = r
                if r < 1e-6: continue
                th = math.atan2(y, x)
                s = int((th + math.pi) / (2 * math.pi) * N_SECT) % N_SECT
                if r < sect_min_r[s]: sect_min_r[s] = r
            if CLIP_TO_SILHOUETTE:
                # spiral mode: arm tips reach R_SEAT (max body R >= R_SEAT + margin)
                if r_max >= R_SEAT + R_MARGIN:
                    SEAT_Z = z_probe
                    print(f"  autodetect (clip): SEAT_Z={SEAT_Z:.2f}  "
                          f"(max_R={r_max:.2f} >= R_SEAT={R_SEAT}+{R_MARGIN})")
                    break
            else:
                # cradle mode: full angular coverage AND valley_R >= R_SEAT + margin
                full_coverage = all(r < math.inf for r in sect_min_r)
                if full_coverage:
                    valley_R = min(sect_min_r)
                    if valley_R >= R_SEAT + R_MARGIN:
                        SEAT_Z = z_probe
                        print(f"  autodetect (cradle): SEAT_Z={SEAT_Z:.2f}  "
                              f"(valley_R={valley_R:.2f} >= R_SEAT={R_SEAT}+{R_MARGIN}, "
                              f"all {N_SECT} sectors covered)")
                        break
        z_probe -= 0.05
    if SEAT_Z is None:
        mode = "spiral (max_R)" if CLIP_TO_SILHOUETTE else "cradle (valley_R)"
        raise SystemExit(f"autodetect failed in {mode} mode: no z in (1, {zmax_sculpt_initial}) "
                         f"satisfies R_SEAT={R_SEAT}+{R_MARGIN}")


def add_mesh_from_verts_faces(name, verts, faces):
    """Create a Blender object from numpy-ish verts/faces."""
    m = bpy.data.meshes.new(name)
    m.from_pydata(verts, [], faces)
    m.update()
    o = bpy.data.objects.new(name, m)
    bpy.context.collection.objects.link(o)
    return o


def build_dome_solid(name="dome", z_offset=0.0, lift=0.0, embed=0.0,
                     n_radial=64, n_meridian=48):
    """Build the connector dome PLUS pedestal as ONE solid of revolution. Meridian
    (counterclockwise in (d, z), local frame, with `z_offset` the seat z in world):

        (R_SEAT, -embed)                   pedestal bottom-right (sits inside body)
        (R_SEAT, lift)                     dome's base (= top of pedestal)
        ... peg_height(d) for d from R_SEAT down to 0, raised by `lift` ...
        (0, lift+DOME_HEIGHT)              dome apex
        (0, -embed)                        closing along axis going down
        back to (R_SEAT, -embed)           closing the bottom

    Combined into one watertight solid -- no internal boundary between pedestal and
    dome -- so the boolean with the body is a single union, no coplanar issues. With
    lift=0, embed=0 this reduces to the plain dome (compatible with old usage)."""
    ms = []
    ms.append((R_SEAT, -embed))                              # pedestal bottom-right
    ms.append((R_SEAT, lift))                                # pedestal top = dome base ring
    # dome upper surface from R_SEAT inward to apex
    for i in range(n_meridian, -1, -1):
        d = R_SEAT * i / n_meridian
        ms.append((d, lift + peg_height(d)))
    ms.append((0.0, -embed))                                 # axis going back down

    verts = [(d, 0.0, z + z_offset) for d, z in ms]
    edges = [(i, i + 1) for i in range(len(verts) - 1)]
    edges.append((len(verts) - 1, 0))                        # close meridian loop
    m = bpy.data.meshes.new(name); m.from_pydata(verts, edges, []); m.update()
    o = bpy.data.objects.new(name, m); bpy.context.collection.objects.link(o)

    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.mode_set(mode='EDIT')
    bm = bmesh.from_edit_mesh(o.data)
    bmesh.ops.spin(bm, geom=bm.verts[:] + bm.edges[:], cent=(0,0,0), axis=(0,0,1),
                   angle=2 * math.pi, steps=n_radial, use_merge=True)
    bmesh.ops.remove_doubles(bm, verts=bm.verts, dist=1e-5)
    bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
    bmesh.update_edit_mesh(o.data)
    bpy.ops.object.mode_set(mode='OBJECT')
    print(f"  dome+pedestal solid: {len(o.data.vertices)} verts, {len(o.data.polygons)} polys "
          f"(lift={lift}, embed={embed})")
    return o


def build_cylinder(name, radius, z0, z1, n=64):
    """Closed cylinder (solid) at z=[z0,z1], r=radius. For the boolean subtract."""
    bpy.ops.mesh.primitive_cylinder_add(vertices=n, radius=radius, depth=(z1 - z0),
                                         enter_editmode=False, location=(0, 0, (z0+z1)/2))
    o = bpy.context.active_object
    o.name = name
    return o


# Pedestal: small flat platform raised ABOVE the body's natural surface (instead of
# carved into it as a divot). LIFT lifts the dome above SEAT_Z; EMBED extends the
# pedestal a touch below SEAT_Z so it blends with the body instead of perching.
PEDESTAL_LIFT  = float(os.environ.get("PEDESTAL_LIFT",  "0.5"))   # mm raised above SEAT_Z
PEDESTAL_EMBED = float(os.environ.get("PEDESTAL_EMBED", "0.5"))   # mm extending into body

# ----- build the dome+pedestal as ONE watertight solid (no internal interface)
dome = build_dome_solid("dome", z_offset=SEAT_Z,
                        lift=PEDESTAL_LIFT, embed=PEDESTAL_EMBED)

zmax_sculpt = max(v.co.z for v in sculpt.data.vertices)
cyl_top = zmax_sculpt + 5.0

if TRUNCATE:
    # MOVE-style spike: chop everything above SEAT_Z so the dome+pedestal sit on a flat top
    r_truncate = max(math.hypot(v.co.x, v.co.y) for v in sculpt.data.vertices) + 2.0
    cutter = build_cylinder("subtract_truncate", radius=r_truncate, z0=SEAT_Z, z1=cyl_top)
    print(f"  TRUNCATE: removing all body above z={SEAT_Z:.2f}")
    bpy.ops.object.select_all(action='DESELECT')
    sculpt.select_set(True); bpy.context.view_layer.objects.active = sculpt
    mod = sculpt.modifiers.new("cut", 'BOOLEAN')
    mod.operation = 'DIFFERENCE'; mod.solver = 'EXACT'; mod.object = cutter
    bpy.ops.object.modifier_apply(modifier="cut")
    bpy.data.objects.remove(cutter, do_unlink=True)

if CLIP_TO_SILHOUETTE:
    # Build a SILHOUETTE PRISM from the body's cross-section at SEAT_Z, extruded
    # vertically over the connector's full z range. Intersect dome+pedestal with the
    # prism so the connector outline matches the body silhouette in the arm-gap
    # directions (the gaps between spiral arms are cut out of the connector).
    # Requires TRUNCATE so the body has a clean flat top to extract from.
    if not TRUNCATE:
        raise SystemExit("CLIP_TO_SILHOUETTE requires TRUNCATE=true (need a flat top at SEAT_Z)")
    # Duplicate truncated body, isolate top face(s) at z=SEAT_Z, extrude to a prism
    bpy.ops.object.select_all(action='DESELECT')
    sculpt.select_set(True); bpy.context.view_layer.objects.active = sculpt
    bpy.ops.object.duplicate()
    mask = bpy.context.view_layer.objects.active
    mask.name = "silhouette_mask"
    bpy.ops.object.mode_set(mode='EDIT')
    bm = bmesh.from_edit_mesh(mask.data)
    for v in bm.verts: v.select = False
    for e in bm.edges: e.select = False
    # select faces whose verts all sit on the SEAT_Z plane (the truncated top)
    top_faces = [f for f in bm.faces
                 if all(abs(v.co.z - SEAT_Z) < 0.01 for v in f.verts)]
    if not top_faces:
        raise SystemExit("CLIP_TO_SILHOUETTE: couldn't find truncated top face at SEAT_Z")
    for f in top_faces: f.select = True
    bmesh.update_edit_mesh(mask.data)
    bpy.ops.mesh.select_all(action='INVERT')
    bpy.ops.mesh.delete(type='FACE')
    # shift the polygon down to (SEAT_Z - EMBED), then extrude up over the full
    # connector range so it clips both the pedestal and the dome
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.transform.translate(value=(0, 0, -PEDESTAL_EMBED))
    extrude_up = PEDESTAL_EMBED + PEDESTAL_LIFT + DOME_HEIGHT + 1.0
    bpy.ops.mesh.extrude_region_move(
        TRANSFORM_OT_translate={"value": (0, 0, extrude_up)})
    bpy.ops.object.mode_set(mode='OBJECT')
    print(f"  CLIP: prism z=[{SEAT_Z-PEDESTAL_EMBED:.2f}, "
          f"{SEAT_Z-PEDESTAL_EMBED+extrude_up:.2f}] body-silhouette extruded")
    # Boolean intersect dome+pedestal with the prism (clips to body silhouette)
    bpy.ops.object.select_all(action='DESELECT')
    dome.select_set(True); bpy.context.view_layer.objects.active = dome
    mod = dome.modifiers.new("clip", 'BOOLEAN')
    mod.operation = 'INTERSECT'; mod.solver = 'EXACT'; mod.object = mask
    bpy.ops.object.modifier_apply(modifier="clip")
    bpy.data.objects.remove(mask, do_unlink=True)
    print(f"  after clip: dome {len(dome.data.vertices)} verts, {len(dome.data.polygons)} polys")

# ----- single boolean: union dome+pedestal with body (additive, no divot, body intact)
bpy.ops.object.select_all(action='DESELECT')
sculpt.select_set(True); bpy.context.view_layer.objects.active = sculpt
mod = sculpt.modifiers.new("addconn", 'BOOLEAN')
mod.operation = 'UNION'; mod.solver = 'EXACT'; mod.object = dome
bpy.ops.object.modifier_apply(modifier="addconn")
bpy.data.objects.remove(dome, do_unlink=True)
print(f"after union dome: {len(sculpt.data.vertices)} verts, {len(sculpt.data.polygons)} polys")

# ----- cleanup: merge by distance, dissolve degenerate, strip tiny floating components
# from the boolean (the EXACT solver tends to leave small "scrap" triangles where the
# tool mesh's ring didn't quite align with the existing topology). After stripping, fill
# any remaining open boundary loops on the main mesh.
bpy.ops.object.mode_set(mode='EDIT')
bm = bmesh.from_edit_mesh(sculpt.data)
bmesh.ops.remove_doubles(bm, verts=bm.verts, dist=5e-4)
bmesh.ops.dissolve_degenerate(bm, edges=bm.edges, dist=1e-4)
bmesh.update_edit_mesh(sculpt.data)
bpy.ops.object.mode_set(mode='OBJECT')

# strip tiny components: select by smaller-than threshold and delete
bpy.ops.object.mode_set(mode='EDIT')
bpy.ops.mesh.select_all(action='DESELECT')
bpy.ops.object.mode_set(mode='OBJECT')
# walk components via bmesh; mark for deletion any with < threshold verts
bm = bmesh.new(); bm.from_mesh(sculpt.data)
# tag verts by component
import collections
visited = [False] * len(bm.verts)
comps = []
bm.verts.ensure_lookup_table()
for seed in range(len(bm.verts)):
    if visited[seed]: continue
    stack = [seed]; ids = []
    while stack:
        i = stack.pop()
        if visited[i]: continue
        visited[i] = True; ids.append(i)
        for e in bm.verts[i].link_edges:
            o = e.other_vert(bm.verts[i]).index
            if not visited[o]: stack.append(o)
    comps.append(ids)
comps.sort(key=len, reverse=True)
print(f"  components: {len(comps)} (largest {len(comps[0])} verts)")
# delete everything except the largest component
to_delete = [bm.verts[i] for c in comps[1:] for i in c]
print(f"  stripping {len(comps) - 1} small components ({len(to_delete)} verts)")
bmesh.ops.delete(bm, geom=to_delete, context='VERTS')
bm.to_mesh(sculpt.data); bm.free()

# fill remaining boundary holes on the main mesh
bpy.ops.object.mode_set(mode='EDIT')
bm = bmesh.from_edit_mesh(sculpt.data)
open_edges = [e for e in bm.edges if not e.is_manifold]
if open_edges:
    print(f"  filling {len(open_edges)} non-manifold edges on main mesh")
    bmesh.ops.triangle_fill(bm, edges=open_edges, use_beauty=True)
bmesh.ops.recalc_face_normals(bm, faces=bm.faces)
nm = sum(1 for e in bm.edges if not e.is_manifold)
bmesh.update_edit_mesh(sculpt.data)
bpy.ops.object.mode_set(mode='OBJECT')
print(f"final mesh: {len(sculpt.data.vertices)} verts, {len(sculpt.data.polygons)} polys, "
      f"non-manifold edges: {nm}")

# ----- export OBJ (Z-up) + STL
os.makedirs(os.path.dirname(SCULPT_OUT), exist_ok=True)
bpy.ops.wm.obj_export(filepath=SCULPT_OUT, up_axis='Z', forward_axis='Y',
                      export_selected_objects=False, export_uv=False,
                      export_normals=False, export_materials=False)
stl_out = SCULPT_OUT.replace('.obj', '.stl')
try:
    bpy.ops.wm.stl_export(filepath=stl_out)
except Exception:
    bpy.ops.export_mesh.stl(filepath=stl_out)
print(f"wrote {SCULPT_OUT}")
print(f"wrote {stl_out}")

# ----- auto-render overview (front + top + 3/4) into pieces/renders/sculpt_<name>_overview.png
# Mirrors the auto-render pattern in meshlib/build.py:_render_pieces so every sculpt graft
# self-documents in pieces/renders/.
name = os.path.splitext(os.path.basename(SCULPT_OUT))[0]   # e.g. "EAT_sculpt_graft"
overview_png = os.path.join(HERE, "renders", f"sculpt_{name}_overview.png")
os.makedirs(os.path.dirname(overview_png), exist_ok=True)

# clear and re-import the just-saved OBJ to render the final committed mesh
bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.wm.obj_import(filepath=SCULPT_OUT, up_axis='Z', forward_axis='Y')
o = next(ob for ob in bpy.data.objects if ob.type == 'MESH')
bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
bpy.context.view_layer.objects.active = o
bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
zmin_o = min(v.co.z for v in o.data.vertices); o.location = (0, 0, -zmin_o)
bpy.ops.object.transform_apply(location=True); bpy.ops.object.shade_smooth()
mat = bpy.data.materials.new("m"); mat.diffuse_color = (0.82, 0.82, 0.86, 1)
o.data.materials.append(mat)

# render three viewpoints, joined in a wide canvas. Easiest = single scene with one camera
# per render; combine the three PNGs into a strip via PIL if available, else write the 3/4.
zmax_o = max(v.co.z for v in o.data.vertices)
sc = bpy.context.scene
sc.render.engine = 'BLENDER_WORKBENCH'; sh = sc.display.shading
sh.light = 'STUDIO'; sh.show_shadows = False
sh.show_cavity = True; sh.cavity_type = 'BOTH'
sh.color_type = 'SINGLE'; sh.single_color = (0.8, 0.8, 0.85)
sc.render.resolution_x = 600; sc.render.resolution_y = 600

panel_pngs = []
# Three views: front (orthogonal silhouette), top (down the z-axis), and 3/4 looking
# DOWN at the connector from above so the dome is visible above the arms. The 3/4 cam
# sits ABOVE the apex and points down-and-in so both the body silhouette and the
# connector crown are in frame.
SEAT_Z_TGT = float(os.environ.get("SEAT_Z", "37.0"))   # for the 3q look-at
for label, cam_loc, target_z, lens in [
    ("front", (0, -180, zmax_o * 0.55),       zmax_o * 0.45,  50),
    ("top",   (0, 0, zmax_o + 80),             zmax_o * 0.5,   50),
    ("3q",    (90, -90, zmax_o + 50),          SEAT_Z_TGT + 1, 50),
]:
    for prev in list(bpy.data.cameras):
        bpy.data.cameras.remove(prev)
    emp = bpy.data.objects.new("t", None); sc.collection.objects.link(emp); emp.location = (0,0,target_z)
    camd = bpy.data.cameras.new("c"); cam = bpy.data.objects.new("c", camd)
    sc.collection.objects.link(cam); cam.location = cam_loc; camd.lens = lens
    con = cam.constraints.new('TRACK_TO'); con.target = emp
    con.track_axis = 'TRACK_NEGATIVE_Z'; con.up_axis = 'UP_Y'; sc.camera = cam
    panel_png = overview_png.replace('.png', f'_{label}.png')
    sc.render.filepath = panel_png
    bpy.ops.render.render(write_still=True)
    panel_pngs.append(panel_png)
    bpy.data.objects.remove(emp, do_unlink=True)
    bpy.data.objects.remove(cam, do_unlink=True)

# stitch the three panels side-by-side into the overview PNG using numpy (ships with
# Blender; PIL doesn't). Blender's image pixels are RGBA floats, rows bottom-to-top; we
# stay in that layout the whole way, so no Y-flip needed.
import numpy as np
arrs = []
for p in panel_pngs:
    bi = bpy.data.images.load(p)
    w, h = bi.size[0], bi.size[1]
    arrs.append(np.array(bi.pixels[:], dtype=np.float32).reshape(h, w, 4))
    bpy.data.images.remove(bi)
H = max(a.shape[0] for a in arrs); W = sum(a.shape[1] for a in arrs)
canvas = np.ones((H, W, 4), dtype=np.float32)
x = 0
for a in arrs:
    canvas[:a.shape[0], x:x + a.shape[1]] = a; x += a.shape[1]
out_img = bpy.data.images.new("overview", width=W, height=H, alpha=True)
out_img.pixels.foreach_set(canvas.flatten())
out_img.filepath_raw = overview_png; out_img.file_format = 'PNG'
out_img.save()
for p in panel_pngs: os.remove(p)
print(f"wrote {overview_png}")
