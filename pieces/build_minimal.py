"""MINIMAL pieces: the 2D-silhouette stand-ins for the 3D EAT/MOVE/GROW.

Each is the piece's top-down SVG silhouette (inputs/{eat,move,grow}.svg -> the same source the 3D
pieces are built from) scaled to the 37 mm footprint, extruded to a 9 mm disk with the silhouette
edges BEVELLED above and below (continuous, no sharp corners). They are "basically food": the
universal connector PEG (dome+ridge) sits on top and a matching SOCKET is carved into the bottom, so
a disk nests onto a piece/food peg and food nests onto it.

Silhouette resolution: rather than let Blender tessellate the vector curves (its SVG importer left
long flat chords, and its arc-fill left GROW's trefoil with ~1000 hole edges), we sample each outline
into a dense polygon with shapely (`clean_svg.py`, run in the repo venv) and extrude THAT. NSAMP
points around the outline -> chord length ~= perimeter/NSAMP; at 1600 that is <=0.26 mm, below the
0.4 mm nozzle, so the print reads as a true smooth curve. shapely's make_valid also unions any
self-overlap into one clean simple boundary, so all three build watertight with a crisp connector and
NO voxel remesh.

  ~/Downloads/.../blender -b --python build_minimal.py           # -> out/{EAT,MOVE,GROW}_mindisk.obj(+stl)
Env: MIN_FP (footprint mm, def 37), MIN_H (body height mm, def 9), MIN_BEVEL (edge bevel mm, def 0.7),
     MIN_NSAMP (outline sample points, def 1600), MIN_BEVEL_RES (bevel profile res, def 6),
     MIN_PIECES ("EAT MOVE" to build a subset)
"""
import bpy, bmesh, os, sys, json, subprocess
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import graft_connector as gc          # universal connector: dims + peg/socket builders
import sor                            # solids-of-revolution: self-supporting socket meridian

FP     = float(os.environ.get("MIN_FP", "37.0"))
H      = float(os.environ.get("MIN_H", "9.0"))
BEVEL  = float(os.environ.get("MIN_BEVEL", "0.7"))
NSAMP  = int(os.environ.get("MIN_NSAMP", "1600"))     # outline sample points = silhouette smoothness
BEV_RES = int(os.environ.get("MIN_BEVEL_RES", "6"))
# Self-supporting socket (the food method): carve the bottom cavity with a pointed cone cap + tented
# ridge roof (sor._socket_ss) instead of the flat-apex parabolic socket, so every downward-facing
# ceiling is <= SS_DEG from vertical and the disk prints socket-DOWN support-free. Mating flanks are
# unchanged -> identical grip. Set MIN_SS_SOCKET=0 for the old (support-needing) parabolic socket.
SS_SOCKET = os.environ.get("MIN_SS_SOCKET", "1").lower() not in ("0", "false", "no")
SS_DEG    = float(os.environ.get("MIN_SS_DEG", "30.0"))
SS_CLR    = float(os.environ.get("MIN_SS_CLR", "0.3"))
ALL_PIECES = [("EAT", "eat"), ("MOVE", "move"), ("GROW", "grow")]
_want = os.environ.get("MIN_PIECES", "").upper().replace(",", " ").split()
PIECES = [p for p in ALL_PIECES if p[0] in _want] if _want else ALL_PIECES

# shapely/svgelements live in the repo venv, not Blender's bundled Python -> shell out to clean_svg.py.
VENV_PY = os.path.normpath(os.path.join(HERE, "..", ".venv", "bin", "python"))

def clean_polygon(stem):
    """Return the shapely-cleaned outline as {exterior:[[x,y]...], interiors:[...]} (SVG units)."""
    if not os.path.exists(VENV_PY):
        raise SystemExit(f"venv python not found at {VENV_PY} (needs shapely+svgelements; run `uv sync`)")
    out_json = f"{HERE}/out/{stem}_clean.json"
    subprocess.run([VENV_PY, f"{HERE}/clean_svg.py", f"{HERE}/inputs/{stem}.svg", out_json, str(NSAMP)],
                   check=True)
    return json.load(open(out_json))

def nonmanifold(body):
    bm = bmesh.new(); bm.from_mesh(body.data)
    n = sum(1 for e in bm.edges if not e.is_manifold); bm.free()
    return n

def _tidy(body, dissolve=False):
    bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
    bpy.context.view_layer.objects.active = body
    bpy.ops.object.mode_set(mode='EDIT'); bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.remove_doubles(threshold=1e-4)
    if dissolve: bpy.ops.mesh.dissolve_degenerate(threshold=1e-4)
    bpy.ops.mesh.normals_make_consistent(inside=False)
    bpy.ops.object.mode_set(mode='OBJECT')

def union_cylinder(body, r, h):
    """Union a central cylinder so there's solid material under the connector."""
    bpy.ops.mesh.primitive_cylinder_add(vertices=128, radius=r, depth=h, location=(0, 0, h/2))
    cyl = bpy.context.active_object
    bpy.context.view_layer.objects.active = body
    m = body.modifiers.new("U_core", 'BOOLEAN'); m.operation = 'UNION'; m.object = cyl; m.solver = 'EXACT'
    bpy.ops.object.modifier_apply(modifier="U_core")
    bpy.data.objects.remove(cyl, do_unlink=True)

def add_socket_ss(body, z_floor=-2.0):
    """Carve a SELF-SUPPORTING socket into the bottom (the food method). sor._socket_ss gives the
    cavity meridian (pointed cone cap + tented ridge groove, ceilings <= SS_DEG from vertical); we
    close it below z=0 and revolve it into one clean cutter, then boolean-difference it out. Prints
    socket-down with no internal support; mating flanks match gc.add_socket -> identical grip."""
    pts, apex_z = sor._socket_ss(gc.DOME_DIA/2, gc.DOME_HEIGHT, gc.RIDGE_ID/2, gc.RIDGE_OD/2,
                                 gc.RIDGE_HEIGHT, gc.RIDGE_PEAK_W, gc.SOCKET_GAP, 0.0, SS_DEG, SS_CLR)
    s_or = pts[-1][0]                                  # outer socket radius at the bottom face (z=0)
    prof = list(pts) + [(s_or, z_floor), (0.0, z_floor)]   # close: down through the bottom face, back to axis
    cutter = sor.revolve(prof, "SocketSS", seg=2 * gc.DOME_SEGS)
    bpy.context.view_layer.objects.active = body
    m = body.modifiers.new("C_socketss", 'BOOLEAN'); m.operation = 'DIFFERENCE'; m.object = cutter; m.solver = 'EXACT'
    bpy.ops.object.modifier_apply(modifier="C_socketss")
    bpy.data.objects.remove(cutter, do_unlink=True)
    return apex_z

def poly_body(name, data):
    """Build the extruded, bevelled disk body from the cleaned outline polygon (a dense simple
    boundary -> Blender fills it cleanly; no SVG importer, no arc-fill holes)."""
    cu = bpy.data.curves.new(name, 'CURVE'); cu.dimensions = '2D'; cu.fill_mode = 'BOTH'
    def add_loop(coords):
        if len(coords) > 1 and coords[0] == coords[-1]: coords = coords[:-1]   # rings close; drop dup
        spl = cu.splines.new('POLY'); spl.points.add(len(coords) - 1)
        for i, (x, y) in enumerate(coords): spl.points[i].co = (x, y, 0.0, 1.0)
        spl.use_cyclic_u = True
    add_loop(data["exterior"])
    for hole in data.get("interiors", []): add_loop(hole)
    obj = bpy.data.objects.new(name, cu); bpy.context.collection.objects.link(obj)
    bpy.context.view_layer.objects.active = obj; obj.select_set(True)
    # clean_svg.py already centers the outline on its area centroid (= the C-n symmetry center), so the
    # object origin (0,0,0) sits on the shape's true center -> the peg/socket graft there, not on the
    # bbox center (which is ~1.6 mm off for the C5 star). Don't re-origin to BOUNDS.
    bpy.context.view_layer.update(); obj.location = (0, 0, 0)
    w = max(obj.dimensions.x, obj.dimensions.y)
    s = (FP - 2*BEVEL) / w                         # bevel grows the outline by ~BEVEL/side -> compensate
    cu.extrude = (H/2 - BEVEL) / s
    cu.bevel_depth = BEVEL / s; cu.bevel_resolution = BEV_RES
    obj.scale = (s, -s, s)                          # -Y: SVG y-down -> match the piece silhouette
    bpy.ops.object.convert(target='MESH')
    body = bpy.context.view_layer.objects.active
    body.location.z = H/2
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    _tidy(body)
    return body

def build(name, stem):
    bpy.ops.wm.read_factory_settings(use_empty=True)
    data = clean_polygon(stem)
    body = poly_body(name, data)
    nm_body = nonmanifold(body)
    union_cylinder(body, gc.RIDGE_OD/2 + 0.8, H)   # solid core under the peg/socket
    gc.add_peg(body, H)                            # peg (dome+ridge) on top
    if SS_SOCKET: add_socket_ss(body)             # self-supporting socket (food method) -> prints support-free
    else:         gc.add_socket(body)             # legacy flat-apex socket (needs support blocker)
    _tidy(body, dissolve=True)
    nm = nonmanifold(body)
    bpy.ops.object.shade_smooth()
    zr = [v.co.z for v in body.data.vertices]
    print(f"{name}: footprint {max(body.dimensions.x, body.dimensions.y):.1f} mm  "
          f"body+peg height {max(zr)-min(zr):.2f} mm  body_nm {nm_body}  final_nm {nm}")
    out = f"{HERE}/out/{name}_mindisk"
    bpy.ops.object.select_all(action='DESELECT'); body.select_set(True)
    bpy.context.view_layer.objects.active = body
    bpy.ops.wm.obj_export(filepath=out + ".obj", export_selected_objects=True, forward_axis='Y', up_axis='Z')
    try: bpy.ops.wm.stl_export(filepath=out + ".stl", export_selected_objects=True)
    except Exception: bpy.ops.export_mesh.stl(filepath=out + ".stl", use_selection=True)
    print("  wrote", out + ".obj (+.stl)")

os.makedirs(f"{HERE}/out", exist_ok=True)
for name, stem in PIECES:
    build(name, stem)
print("minimal disks done")
