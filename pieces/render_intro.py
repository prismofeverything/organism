"""Beauty intro render: 3 sculpt pieces (EAT, MOVE, GROW) on a row of hex spaces on
the board, with one food snapped on each, in the BLUE player color (109, 162, 178).
Photoreal via EEVEE Next with sun + fill, modest zoom-out 3/4 camera.

Pieces sit on board hex spaces C:2, C:1, C:0 (the symmetric 3-in-a-row at y=+74.5,
which read as green on the printed art).

Run:
  ~/Downloads/blender-5.1.2-linux-x64/blender --background --threads 4 --python render_intro.py
"""
import sys, bpy, math, os
from mathutils import Vector
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import organism_format as ogf

HERE  = os.path.dirname(os.path.abspath(__file__))
SCENE = os.path.join(HERE, "scene"); os.makedirs(SCENE, exist_ok=True)
OUT   = os.environ.get("INTRO_OUT", os.path.join(SCENE, "intro_sculpt.png"))

# Universal connector dome height (sculpts have apex at SEAT_Z + LIFT + DOME_HEIGHT)
DOME_HEIGHT = 4.3
SEAT_LIFT   = 0.5     # matches build_sculpt_graft PEDESTAL_LIFT default
FOOD_STACK_DZ = 6.4   # vertical step between stacked food tokens (nesting offset)

# Player color: Blue (sRGB 0-255 from build_setup_scene.py)
BLUE_RGB = (109, 162, 178)
FOOD_RGB = (242, 230, 158)   # soft warm yellow (matches video)

# env: FOOD_COUNTS="1,2,3" (per-piece counts in EAT/MOVE/GROW order)
#      VIEW="3q" | "top"
FOOD_COUNTS = [int(x) for x in os.environ.get("FOOD_COUNTS", "1,1,1").split(",")]
VIEW        = os.environ.get("VIEW", "3q")

def s2l(c):
    c /= 255.0
    return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4
def lin(rgb): return (s2l(rgb[0]), s2l(rgb[1]), s2l(rgb[2]), 1.0)

bpy.ops.wm.read_factory_settings(use_empty=True)
sc = bpy.context.scene
try: sc.render.engine = 'BLENDER_EEVEE_NEXT'
except Exception: sc.render.engine = 'BLENDER_EEVEE'
try: sc.eevee.taa_render_samples = 96
except Exception: pass
sc.render.resolution_x, sc.render.resolution_y = 1800, 1200
sc.render.film_transparent = False
# soft warm world ambient
w = bpy.data.worlds.new("W"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (0.62, 0.63, 0.66, 1)
w.node_tree.nodes["Background"].inputs[1].default_value = 0.9

def C(): return bpy.context.collection

def color_mat(name, rgb, rough=0.45):
    m = bpy.data.materials.new(name); m.use_nodes = True
    b = m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value = lin(rgb)
    b.inputs["Roughness"].default_value = rough
    return m

def img_mat(name, path):
    m = bpy.data.materials.new(name); m.use_nodes = True
    nt = m.node_tree; b = nt.nodes["Principled BSDF"]
    t = nt.nodes.new("ShaderNodeTexImage"); t.image = bpy.data.images.load(path)
    nt.links.new(t.outputs["Color"], b.inputs["Base Color"])
    b.inputs["Roughness"].default_value = 0.85
    return m

# Table + Board
def plane(name, sz, mat, loc):
    me = bpy.data.meshes.new(name); o = bpy.data.objects.new(name, me); C().objects.link(o)
    s = sz / 2
    me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)], [], [(0,1,2,3)])
    uv = me.uv_layers.new()
    for lp in me.loops:
        uv.data[lp.index].uv = [(0,0),(1,0),(1,1),(0,1)][lp.vertex_index]
    me.update(); me.materials.append(mat); o.location = loc
    return o

plane("Table", 2000, color_mat("table", (160, 152, 138), 0.95), (0, 0, -2))
plane("Board", 540, img_mat("Board", f"{HERE}/board_hex_2000.png"), (0, 0, 0))

# Import sculpt grafts (Z-up files); fall back to parametric grafts if sculpt missing
def import_piece(name):
    sculpt = f"{HERE}/out/{name}_sculpt_graft.obj"
    parametric = f"{HERE}/out/{name}_graft.obj"
    path = sculpt if os.path.exists(sculpt) else parametric
    before = set(bpy.data.objects)
    bpy.ops.wm.obj_import(filepath=path, up_axis='Z', forward_axis='Y')
    objs = [o for o in bpy.data.objects if o not in before and o.type == 'MESH']
    if len(objs) > 1:
        bpy.ops.object.select_all(action='DESELECT')
        for x in objs: x.select_set(True)
        bpy.context.view_layer.objects.active = objs[0]
        bpy.ops.object.join()
    o = objs[0]; o.name = name
    o.data.materials.clear()
    o.data.materials.append(color_mat(f"{name}_mat", BLUE_RGB))
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.shade_smooth()
    return o

def import_food():
    path = f"{HERE}/renders/food/FOOD_snap.stl"   # snap fit for the beauty shot
    before = set(bpy.data.objects)
    try: bpy.ops.wm.stl_import(filepath=path)
    except Exception: bpy.ops.import_mesh.stl(filepath=path)
    o = next(x for x in bpy.data.objects if x not in before and x.type == 'MESH')
    o.name = "FOOD_template"
    o.data.materials.clear()
    o.data.materials.append(color_mat("food_mat", FOOD_RGB, rough=0.55))
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
    bpy.context.view_layer.objects.active = o
    bpy.ops.object.shade_smooth()
    o.hide_render = True; o.location = (9000, 9000, 0)   # park template
    return o

food_T = import_food()
pieces = {n: import_piece(n) for n in ("EAT", "MOVE", "GROW")}

# Compute each piece's seat plane (top of pedestal, where the food sits)
def top_z(o):
    return max((o.matrix_world @ v.co).z for v in o.data.vertices)

seat_z = {n: top_z(p) - DOME_HEIGHT for n, p in pieces.items()}
print("seat z per piece:", seat_z)

# Place pieces on 3 board hex spaces. C:2, C:1, C:0 form a perfect symmetric row at
# y=+74.5 (43mm spacing), reading as the green band of spaces behind the camera's
# central view. Same rotation+scale convention as build_play_real.py (BROT=30deg
# rotates the canonical hex frame into the printed art's lattice).
ROOM = os.environ.get("OGF", "/home/youdonotexist/code/organism/ogf/zach-dan-ryan.json")
G = ogf.load_ogf(ROOM); LOC = ogf.board_locations(G)
BROT = math.radians(30); SCALE = 43.0
def P2(sp):
    x, y = LOC[sp]
    xr = x * math.cos(BROT) - y * math.sin(BROT)
    yr = x * math.sin(BROT) + y * math.cos(BROT)
    return (xr * SCALE, yr * SCALE)

# MOVE on A:0 (board center), EAT left on B:2 (-43, 0), GROW right on B:5 (+43, 0).
# Three horizontal-row spaces through the center; spacing 43mm.
spaces = {"EAT": "B:2", "MOVE": "A:0", "GROW": "B:5"}
positions = {n: P2(s) for n, s in spaces.items()}
print("piece positions:", positions)

order = ["EAT", "MOVE", "GROW"]
for idx, n in enumerate(order):
    p = pieces[n]
    bpy.ops.object.select_all(action='DESELECT'); p.select_set(True)
    bpy.context.view_layer.objects.active = p
    bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
    x, y = positions[n]
    p.location = (x, y, 0)
    # stack `count` food tokens on top; each step is FOOD_STACK_DZ
    count = FOOD_COUNTS[idx] if idx < len(FOOD_COUNTS) else 0
    for k in range(count):
        fo = food_T.copy(); C().objects.link(fo); fo.hide_render = False
        fo.name = f"FOOD_{n}_{k}"
        fo.location = (x, y, seat_z[n] + k * FOOD_STACK_DZ)
print(f"food counts per piece (EAT/MOVE/GROW): {FOOD_COUNTS}")

# Lights: warm key + cool fill, gentle rim from behind
def sun(name, energy, rot, color=(1, 1, 1)):
    d = bpy.data.lights.new(name, 'SUN'); d.energy = energy
    d.color = color; d.angle = math.radians(4)
    o = bpy.data.objects.new(name, d); C().objects.link(o)
    o.rotation_euler = rot; return o

sun("Key",  4.5, (math.radians(48), math.radians(10),  math.radians(30)),  color=(1.0, 0.97, 0.92))
sun("Fill", 1.6, (math.radians(60), math.radians(-15), math.radians(-130)), color=(0.92, 0.95, 1.0))
sun("Rim",  2.0, (math.radians(110), 0, math.radians(0)), color=(1.0, 0.95, 0.88))

# Camera: 3q (perspective low-angle) or top (orthographic overhead).
tgt = bpy.data.objects.new("Target", None); C().objects.link(tgt)
camd = bpy.data.cameras.new("Cam"); camd.clip_end = 5000
cam = bpy.data.objects.new("Cam", camd); C().objects.link(cam); sc.camera = cam
if VIEW == "top":
    # Orthographic so the pieces project at their true plan-view sizes (no perspective
    # foreshortening — useful as a reference image or for documentation)
    camd.type = 'ORTHO'
    camd.ortho_scale = 230                                # mm wide field of view
    tgt.location = (0, positions["MOVE"][1], 0)
    cam.location = (0, positions["MOVE"][1], 400)         # straight overhead
    cam.rotation_mode = 'XYZ'; cam.rotation_euler = (0, 0, 0)
else:  # "3q"
    tgt.location = (0, positions["MOVE"][1], 25)
    camd.lens = 60
    cam.location = (0, positions["MOVE"][1] - 320, 145)   # 3/4, pulled back + raised
    con = cam.constraints.new('TRACK_TO'); con.target = tgt
    con.track_axis = 'TRACK_NEGATIVE_Z'; con.up_axis = 'UP_Y'

# Render + save
sc.render.filepath = OUT
bpy.ops.render.render(write_still=True)
print(f"wrote {OUT}")
