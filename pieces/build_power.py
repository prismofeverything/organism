"""#4 POWER BOARD (score track) clip.

The power board is the nested-arc "pyramid": 6 level nodes up the centerline, 0 (wide
purple base) -> 5 (gold apex). Player discs sit on the node matching their power. This
clip shows an end-game: green holds 3, red climbs 4->5, purple climbs 4->6 — and 6
overflows the track, so purple lands a lap disc on 5 and its marker on 1 (5 + 1 = 6).

  CLIP frames -> renders/clips/power/ ; CHECK=1 drops a marker on every level (alignment).
"""
import bpy, os, math
from mathutils import Vector

P = os.path.dirname(os.path.abspath(__file__))
ASSETS = f"{P}/clip_assets"
FR = os.environ.get("FR", f"{P}/renders/clips/power"); os.makedirs(FR, exist_ok=True)
RES = int(os.environ.get("RES", "1080")); SAMPLES = int(os.environ.get("SAMPLES", "24"))
FPS = int(os.environ.get("FPS", "24")); LEN = int(os.environ.get("LEN", "140"))

def _l(c): return c/12.92 if c <= 0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]), _l(c[1]), _l(c[2]), 1.0)
import colorsys, re
def hsl(s):
    h, sa, li = re.match(r"hsl\((\d+),(\d+)%?,(\d+)%?\)", s).groups()
    return colorsys.hls_to_rgb(float(h)/360, float(li)/100, float(sa)/100)
PCOL = {"green": hsl("hsl(118,48%,55%)"), "red": hsl("hsl(353,65%,57%)"), "purple": hsl("hsl(266,60%,60%)")}

bpy.ops.wm.read_factory_settings(use_empty=True); sc = bpy.context.scene
try: sc.render.engine = "BLENDER_EEVEE_NEXT"
except Exception: sc.render.engine = "BLENDER_EEVEE"
try: sc.eevee.taa_render_samples = SAMPLES
except Exception: pass
sc.render.resolution_x = sc.render.resolution_y = RES; sc.render.fps = FPS
try: sc.view_settings.view_transform = 'Standard'
except Exception: pass
w = bpy.data.worlds.new("W"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (0.05, 0.05, 0.07, 1.0)

def C(): return bpy.context.collection
def colmat(name, rgb, rough=0.4):
    m = bpy.data.materials.new(name); m.use_nodes = True
    b = m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value = lin3(rgb); b.inputs["Roughness"].default_value = rough
    return m
for nm, en, rot in [("S", 3.4, (math.radians(58), 0, math.radians(20))),
                    ("S2", 1.2, (math.radians(64), 0, math.radians(-130)))]:
    d = bpy.data.lights.new(nm, 'SUN'); d.energy = en; d.angle = math.radians(4)
    o = bpy.data.objects.new(nm, d); C().objects.link(o); o.rotation_euler = rot

# power board plane (shadeless, faithful printed colors)
W = 360.0
me = bpy.data.meshes.new("PB"); board = bpy.data.objects.new("PB", me); C().objects.link(board)
s = W/2; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)], [], [(0,1,2,3)])
uv = me.uv_layers.new()
for lp in me.loops: uv.data[lp.index].uv = [(0,0),(1,0),(1,1),(0,1)][lp.vertex_index]
me.update()
bm = bpy.data.materials.new("pb_m"); bm.use_nodes = True; nt = bm.node_tree
pb = nt.nodes["Principled BSDF"]; t = nt.nodes.new("ShaderNodeTexImage")
t.image = bpy.data.images.load(f"{ASSETS}/power_board.png")
hsv = nt.nodes.new("ShaderNodeHueSaturation"); hsv.inputs["Saturation"].default_value = 1.15
nt.links.new(t.outputs["Color"], hsv.inputs["Color"]); nt.links.new(hsv.outputs["Color"], pb.inputs["Emission Color"])
pb.inputs["Emission Strength"].default_value = 1.0; pb.inputs["Base Color"].default_value = (0, 0, 0, 1)
me.materials.append(bm)

# level nodes (normalized centreline positions, measured from the art) -> world (x,y)
NY = {0: -0.80, 1: -0.507, 2: -0.193, 3: 0.127, 4: 0.45, 5: 0.76}
def lvl(v, dx=0.0): return (dx, NY[v]*(W/2))
DISC_Z = 3.2

def disc(name, rgb):
    bpy.ops.mesh.primitive_cylinder_add(vertices=44, radius=18, depth=5.0, location=(9000, 9000, 0))
    o = bpy.context.active_object; o.name = name           # flat coin (no smooth -> crisp rim)
    try: bpy.ops.object.shade_flat()
    except Exception: pass
    o.data.materials.append(colmat(name+"_m", rgb)); o.scale = (0, 0, 0)
    return o
def key(o, fr, xy=None, sc_=None):
    if xy is not None: o.location = (xy[0], xy[1], DISC_Z); o.keyframe_insert("location", frame=fr)
    if sc_ is not None: o.scale = (sc_, sc_, sc_); o.keyframe_insert("scale", frame=fr)

sc.frame_start = 0; sc.frame_end = LEN

if os.environ.get("CHECK"):
    for v in range(6):
        d = disc(f"chk{v}", (1, 0.2, 0.2)); key(d, 0, lvl(v), 1.0)
else:
    # green: holds at 3
    g = disc("green", PCOL["green"]);
    for f in (0, LEN): key(g, f, lvl(3), 1.0)
    # red: 4 -> 5  (left side of the nodes)
    r = disc("red", PCOL["red"])
    key(r, 0, lvl(4, -0.12), 1.0); key(r, 45, lvl(4, -0.12), 1.0)
    key(r, 92, lvl(5, -0.12), 1.0); key(r, LEN, lvl(5, -0.12), 1.0)
    # purple main: 4 -> 5 -> overflow, fade out at apex and reappear on level 1
    pm = disc("purple_main", PCOL["purple"])
    key(pm, 0, lvl(4, 0.12), 1.0); key(pm, 45, lvl(4, 0.12), 1.0)
    key(pm, 92, lvl(5, 0.12), 1.0); key(pm, 100, lvl(5, 0.12), 0.0)         # vanish at apex
    pm.location = (lvl(1)[0], lvl(1)[1], DISC_Z); pm.keyframe_insert("location", frame=101)  # jump while hidden
    key(pm, 110, lvl(1), 1.0); key(pm, LEN, lvl(1), 1.0)                    # reappear on 1
    # purple lap disc: appears on 5 as the marker that purple has lapped (5 + 1 = 6)
    pl = disc("purple_lap", PCOL["purple"])
    key(pl, 0, lvl(5, 0.12), 0.0); key(pl, 96, lvl(5, 0.12), 0.0); key(pl, 106, lvl(5, 0.12), 1.0)
    key(pl, LEN, lvl(5, 0.12), 1.0)

# camera: mostly top-down on the flat board, slight tilt + slow drift
cam_d = bpy.data.cameras.new("Cam"); cam_d.lens = 50; cam_d.clip_end = 20000
cam = bpy.data.objects.new("Cam", cam_d); C().objects.link(cam); sc.camera = cam
cam.rotation_mode = 'QUATERNION'
def smooth(u): return u*u*(3-2*u)
for f in range(LEN+1):
    u = f/LEN
    # camera on the -Y side looking +Y so the board's apex points up/away in frame
    az = math.radians(-90 + smooth(u)*11); el = math.radians(65 - smooth(u)*2.5); dist = 520 - smooth(u)*16
    loc = Vector((dist*math.cos(el)*math.cos(az), dist*math.cos(el)*math.sin(az), dist*math.sin(el)))
    cam.location = loc; cam.rotation_quaternion = (Vector((0, 0, 6)) - loc).to_track_quat('-Z', 'Y')
    cam.keyframe_insert("location", frame=f); cam.keyframe_insert("rotation_quaternion", frame=f)

single = os.environ.get("FRAME")
sc.render.image_settings.file_format = 'PNG'
if single:
    sc.frame_set(int(single)); sc.render.filepath = f"{FR}/single_{int(single):04d}"
    bpy.ops.render.render(write_still=True)
else:
    sc.render.filepath = f"{FR}/f"; bpy.ops.render.render(animation=True)
print("power done")
