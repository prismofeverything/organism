"""#1 ORGANISM TITLE / BOX image — a 3D product shot of the game box (cover art on top),
with the three hero pieces beside it. Renders a still (FRAME) or a slow-drift clip.

  ~/Downloads/.../blender -b --python build_box.py            # clip -> renders/clips/box/
  FRAME=40 ... build_box.py                                   # single hero still
"""
import bpy, os, math
from mathutils import Vector

P = os.path.dirname(os.path.abspath(__file__)); ASSETS = f"{P}/clip_assets"
FR = os.environ.get("FR", f"{P}/renders/clips/box"); os.makedirs(FR, exist_ok=True)
RES = int(os.environ.get("RES", "1080")); SAMPLES = int(os.environ.get("SAMPLES", "48"))
LEN = int(os.environ.get("LEN", "120")); FPS = 24

def _l(c): return c/12.92 if c <= 0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]), _l(c[1]), _l(c[2]), 1.0)

bpy.ops.wm.read_factory_settings(use_empty=True); sc = bpy.context.scene
try: sc.render.engine = "BLENDER_EEVEE_NEXT"
except Exception: sc.render.engine = "BLENDER_EEVEE"
try: sc.eevee.taa_render_samples = SAMPLES
except Exception: pass
sc.render.resolution_x = sc.render.resolution_y = RES; sc.render.fps = FPS
try: sc.view_settings.view_transform = 'Standard'
except Exception: pass
w = bpy.data.worlds.new("W"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (0.20, 0.21, 0.24, 1.0)

def C(): return bpy.context.collection
def colmat(name, rgb, rough=0.5):
    m = bpy.data.materials.new(name); m.use_nodes = True
    b = m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value = lin3(rgb); b.inputs["Roughness"].default_value = rough
    return m
def imgmat(name, path):
    m = bpy.data.materials.new(name); m.use_nodes = True; nt = m.node_tree
    b = nt.nodes["Principled BSDF"]; t = nt.nodes.new("ShaderNodeTexImage")
    t.image = bpy.data.images.load(path); nt.links.new(t.outputs["Color"], b.inputs["Base Color"])
    b.inputs["Roughness"].default_value = 0.6
    return m
for nm, en, rot in [("Key", 4.0, (math.radians(50), math.radians(8), math.radians(35))),
                    ("Fill", 1.5, (math.radians(62), 0, math.radians(-120))),
                    ("Rim", 2.2, (math.radians(108), 0, 0))]:
    d = bpy.data.lights.new(nm, 'SUN'); d.energy = en; d.angle = math.radians(4)
    o = bpy.data.objects.new(nm, d); C().objects.link(o); o.rotation_euler = rot

# table
me = bpy.data.meshes.new("T"); o = bpy.data.objects.new("T", me); C().objects.link(o)
s = 1400; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)], [], [(0,1,2,3)]); me.update()
me.materials.append(colmat("table", (0.16, 0.17, 0.19), 0.9)); o.location = (0, 0, -0.1)

# box: cover art on top, dark sides
HW, HT = 110.0, 58.0
v = [(-HW,-HW,0),(HW,-HW,0),(HW,HW,0),(-HW,HW,0), (-HW,-HW,HT),(HW,-HW,HT),(HW,HW,HT),(-HW,HW,HT)]
faces = [(4,5,6,7), (0,3,2,1), (0,1,5,4), (1,2,6,5), (2,3,7,6), (3,0,4,7)]   # top first
bm = bpy.data.meshes.new("box"); box = bpy.data.objects.new("box", bm); C().objects.link(box)
bm.from_pydata(v, [], faces); bm.update()
bm.materials.append(imgmat("cover", f"{ASSETS}/box_top.png"))      # slot 0 = top
bm.materials.append(colmat("side", (0.12, 0.17, 0.19), 0.55))      # slot 1 = sides/bottom
for i, poly in enumerate(bm.polygons): poly.material_index = 0 if i == 0 else 1
uv = bm.uv_layers.new()
for lp in bm.polygons[0].loop_indices:                            # map cover onto the top face
    vi = bm.loops[lp].vertex_index
    uv.data[lp].uv = {4: (0, 0), 5: (1, 0), 6: (1, 1), 7: (0, 1)}[vi]
box.location = (0, 0, 0)
for poly in bm.polygons: poly.use_smooth = False

# three hero pieces beside the box (parametric grafts)
def piece(name, color, loc, rotz):
    pa = f"{P}/out/{name}_graft.obj"
    bpy.ops.wm.obj_import(filepath=pa, up_axis='Z', forward_axis='Y')
    o = [x for x in bpy.context.selected_objects if x.type == 'MESH'][0]
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    o.data.materials.clear(); o.data.materials.append(colmat(name+"_m", color))
    o.scale = (2.0, 2.0, 2.0); o.location = loc; o.rotation_euler = (0, 0, rotz)
    return o
piece("EAT",  (0.83, 0.30, 0.36), (165, -70, 0), math.radians(20))
piece("MOVE", (0.42, 0.64, 0.70), (205, 35, 0), math.radians(-40))
piece("GROW", (0.55, 0.42, 0.70), (150, 120, 0), math.radians(120))

cam_d = bpy.data.cameras.new("Cam"); cam_d.lens = 58; cam_d.clip_end = 9000
cam = bpy.data.objects.new("Cam", cam_d); C().objects.link(cam); sc.camera = cam
cam.rotation_mode = 'QUATERNION'
def smooth(u): return u*u*(3-2*u)
tgt = Vector((40, 10, 28))
def cam_at(u):
    az = math.radians(-58 + smooth(u)*10); el = math.radians(34 - smooth(u)*3); dist = 470 - smooth(u)*18
    return Vector((dist*math.cos(el)*math.cos(az), dist*math.cos(el)*math.sin(az), 30 + dist*math.sin(el)))
sc.frame_start = 0; sc.frame_end = LEN
prev = None
for f in range(LEN+1):
    loc = cam_at(f/LEN); q = (tgt - loc).to_track_quat('-Z', 'Y')
    if prev is not None and prev.dot(q) < 0: q = -q
    prev = q
    cam.location = loc; cam.rotation_quaternion = q
    cam.keyframe_insert("location", frame=f); cam.keyframe_insert("rotation_quaternion", frame=f)

sc.render.image_settings.file_format = 'PNG'
single = os.environ.get("FRAME")
if single:
    sc.frame_set(int(single)); sc.render.filepath = f"{FR}/single_{int(single):04d}"
    bpy.ops.render.render(write_still=True)
else:
    sc.render.filepath = f"{FR}/f"; bpy.ops.render.render(animation=True)
print("box done")
