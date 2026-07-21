"""ORGANISM box-opening animation.

Frame 0: the closed box (lid on) sits in the foreground at the ~11.5in sweet-spot scale.
The lid lifts off and stands up at the back (cover art -> the hero title). Then every
component streams OUT of the box in staggered waves, arcing up-and-over to the exact
positions of the "what's in the box" hero layout (build_layout.py): the pentagonal board
first, then the five player sets around the football arc, all the food, the mutation
tokens fanned up top, and the rulebook / power board / power tokens in the back row.

Reuses build_layout.py's placement math verbatim so the END STATE == the hero still.

  ~/Downloads/.../blender -b --threads 4 --python build_unbox.py         # full clip
  FRAME=300 RESX=960 RESY=540 ... build_unbox.py                          # single-frame check
Env: FR (out dir), RESX/RESY, SAMPLES, FRAME (single still), LAST (end frame).
"""
import bpy, os, math, glob, colorsys
from mathutils import Vector

P = os.path.dirname(os.path.abspath(__file__))
ASSETS = f"{P}/clip_assets"; LA = f"{P}/layout_assets"
FR = os.environ.get("FR", "/mnt/data/archive/organism-renders/unbox"); os.makedirs(FR, exist_ok=True)
SAMPLES = int(os.environ.get("SAMPLES", "24"))
RESX = int(os.environ.get("RESX", "1280")); RESY = int(os.environ.get("RESY", "720"))
LAST = int(os.environ.get("LAST", "310"))

def _l(c): return c/12.92 if c <= 0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]), _l(c[1]), _l(c[2]), 1.0)
def hsl(h, s, l): return colorsys.hls_to_rgb(h/360.0, l/100.0, s/100.0)
COLORS = {"green": hsl(118,45,68), "red": hsl(353,62,60), "purple": hsl(266,55,60),
          "blue": hsl(196,45,58), "yellow": hsl(45,78,62)}
PLAYERS = ["green", "red", "yellow", "blue", "purple"]
FOOD_RGB = (242/255, 230/255, 158/255)
def rnd(seed): x = math.sin(seed*12.9898)*43758.5453; return x - math.floor(x)

# ---------------- scene ----------------
bpy.ops.wm.read_factory_settings(use_empty=True); sc = bpy.context.scene
try: sc.render.engine = "BLENDER_EEVEE_NEXT"
except Exception: sc.render.engine = "BLENDER_EEVEE"
try: sc.eevee.taa_render_samples = SAMPLES
except Exception: pass
sc.render.resolution_x = RESX; sc.render.resolution_y = RESY; sc.render.fps = 24
try: sc.view_settings.view_transform = 'Standard'
except Exception: pass
w = bpy.data.worlds.new("W"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (0.05, 0.055, 0.065, 1.0)

def C(): return bpy.context.collection
def sun(nm, en, rx, rz, col=(1,1,1)):
    d = bpy.data.lights.new(nm,'SUN'); d.energy=en; d.angle=math.radians(4); d.color=col
    o = bpy.data.objects.new(nm,d); C().objects.link(o); o.rotation_euler=(math.radians(rx),0,math.radians(rz)); return o
sun("Key", 4.8, 44, 38); sun("Fill", 2.1, 58, -122); sun("Rim", 2.8, 116, 180)

def colmat(name, rgb, rough=0.5):
    m = bpy.data.materials.new(name); m.use_nodes = True
    b = m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value = lin3(rgb); b.inputs["Roughness"].default_value = rough
    return m
def imgmat(name, path, shadeless=False):
    m = bpy.data.materials.new(name); m.use_nodes = True; nt = m.node_tree
    out = nt.nodes["Material Output"]
    if shadeless:
        for n in list(nt.nodes):
            if n.type != "OUTPUT_MATERIAL": nt.nodes.remove(n)
        t = nt.nodes.new("ShaderNodeTexImage"); t.image = bpy.data.images.load(path)
        e = nt.nodes.new("ShaderNodeEmission"); nt.links.new(t.outputs["Color"], e.inputs["Color"])
        nt.links.new(e.outputs[0], out.inputs["Surface"])
    else:
        t = nt.nodes.new("ShaderNodeTexImage"); t.image = bpy.data.images.load(path)
        b = nt.nodes["Principled BSDF"]; nt.links.new(t.outputs["Color"], b.inputs["Base Color"])
        b.inputs["Roughness"].default_value = 0.62
    return m

# ---------------- table ----------------
me = bpy.data.meshes.new("T"); o = bpy.data.objects.new("T", me); C().objects.link(o)
s = 6000; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)], [], [(0,1,2,3)]); me.update()
me.materials.append(colmat("table", (0.11, 0.12, 0.14), 0.85)); o.location = (0, 0, -0.2)

# ---------------- component templates + registry ----------------
PLACED = []   # each dict: obj, loc(Vector), rotz, scale, wave, idx
WCOUNT = {}
def register(obj, wave):
    i = WCOUNT.get(wave, 0); WCOUNT[wave] = i + 1
    PLACED.append(dict(obj=obj, loc=obj.location.copy(), rotz=obj.rotation_euler.z,
                       scale=obj.scale.x, wave=wave, idx=i))
    return obj

def load(path, zup=True):
    if zup: bpy.ops.wm.obj_import(filepath=path, up_axis='Z', forward_axis='Y')
    else:   bpy.ops.wm.obj_import(filepath=path, up_axis='Y', forward_axis='NEGATIVE_Z')
    o = [x for x in bpy.context.selected_objects if x.type == 'MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active = o
    bpy.ops.object.transform_apply(location=False, rotation=True, scale=True)
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    if not o.data.materials: o.data.materials.append(colmat(o.name+"_b", (0.5,0.5,0.5)))
    o.hide_render = True; o.location = (9000, 9000, 0); return o
TPL = {t: load(f"{P}/out/{m}_sculpt_graft.obj") for t, m in [("eat","EAT"),("move","MOVE"),("grow","GROW")]}
FOODT = load(f"{P}/renders/food/FOOD_slip.obj", zup=False)
GOLD = colmat("gold", FOOD_RGB, rough=0.5)

def dup(tpl, name, mat, loc, rotz=0.0, scale=1.0):
    o = bpy.data.objects.new(name, tpl.data); C().objects.link(o)
    o.location = loc; o.rotation_euler = (0, 0, rotz); o.scale = (scale, scale, scale)
    o.material_slots[0].link = 'OBJECT'; o.material_slots[0].material = mat; return o
def disc(name, mat, loc, r, h=2.0, rotz=0.0):
    bpy.ops.mesh.primitive_cylinder_add(vertices=64, radius=r, depth=h, location=loc)
    o = bpy.context.active_object; o.name = name; o.rotation_euler = (0, 0, rotz)
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    o.data.materials.clear(); o.data.materials.append(mat); return o
def disc_art(name, path, loc, r, rotz=0.0, h=2.0, shadeless=True, crop=1.0):
    o = disc(name, colmat(name+"_e", (0.05,0.06,0.08), 0.7), loc, r, h=h, rotz=0)
    o.data.materials.append(imgmat(name+"_f", path, shadeless=shadeless))
    for poly in o.data.polygons: poly.material_index = 1 if poly.normal.z > 0.9 else 0
    uv = o.data.uv_layers[0] if o.data.uv_layers else o.data.uv_layers.new()
    for poly in o.data.polygons:
        for li in poly.loop_indices:
            co = o.data.vertices[o.data.loops[li].vertex_index].co
            uv.data[li].uv = (0.5 + crop*co.x/(2*r), 0.5 + crop*co.y/(2*r))
    o.rotation_euler = (0, 0, rotz); return o

ELEV = 1.0; PSCALE = 1.5; FSCALE = 1.3; SPACE_R = 21.0

# ---------------- FINAL positions (copied from build_layout.py) ----------------
register(disc_art("pentboard", f"{LA}/27_Pent_54cm_01.png", (0, 0, 0.3), 350, h=3.0, shadeless=True), "board")

RDIAG = 770; ROUT = 770
def ring(deg): th = math.radians(deg); return ROUT*math.cos(th), ROUT*math.sin(th), th - math.radians(90)
def ring_arc(prefix, paths, center_deg, step_deg, r, radius=None, crop=1.0, wave="mut"):
    rad = RDIAG if radius is None else radius; nn = len(paths)
    for k, p in enumerate(paths):
        a = math.radians(center_deg + (k - (nn-1)/2.0)*step_deg)
        register(disc_art(f"{prefix}_{k}", p, (rad*math.cos(a), rad*math.sin(a), 1.2 + k*0.3),
                 r, rotz=a - math.radians(90), h=1.0, shadeless=True, crop=crop), wave)

# rulebook (left)
rbx, rby, rir = ring(138)
bm = bpy.data.meshes.new("book"); book = bpy.data.objects.new("book", bm); C().objects.link(book)
RW, RT = 200.0, 6.0
bv = [(-RW/2,-RW/2,0),(RW/2,-RW/2,0),(RW/2,RW/2,0),(-RW/2,RW/2,0),
      (-RW/2,-RW/2,RT),(RW/2,-RW/2,RT),(RW/2,RW/2,RT),(-RW/2,RW/2,RT)]
bm.from_pydata(bv, [], [(4,5,6,7),(0,3,2,1),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); bm.update()
bm.materials.append(imgmat("book_cov", f"{ASSETS}/box_top.png")); bm.materials.append(colmat("book_s", (0.9,0.9,0.88), 0.7))
for i, poly in enumerate(bm.polygons): poly.material_index = 0 if i == 0 else 1
uv = bm.uv_layers.new()
for li in bm.polygons[0].loop_indices:
    vi = bm.loops[li].vertex_index; uv.data[li].uv = {4:(0,0),5:(1,0),6:(1,1),7:(0,1)}[vi]
book.location = (rbx, rby, 0); book.rotation_euler = (0, 0, rir); register(book, "rule")

ring_arc("aid", [f"{ASSETS}/player_aid.png"]*5, 114, 4.4, 64, wave="aid")

# power board + power tokens (right)
pwx, pwy, pir = ring(66)
register(disc_art("powerboard", f"{ASSETS}/power_board.png", (pwx, pwy, 0.4), 140, rotz=pir, h=2.0, shadeless=True), "power")
sdx, sdy = -math.cos(math.radians(66)), -math.sin(math.radians(66)); tdx, tdy = -sdy, sdx
for pl_i, pl in enumerate(PLAYERS):
    cx = pwx + 96*sdx + (pl_i - 2)*24*tdx; cy = pwy + 96*sdy + (pl_i - 2)*24*tdy
    for k in range(3):
        register(disc(f"pow_{pl}_{k}", colmat(f"pw_{pl}", COLORS[pl], 0.5), (cx, cy, 2.0 + k*2.6), 11, h=2.4), "ptok")

# five player sets + food on TWO curved arcs (a football around the board)
ARC = [(130, "food"), (180, "red"), (230, "yellow"), (50, "blue"), (0, "purple"), (-50, "green")]
R = 420; DCOL = math.radians(5.6)
for deg, what in ARC:
    th = math.radians(deg)
    def ppos(dcol, drad, th=th):
        a = th + dcol*DCOL; rr = R + drad
        return rr*math.cos(a), rr*math.sin(a), a - math.radians(90)
    if what == "food":
        n = 0; col = 0
        while n < 60:
            hc = 3 + int(rnd(col)*6)
            bx, by, _ = ppos(col % 4 - 1.5, (col // 4 - 1)*44)
            for k in range(hc):
                if n >= 60: break
                register(dup(FOODT, f"food_{n}", GOLD, (bx, by, ELEV + k*7.2), rnd(n)*6.28, FSCALE), "food"); n += 1
            col += 1
        continue
    pl = what; pmat = colmat(pl+"_p", COLORS[pl], rough=0.42)
    for r, ptype in enumerate(["eat", "move", "grow"]):
        for c in range(4):
            wx, wy, prot = ppos(c - 1.5, (r - 1)*44)
            register(dup(TPL[ptype], f"{pl}_{ptype}_{c}", pmat, (wx, wy, ELEV), prot, PSCALE), pl)
    for k in range(5):
        wx, wy, prot = ppos(-2.7, 0)
        register(disc_art(f"{pl}_plat_{k}", f"{LA}/plats/{pl}.png", (wx, wy, 1.0+k*2.4), SPACE_R,
                          rotz=prot, h=2.2, crop=0.8, shadeless=False), pl)

# mutation tokens fanned up top
mcards = sorted(glob.glob(f"{LA}/cards/card_*.png"))
t3 = (len(mcards) + 2)//3
ring_arc("mut0", mcards[:t3],     42, 2.7, 34, radius=RDIAG - 54, crop=0.9, wave="mut")
ring_arc("mut1", mcards[t3:2*t3], 42, 2.7, 34, radius=RDIAG,      crop=0.9, wave="mut")
ring_arc("mut2", mcards[2*t3:],   42, 2.9, 34, radius=RDIAG + 54, crop=0.9, wave="mut")

# ================= THE BOX (tray + lid) =================
TY = -540.0; BW = 300.0; TH = 46.0; hw = BW/2   # box in the clear foreground, in front of the board
tray_dark = colmat("tray", (0.10, 0.16, 0.18), 0.6)
tv = [(-hw,-hw,0),(hw,-hw,0),(hw,hw,0),(-hw,hw,0),(-hw,-hw,TH),(hw,-hw,TH),(hw,hw,TH),(-hw,hw,TH)]
tf = [(0,1,2,3),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]   # bottom + 4 walls, open top
tm = bpy.data.meshes.new("tray"); tray = bpy.data.objects.new("tray", tm); C().objects.link(tray)
tm.from_pydata(tv, [], tf); tm.update(); tm.materials.append(tray_dark)
for poly in tm.polygons: poly.use_smooth = False
tray.location = (0, TY, 0)

BWL = BW + 8; LH = 16.0; hl = BWL/2
lv = [(-hl,-hl,0),(hl,-hl,0),(hl,hl,0),(-hl,hl,0),(-hl,-hl,LH),(hl,-hl,LH),(hl,hl,LH),(-hl,hl,LH)]
lf = [(4,5,6,7),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]   # top plate (cover) + 4 skirt walls
lm = bpy.data.meshes.new("lid"); lid = bpy.data.objects.new("lid", lm); C().objects.link(lid)
lm.from_pydata(lv, [], lf); lm.update()
lm.materials.append(imgmat("lid_cov", f"{ASSETS}/box_top.png")); lm.materials.append(colmat("lid_s", (0.09,0.14,0.16), 0.55))
for i, poly in enumerate(lm.polygons): poly.material_index = 0 if i == 0 else 1
uv = lm.uv_layers.new()
for li in lm.polygons[0].loop_indices:
    vi = lm.loops[li].vertex_index; uv.data[li].uv = {4:(0,0),5:(1,0),6:(1,1),7:(0,1)}[vi]
for poly in lm.polygons: poly.use_smooth = False

# ================= ANIMATION =================
MOUTH = Vector((0, TY, TH + 8))
WAVE_START = {"board":30, "aid":214, "green":60, "red":82, "yellow":104, "blue":126,
              "purple":148, "food":170, "mut":206, "rule":232, "power":240, "ptok":248}
STAG = {"food":0.7, "mut":0.8, "ptok":0.7}
DUR  = {"board":48, "food":22, "mut":20}

def key_loc(o, v, f): o.location = v; o.keyframe_insert("location", frame=f)
def key_rot(o, rz, f): o.rotation_euler = (0, 0, rz); o.keyframe_insert("rotation_euler", frame=f)
def key_scl(o, s, f): o.scale = (s, s, s); o.keyframe_insert("scale", frame=f)

for p in PLACED:
    o = p["obj"]; L = p["loc"]; R = p["rotz"]; S = p["scale"]; wv = p["wave"]
    t0 = WAVE_START[wv] + p["idx"] * STAG.get(wv, 0.9)
    dur = DUR.get(wv, 26); tm_ = t0 + dur*0.45; t1 = t0 + dur
    apex = (MOUTH + L)*0.5 + Vector((0, 0, 120 + 0.12*(L - MOUTH).length))
    key_loc(o, MOUTH, 0);            key_scl(o, 0.0, 0);      key_rot(o, R - 0.5, 0)
    key_loc(o, MOUTH, t0);           key_scl(o, 0.0, t0);     key_rot(o, R - 0.5, t0)
    key_loc(o, apex, tm_);           key_scl(o, S*0.92, tm_)
    key_loc(o, L, t1);               key_scl(o, S, t1);       key_rot(o, R, t1)

# lid: lift straight up, then swing back and stand upright at the back (cover -> hero title)
lid.location = (0, TY, TH - 8)
key_loc(lid, (0, TY, TH - 8), 0);   key_rot(lid, 0, 0)
key_loc(lid, (0, TY, TH + 150), 10); key_rot(lid, 0, 10)
key_loc(lid, (0, 500, hl + 2), 40); lid.rotation_euler = (math.radians(90), 0, 0); lid.keyframe_insert("rotation_euler", frame=40)
# tray: holds as the box things pour out of, then sinks away once empty -> clean hero final frame
key_loc(tray, (0, TY, 0), 0); key_loc(tray, (0, TY, 0), 284); key_loc(tray, (0, TY, -170), 306)

# ================= CAMERA =================
cam_d = bpy.data.cameras.new("Cam"); cam_d.lens = 46; cam_d.clip_end = 14000
cam = bpy.data.objects.new("Cam", cam_d); C().objects.link(cam); sc.camera = cam
aim = bpy.data.objects.new("aim", None); C().objects.link(aim)
tcon = cam.constraints.new('TRACK_TO'); tcon.target = aim; tcon.track_axis = 'TRACK_NEGATIVE_Z'; tcon.up_axis = 'UP_Y'
CAMK = [   # (frame, cam_loc, aim_loc)
    (0,   (0, -980, 250),  (0, -540, 45)),    # tight on the closed box (foreground)
    (150, (0, -1200, 450), (0, -120, 58)),    # pull back + rise as things emerge
    (300, (0, -1560, 520), (0, 140, 74)),     # settle on the hero spread
]
for fr, cl, al in CAMK:
    cam.location = cl; cam.keyframe_insert("location", frame=fr)
    aim.location = al; aim.keyframe_insert("location", frame=fr)

# ================= RENDER =================
sc.frame_start = 0; sc.frame_end = LAST
sc.render.image_settings.file_format = 'PNG'
single = os.environ.get("FRAME")
if single:
    sc.frame_set(int(single)); sc.render.filepath = f"{FR}/single_{int(single):04d}"
    bpy.ops.render.render(write_still=True); print("unbox frame ->", sc.render.filepath)
else:
    sc.render.filepath = f"{FR}/f"; bpy.ops.render.render(animation=True); print("unbox anim ->", FR)
