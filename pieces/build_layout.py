"""ORGANISM full component layout -- a "what's in the box" hero still / future final state of the
box-opening animation. Back row tucked behind the board: rulebook (left), standing box (center,
cover front + wrap sides), power board (right) with each player's power tokens stacked on it.
The round PENTAGONAL board fills the foreground; the five player sets curve along TWO arcs (a
"football" around the board, front + back open); the 6th arc slot holds ALL the food in upright
stacks; round mutation cards fan up top.

  ~/Downloads/.../blender -b --python build_layout.py          # still -> renders/clips/layout/
  RES=1200 SAMPLES=20 ... build_layout.py                      # quick preview
"""
import bpy, os, math, glob, colorsys
from mathutils import Vector

P = os.path.dirname(os.path.abspath(__file__))
ASSETS = f"{P}/clip_assets"; LA = f"{P}/layout_assets"
FR = os.environ.get("FR", f"{P}/renders/clips/layout"); os.makedirs(FR, exist_ok=True)
RES = int(os.environ.get("RES", "1600")); SAMPLES = int(os.environ.get("SAMPLES", "32"))

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
sc.render.resolution_x = int(os.environ.get("RESX", "1920")); sc.render.resolution_y = int(os.environ.get("RESY", "1200"))   # landscape
try: sc.view_settings.view_transform = 'Standard'
except Exception: pass
w = bpy.data.worlds.new("W"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (0.05, 0.055, 0.065, 1.0)
def C(): return bpy.context.collection
for nm, en, rot in [("Key", 4.8, (math.radians(44), math.radians(10), math.radians(38))),
                    ("Fill", 2.1, (math.radians(58), 0, math.radians(-122))),
                    ("Rim", 2.8, (math.radians(116), 0, math.radians(180)))]:
    d = bpy.data.lights.new(nm, 'SUN'); d.energy = en; d.angle = math.radians(5)
    o = bpy.data.objects.new(nm, d); C().objects.link(o); o.rotation_euler = rot

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
s = 3000; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)], [], [(0,1,2,3)]); me.update()
me.materials.append(colmat("table", (0.11, 0.12, 0.14), 0.85)); o.location = (0, 0, -0.2)

# ---------------- templates ----------------
def load(path, zup=True):
    if zup: bpy.ops.wm.obj_import(filepath=path, up_axis='Z', forward_axis='Y')
    else:   bpy.ops.wm.obj_import(filepath=path, up_axis='Y', forward_axis='NEGATIVE_Z')   # food: flat (match clips)
    o = [x for x in bpy.context.selected_objects if x.type == 'MESH'][0]
    # bake the import's axis-conversion rotation into the MESH (dup() makes fresh objects that
    # would otherwise drop the object-level rotation -> the food would render 90 deg off)
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
def disc_art(name, path, loc, r, rotz=0.0, h=2.0, shadeless=True, crop=1.0):   # round token: art centered (crop<1 zooms IN, trims border)
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

# ---------------- round pentagonal board (foreground, corners cropped) ----------------
disc_art("pentboard", f"{LA}/27_Pent_54cm_01.png", (0, 0, 0.3), 350, h=3.0, shadeless=True)

# ---------------- back row tucked behind the board: rulebook | box | power board ----------------
def standing_box(name, loc, S, H, cover, wrap):
    v = [(-S/2,0,0),(S/2,0,0),(S/2,0,S),(-S/2,0,S), (-S/2,H,0),(S/2,H,0),(S/2,H,S),(-S/2,H,S)]
    faces = [(0,1,2,3),(5,4,7,6),(4,0,3,7),(1,5,6,2),(3,2,6,7),(4,5,1,0)]
    bm = bpy.data.meshes.new(name); ob = bpy.data.objects.new(name, bm); C().objects.link(ob)
    bm.from_pydata(v, [], faces); bm.update()
    bm.materials.append(imgmat(name+"_cov", cover)); bm.materials.append(imgmat(name+"_wrap", wrap))
    for i, poly in enumerate(bm.polygons): poly.material_index = 0 if i == 0 else 1
    uv = bm.uv_layers.new()
    for poly in bm.polygons:
        nx, ny, nz = poly.normal
        for li in poly.loop_indices:
            co = bm.vertices[bm.loops[li].vertex_index].co
            if poly.material_index == 0: uv.data[li].uv = (co.x/S + 0.5, co.z/S)
            elif abs(nx) > 0.5:
                band = (0.82, 1.0) if nx > 0 else (0.18, 0.0)
                uv.data[li].uv = (band[0] + (band[1]-band[0])*(co.y/H), 0.12 + 0.76*(co.z/S))
            elif abs(nz) > 0.5:
                v0 = 0.82 if nz > 0 else 0.18; v1 = 1.0 if nz > 0 else 0.0
                uv.data[li].uv = (0.12 + 0.76*(co.x/S+0.5), v0 + (v1-v0)*(co.y/H))
            else: uv.data[li].uv = (co.x/S+0.5, co.z/S)
    for poly in bm.polygons: poly.use_smooth = False
    ob.location = loc; return ob
# OUTER RING = 5 positions; the MIDDLE ONE is left BLANK. The box is placed independently, closer to
# the board (in front of the blank middle). positions: rulebook 150, diagrams 120, [90 blank],
# mutations 60, power board 30 (off to the side at the end).
ROUT = 680                                                    # ring pushed out a bit (buffer between components; power not occluded)
def ring(deg): th = math.radians(deg); return ROUT*math.cos(th), ROUT*math.sin(th), th - math.radians(90)
RDIAG = 680                                                   # diagrams + mutations: same ring radius as rulebook/power (centered)
def ring_arc(prefix, paths, center_deg, step_deg, r, radius=None, crop=1.0):   # cards along the concentric arc (radially aligned)
    rad = RDIAG if radius is None else radius; nn = len(paths)
    for k, p in enumerate(paths):
        a = math.radians(center_deg + (k - (nn-1)/2.0)*step_deg)
        disc_art(f"{prefix}_{k}", p, (rad*math.cos(a), rad*math.sin(a), 1.2 + k*0.3),
                 r, rotz=a - math.radians(90), h=1.0, shadeless=True, crop=crop)

standing_box("box", (0, 470, 0), 240, 72, f"{ASSETS}/box_top.png", f"{ASSETS}/box_wrap.png")   # independent, closer to board

rbx, rby, rir = ring(138)                                      # rulebook (left end; ring span reduced so it's in frame)
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
book.location = (rbx, rby, 0); book.rotation_euler = (0, 0, rir)

ring_arc("aid", [f"{ASSETS}/player_aid.png"]*5, 114, 4.4, 64)   # diagrams: concentric arc, tighter spread (rulebook more in frame)

pwx, pwy, pir = ring(66)                                       # power board (swapped with mutations -> inner-right)
disc_art("powerboard", f"{ASSETS}/power_board.png", (pwx, pwy, 0.4), 140, rotz=pir, h=2.0, shadeless=True)
sdx, sdy = -math.cos(math.radians(66)), -math.sin(math.radians(66)); tdx, tdy = -sdy, sdx   # toward board + tangent
for pl_i, pl in enumerate(PLAYERS):                            # power tokens, STRAIGHT stacks on the 0 space
    cx = pwx + 96*sdx + (pl_i - 2)*24*tdx; cy = pwy + 96*sdy + (pl_i - 2)*24*tdy
    for k in range(3):
        disc(f"pow_{pl}_{k}", colmat(f"pw_{pl}", COLORS[pl], 0.5), (cx, cy, 2.0 + k*2.6), 11, h=2.4)

# ---------------- five player sets + food on TWO curved arcs (a football around the board) ----------------
ARC = [(130, "food"), (180, "red"), (230, "yellow"), (50, "blue"), (0, "purple"), (-50, "green")]   # food back-left
R = 420
DCOL = math.radians(5.6)                                      # angular spacing per column -> every formation curves with the board
for deg, what in ARC:
    th = math.radians(deg)
    def ppos(dcol, drad):                                     # polar: (column offset, radial offset) -> world x,y + facing rot
        a = th + dcol*DCOL; rr = R + drad
        return rr*math.cos(a), rr*math.sin(a), a - math.radians(90)
    if what == "food":                                        # all 60 food: upright stacks on a curved polar grid
        n = 0; col = 0
        while n < 60:
            hc = 3 + int(rnd(col)*6)
            bx, by, _ = ppos(col % 4 - 1.5, (col // 4 - 1)*44)
            for k in range(hc):
                if n >= 60: break
                dup(FOODT, f"food_{n}", GOLD, (bx, by, ELEV + k*7.2), rnd(n)*6.28, FSCALE); n += 1
            col += 1
        continue
    pl = what
    pmat = colmat(pl+"_p", COLORS[pl], rough=0.42)
    for r, ptype in enumerate(["eat", "move", "grow"]):       # 3 rows (radii) x 4 cols (along the arc) -> curves with board
        for c in range(4):
            wx, wy, prot = ppos(c - 1.5, (r - 1)*44)
            dup(TPL[ptype], f"{pl}_{ptype}_{c}", pmat, (wx, wy, ELEV), prot, PSCALE)
    for k in range(5):                                        # 5 platforms stacked (printed dotted-token art, lit, centered+clipped)
        wx, wy, prot = ppos(-2.7, 0)
        disc_art(f"{pl}_plat_{k}", f"{LA}/plats/{pl}.png", (wx, wy, 1.0+k*2.4), SPACE_R, rotz=prot, h=2.2, crop=0.82, shadeless=False)

# ---------------- mutation cards fanned up top, in the back gap ----------------
mcards = sorted(glob.glob(f"{LA}/cards/card_*.png"))         # mutations: TWO rows STRADDLING the ring radius (centered, not pushed out)
h2 = (len(mcards) + 1)//2
ring_arc("mut0", mcards[:h2], 42, 2.6, 34, radius=RDIAG - 38, crop=0.9)
ring_arc("mut1", mcards[h2:], 42, 2.8, 34, radius=RDIAG + 38, crop=0.9)

# ---------------- camera: tighter, lower (box more head-on) ----------------
cam_d = bpy.data.cameras.new("Cam"); cam_d.lens = 58; cam_d.clip_end = 14000   # zoomed in -> scene scaled up, fills the frame
cam = bpy.data.objects.new("Cam", cam_d); C().objects.link(cam); sc.camera = cam
cam.rotation_mode = 'QUATERNION'
# camera as a PHYSICAL OBJECT: HORIZ in front of the scene (-Y), at height CAMZ, aimed at the scene.
# lower CAMZ = camera moves straight DOWN = more head-on with the box + content rises in frame.
tgt = Vector((0, 150, 72))                                     # camera lower (CAMZ) + aim tipped UP to compensate -> stays centered, more head-on
HORIZ, CAMZ = 1620, 560
loc = Vector((0, tgt.y - HORIZ, CAMZ))
cam.location = loc; cam.rotation_quaternion = (tgt - loc).to_track_quat('-Z', 'Y')

sc.render.image_settings.file_format = 'PNG'
fr = int(os.environ.get("FRAME", "0"))
sc.render.filepath = f"{FR}/single_{fr:04d}"
bpy.ops.render.render(write_still=True)
print("layout done ->", sc.render.filepath)
