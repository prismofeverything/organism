"""ORGANISM component manifest -- one giant long image listing EVERY component.

Name on the left, the actual components on the right, one row per component type:
"20 EATERS" and then twenty rendered eaters, four in each player colour. Two
editions, from the same bill of materials the packing solver uses:

  MODE=maximal   the big box: 3D pieces, food, platforms, power tokens,
                 mutations, both boards, aids, rulebook, box
  MODE=minimal   the small box: silhouette disks, food, board, rulebook

Everything is one Blender scene rendered in one pass -- the labels are 3D text
in the same scene as the pieces, under the same lights, so the sheet is a
photograph of the whole game rather than a montage of crops.

Scale is TRUE: every template is measured on import and scaled to its real
footprint from pack_box.COMPONENTS, so a platform really is bigger than a power
token and a mutation card really does dwarf both.

  ~/Downloads/blender-5.1.2-linux-x64/blender -b --threads 4 --python build_manifest.py
Env: MODE (maximal|minimal), RESX (image width px, def 1800), SAMPLES (def 32),
     TILT (camera degrees off vertical, def 20), FR (output dir)
"""
import bpy, os, math, glob, colorsys, sys
from mathutils import Vector

P = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, P)
from pack_box import COMPONENTS                      # the measured component library = single source of truth

ASSETS = f"{P}/clip_assets"; LA = f"{P}/layout_assets"
MODE    = os.environ.get("MODE", "maximal")
FR      = os.environ.get("FR", f"{P}/renders/manifest"); os.makedirs(FR, exist_ok=True)
RESX    = int(os.environ.get("RESX", "1800"))
SAMPLES = int(os.environ.get("SAMPLES", "24"))
# 0 = straight down. Any tilt foreshortens the vertical by cos(tilt), which squashes
# every circle on the sheet -- a component list has to show true proportions, so the
# default is dead overhead and form comes from the lighting instead.
TILT    = math.radians(float(os.environ.get("TILT", "0")))
# The brand face was lifted out of the manual PDF, which never printed a 0, 7, 8
# or 9 -- those glyphs are empty in the file. JTEnergy-zero.ttf is that font with a
# zero derived from its own O (see make_zero_glyph.py); the counts need one.
FONT    = f"{P}/inputs/JTEnergy-zero.ttf"

def _l(c): return c/12.92 if c <= 0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]), _l(c[1]), _l(c[2]), 1.0)
def hsl(h, s, l): return colorsys.hls_to_rgb(h/360.0, l/100.0, s/100.0)
COLORS = {"green": hsl(118,45,68), "red": hsl(353,62,60), "purple": hsl(266,55,60),
          "blue": hsl(196,45,58), "yellow": hsl(45,78,62)}
PLAYERS = ["green", "red", "yellow", "blue", "purple"]
FOOD_RGB = (242/255, 230/255, 158/255)

# ---------------- scene ----------------
bpy.ops.wm.read_factory_settings(use_empty=True); sc = bpy.context.scene
# CPU Cycles, deliberately. EEVEE is a GPU renderer and in background mode it seizes
# whichever GPU is present -- here a 2 GB GT 1030 that is also drawing the desktop, so a
# ten-megapixel sheet froze the whole machine. Cycles on CPU never opens a GL context;
# with `nice` and a thread cap the box stays usable while it works.
ENGINE  = os.environ.get("ENGINE", "CYCLES")
THREADS = int(os.environ.get("THREADS", "6"))
sc.render.engine = ENGINE
if ENGINE == "CYCLES":
    # safe-blender sets SAFE_BLENDER_GPU=1 only after checking there is VRAM to
    # spare. Without it the GPU is not merely unused -- it is hidden.
    if os.environ.get("SAFE_BLENDER_GPU") == "1":
        prefs = bpy.context.preferences.addons["cycles"].preferences
        prefs.compute_device_type = "CUDA"          # GT 1030 is Pascal: no OptiX
        try: prefs.get_devices()
        except Exception: pass
        picked = [d.name for d in prefs.devices if d.type == "CUDA"]
        for d in prefs.devices:
            d.use = (d.type == "CUDA")
        sc.cycles.device = "GPU" if picked else "CPU"
        # Auto-tiling is what makes a 2 GB card viable for a sheet this long:
        # only one tile lives in VRAM, the full image is held in host RAM, so
        # output size stops driving VRAM use at all.
        sc.cycles.use_auto_tile = True
        sc.cycles.tile_size = int(os.environ.get("TILE", "256"))
        print(f"cycles device: {sc.cycles.device} {picked or '(no CUDA device found)'} "
              f"tile={sc.cycles.tile_size}")
    else:
        sc.cycles.device = "CPU"
    sc.cycles.samples = SAMPLES
    sc.cycles.use_adaptive_sampling = True
    try:
        sc.cycles.use_denoising = True
        sc.cycles.denoiser = "OPENIMAGEDENOISE"
    except Exception: pass
    for attr, val in [("max_bounces", 4), ("diffuse_bounces", 2), ("glossy_bounces", 2),
                      ("transmission_bounces", 2), ("volume_bounces", 0), ("transparent_max_bounces", 4)]:
        try: setattr(sc.cycles, attr, val)
        except Exception: pass
else:
    try: sc.eevee.taa_render_samples = SAMPLES
    except Exception: pass
# Leave cores for the desktop; Blender otherwise grabs every one of them.
sc.render.threads_mode = "FIXED"
sc.render.threads = THREADS
try: sc.view_settings.view_transform = 'Standard'
except Exception: pass
def C(): return bpy.context.collection

w = bpy.data.worlds.new("W"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (0.05, 0.055, 0.065, 1.0)
def sun(nm, en, rx, rz, col=(1,1,1)):
    d = bpy.data.lights.new(nm,'SUN'); d.energy=en; d.angle=math.radians(6); d.color=col
    o = bpy.data.objects.new(nm,d); C().objects.link(o); o.rotation_euler=(math.radians(rx),0,math.radians(rz)); return o
sun("Key", 4.6, 32, 38); sun("Fill", 2.0, 52, -122); sun("Rim", 2.4, 108, 180)

def colmat(name, rgb, rough=0.5):
    m = bpy.data.materials.new(name); m.use_nodes = True
    b = m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value = lin3(rgb); b.inputs["Roughness"].default_value = rough
    return m
# ---- textures -------------------------------------------------------------
# Blender holds an image as float RGBA: width x height x 4 x 4 bytes. The source
# art is print resolution -- the board is 6324 square, 610 MB -- and the naive
# version loaded a fresh copy per call, so five player aids meant five 3000-square
# buffers (687 MB) for a token that lands 135 px wide. Together that was ~3.5 GB of
# texture for one sheet, which is what took the machine down. Load each file once,
# and scale it to the pixels it actually occupies.
_IMG = {}
def tex_px(size_mm):
    """Pixels a component of this size gets on the sheet, x2 for antialiasing."""
    px = int(size_mm * (RESX / 2100.0) * 2.0)
    return max(64, min(2048, px))

def image(path, size_mm):
    key = (path, tex_px(size_mm))
    if key not in _IMG:
        img = bpy.data.images.load(path)
        w, h = img.size
        want = key[1]
        if max(w, h) > want:
            sc_ = want / float(max(w, h))
            img.scale(max(1, int(w*sc_)), max(1, int(h*sc_)))
        _IMG[key] = img
    return _IMG[key]

_MAT = {}
def imgmat(name, path, shadeless=False, size_mm=240.0):
    key = (path, shadeless, tex_px(size_mm))
    if key in _MAT: return _MAT[key]
    m = bpy.data.materials.new(name); m.use_nodes = True; nt = m.node_tree
    out = nt.nodes["Material Output"]
    img = image(path, size_mm)
    if shadeless:
        for n in list(nt.nodes):
            if n.type != "OUTPUT_MATERIAL": nt.nodes.remove(n)
        t = nt.nodes.new("ShaderNodeTexImage"); t.image = img
        e = nt.nodes.new("ShaderNodeEmission"); nt.links.new(t.outputs["Color"], e.inputs["Color"])
        nt.links.new(e.outputs[0], out.inputs["Surface"])
    else:
        t = nt.nodes.new("ShaderNodeTexImage"); t.image = img
        b = nt.nodes["Principled BSDF"]; nt.links.new(t.outputs["Color"], b.inputs["Base Color"])
        b.inputs["Roughness"].default_value = 0.62
    _MAT[key] = m
    return m

# ---------------- templates, measured then scaled to real size ----------------
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

def measure(o):
    xs = [v.co.x for v in o.data.vertices]; ys = [v.co.y for v in o.data.vertices]; zs = [v.co.z for v in o.data.vertices]
    return (max(xs)-min(xs), max(ys)-min(ys), max(zs)-min(zs), min(zs))

def fit_scale(o, target_w):
    """Scale factor that puts the template at its real-world footprint.

    The meshes come from different pipelines and not all of them are in mm
    (the food solids of revolution are metres), so nothing is assumed: measure
    the mesh, compare with the measured library in pack_box, and scale."""
    w, d, h, z0 = measure(o)
    return target_w / max(w, d) if max(w, d) > 0 else 1.0

def dup(tpl, name, mat, loc, rotz=0.0, scale=1.0):
    o = bpy.data.objects.new(name, tpl.data); C().objects.link(o)
    o.location = loc; o.rotation_euler = (0, 0, rotz); o.scale = (scale, scale, scale)
    o.material_slots[0].link = 'OBJECT'; o.material_slots[0].material = mat; return o

def disc(name, mat, loc, r, h=2.0, rotz=0.0):
    bpy.ops.mesh.primitive_cylinder_add(vertices=64, radius=r, depth=h, location=(loc[0], loc[1], loc[2]+h/2))
    o = bpy.context.active_object; o.name = name; o.rotation_euler = (0, 0, rotz)
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    o.data.materials.clear(); o.data.materials.append(mat); return o

def disc_art(name, path, loc, r, rotz=0.0, h=2.0, shadeless=True, crop=1.0):
    o = disc(name, colmat(name+"_e", (0.05,0.06,0.08), 0.7), loc, r, h=h, rotz=0)
    o.data.materials.append(imgmat(name+"_f", path, shadeless=shadeless, size_mm=2*r))
    for poly in o.data.polygons: poly.material_index = 1 if poly.normal.z > 0.9 else 0
    uv = o.data.uv_layers[0] if o.data.uv_layers else o.data.uv_layers.new()
    for poly in o.data.polygons:
        for li in poly.loop_indices:
            co = o.data.vertices[o.data.loops[li].vertex_index].co
            uv.data[li].uv = (0.5 + crop*co.x/(2*r), 0.5 + crop*co.y/(2*r))
    o.rotation_euler = (0, 0, rotz); return o

def flat_card(name, path, loc, w, h_mm, t=3.0):
    """A rectangular printed good lying face up (rulebook, player aid, board)."""
    bpy.ops.mesh.primitive_cube_add(size=1, location=(loc[0], loc[1], loc[2]+t/2))
    o = bpy.context.active_object; o.name = name; o.scale = (w, h_mm, t)
    # location/rotation default to True on this operator: without saying otherwise the
    # card's position gets baked into its vertices, and the UVs below -- which read
    # local coordinates -- slide off the texture by loc/size.
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)
    o.data.materials.append(colmat(name+"_s", (0.86,0.86,0.84), 0.7))
    o.data.materials.append(imgmat(name+"_f", path, size_mm=max(w, h_mm)))
    for poly in o.data.polygons: poly.material_index = 1 if poly.normal.z > 0.9 else 0
    uv = o.data.uv_layers[0] if o.data.uv_layers else o.data.uv_layers.new()
    for poly in o.data.polygons:
        for li in poly.loop_indices:
            co = o.data.vertices[o.data.loops[li].vertex_index].co
            uv.data[li].uv = (0.5 + co.x/w, 0.5 + co.y/h_mm)
    if os.environ.get("DEBUG_ROWS"):
        tops = sorted({(round(uv.data[li].uv[0],3), round(uv.data[li].uv[1],3))
                       for poly in o.data.polygons if poly.normal.z > 0.9 for li in poly.loop_indices})
        xs = [v.co.x for v in o.data.vertices]
        print(f"  flat_card {name}: w={w} measured_edge={max(xs)-min(xs):.1f} topUV={tops} "
              f"mats={[m.name for m in o.data.materials]}")
    return o

# ---------------- templates ----------------
TPL   = {t: load(f"{P}/out/{m}_sculpt_graft.obj") for t, m in [("eat","EAT"),("move","MOVE"),("grow","GROW")]}
MTPL  = {t: load(f"{P}/out/{m}_mindisk.obj")      for t, m in [("eat","EAT"),("move","MOVE"),("grow","GROW")]}
FOODT = load(f"{P}/renders/food/FOOD_slip.obj", zup=False)
GOLD  = colmat("gold", FOOD_RGB, rough=0.5)
PMAT  = {pl: colmat(pl+"_p", COLORS[pl], rough=0.42) for pl in PLAYERS}

SCL = {t: fit_scale(TPL[t],  max(COMPONENTS[t.upper()]["w"], COMPONENTS[t.upper()]["d"])) for t in TPL}
MSCL = {t: fit_scale(MTPL[t], COMPONENTS["DISC"]["dia"]) for t in MTPL}
FSCL = fit_scale(FOODT, COMPONENTS["FOOD"]["dia"])
print("scales: pieces", {k: round(v,4) for k,v in SCL.items()},
      "disks", {k: round(v,4) for k,v in MSCL.items()}, "food", round(FSCL,4))

# ---------------- layout engine ----------------
CONTENT_X0 = 90.0
CONTENT_W  = float(os.environ.get("CONTENT_W", "1180"))   # narrow -> rows wrap -> a long sheet
LABEL_RIGHT = -70.0
X_MIN, X_MAX = -640.0, CONTENT_X0 + CONTENT_W
ROW_GAP    = 78.0        # blank band between rows
GROUP_GAP  = 30.0        # extra space between one player's group and the next

FONTD = bpy.data.fonts.load(FONT)
# The face is a PDF subset with real holes -- no J/Q/Z, almost no lowercase, no 7/8/9,
# no full stop. Silently dropping a glyph turns "every component" into "e ery omponent",
# so every label is checked against what the file can actually draw.
DRAWABLE = set("ABCDEFGHIKLMNOPRSTUVWXY" + "aelmnoprsty" + "0123456" + " ,:-()")
def safe(body):
    missing = sorted({c for c in body if c not in DRAWABLE})
    if missing:
        raise SystemExit(f"build_manifest: {body!r} needs glyphs this font lacks: {missing}. "
                         f"Reword it (uppercase, avoid J/Q/Z and 7/8/9 and '.').")
    return body
LBLMAT = colmat("lbl", (0.93,0.94,0.96), 0.45)
SUBMAT = colmat("sub", (0.46,0.50,0.56), 0.6)

def text(body, loc, size, mat, align='RIGHT'):
    body = safe(body)
    tc = bpy.data.curves.new(body, 'FONT'); tc.body = body; tc.font = FONTD
    tc.align_x = align; tc.align_y = 'CENTER'; tc.size = size; tc.extrude = 0.8
    o = bpy.data.objects.new("t_"+body[:20], tc); C().objects.link(o)
    o.location = loc; o.rotation_euler = (TILT, 0, 0)      # billboarded: square to the camera
    o.data.materials.append(mat); return o

def flow(y_top, cells, pitch_y, x0=CONTENT_X0, max_w=CONTENT_W):
    """Place cells left to right, wrapping. Each cell is (advance, gap_after, fn(cx, cy)).
       Returns the height consumed.

    The tolerance matters: an exact `>` comparison wraps a line one item early
    whenever the accumulated float lands a whisker over the limit (19 x 38.1 is
    723.9000000000001), which turned 3 tidy lines of 20 food into 19/19/19/3."""
    EPS = 0.5                                                  # mm; far below anything visible
    x = x0; y = y_top; lines = 1
    for adv, gap, fn in cells:
        if x > x0 and (x + adv) > (x0 + max_w + EPS):
            x = x0; y -= pitch_y; lines += 1
        fn(x + adv/2.0, y - pitch_y/2.0)
        x += adv + gap
    return lines * pitch_y

# ---------------- row builders ----------------
def row_pieces(kind, y_top, per_player=4, minimal=False):
    tpl = (MTPL if minimal else TPL)[kind]
    scl = (MSCL if minimal else SCL)[kind]
    dia = COMPONENTS["DISC"]["dia"] if minimal else max(COMPONENTS[kind.upper()]["w"], COMPONENTS[kind.upper()]["d"])
    pitch = dia + 12.0
    cells = []
    for pi, pl in enumerate(PLAYERS):
        for k in range(per_player):
            last = (k == per_player-1)
            cells.append((pitch, GROUP_GAP if last else 0.0,
                          (lambda pl_, i_: lambda cx, cy: dup(
                              tpl, f"{kind}_{pl_}_{i_}", PMAT[pl_], (cx, cy, 0.0), 0.0, scl))(pl, pi*per_player+k)))
    return flow(y_top, cells, pitch + 14.0)

def row_food(y_top, n=60):
    pitch = COMPONENTS["FOOD"]["dia"] + 10.0
    cells = [(pitch, 0.0, (lambda i_: lambda cx, cy: dup(FOODT, f"food_{i_}", GOLD, (cx, cy, 0.0), (i_*37 % 360)*math.pi/180, FSCL))(i))
             for i in range(n)]
    return flow(y_top, cells, pitch + 8.0, max_w=20*pitch)          # 20 per line -> 3 even lines

def row_blocks(y_top, name, per_player, dia, thick, cols, art=None):
    """One small grid per player, every token drawn.

    These were stacks at first, which is how they live in the box -- but nine 2 mm
    platforms stacked read as a single disc from above, and the whole point of the
    sheet is that you can count what you are getting."""
    pitch = dia + 9.0
    nrow = int(math.ceil(per_player/float(cols)))
    bw = cols*pitch
    def mk(pl_):
        def place(cx, cy):
            for k in range(per_player):
                r, c = divmod(k, cols)
                x = cx - bw/2.0 + pitch/2.0 + c*pitch
                y = cy + (nrow-1)*pitch/2.0 - r*pitch
                if art: disc_art(f"{name}_{pl_}_{k}", art(pl_), (x, y, 0.0), dia/2.0, h=thick, crop=0.82, shadeless=False)
                else:   disc(f"{name}_{pl_}_{k}", PMAT[pl_], (x, y, 0.0), dia/2.0, h=thick)
        return place
    cells = [(bw, GROUP_GAP, mk(pl)) for pl in PLAYERS]
    return flow(y_top, cells, nrow*pitch + 22.0)

def row_mutations(y_top):
    cards = sorted(glob.glob(f"{LA}/cards/card_*.png"))
    dia = COMPONENTS["MUT"]["dia"]; pitch = dia + 14.0
    cells = [(pitch, 0.0, (lambda p_, i_: lambda cx, cy: disc_art(f"mut_{i_}", p_, (cx, cy, 0.0), dia/2.0, h=1.2, crop=0.94))(p, i))
             for i, p in enumerate(cards)]
    return flow(y_top, cells, pitch + 10.0, max_w=7*pitch), len(cards)   # 7 per line

def row_single(y_top, fn, w, h):
    fn(CONTENT_X0 + w/2.0, y_top - h/2.0 - 10.0)
    return h + 40.0

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

# ---------------- table ----------------
me = bpy.data.meshes.new("T"); tob = bpy.data.objects.new("T", me); C().objects.link(tob)
s = 12000; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)], [], [(0,1,2,3)]); me.update()
me.materials.append(colmat("table", (0.085, 0.095, 0.11), 0.88)); tob.location = (0, 0, -0.4)

# ---------------- the sheet ----------------
CURSOR = [-210.0]                                   # running y; row 0 starts below the title
def emit(label, builder):
    """One row: its name on the left, the components themselves on the right."""
    y_top = CURSOR[0]
    h = builder(y_top)
    text(label, (LABEL_RIGHT, y_top - h/2.0, 34.0), 54, LBLMAT)
    CURSOR[0] = y_top - h - ROW_GAP

BOARD_D = 540.0; PWR_D = 280.0; AID_D = 128.0; BOOK = 222.0

def board_row(y):
    """Both faces, side by side and the same size, so the layouts can be compared."""
    gap = 70.0; cy = y - BOARD_D/2.0 - 10.0
    xh = CONTENT_X0 + BOARD_D/2.0
    xp = CONTENT_X0 + BOARD_D + gap + BOARD_D/2.0
    disc_art("board_hex",  f"{LA}/27_HEX_54cm_01.png",  (xh, cy, 0.0), BOARD_D/2, h=6.0)
    disc_art("board_pent", f"{LA}/27_Pent_54cm_01.png", (xp, cy, 0.0), BOARD_D/2, h=6.0)
    text("HEX",   (xh, cy - BOARD_D/2.0 - 46, 30.0), 40, SUBMAT, align='CENTER')
    text("PENTA", (xp, cy - BOARD_D/2.0 - 46, 30.0), 40, SUBMAT, align='CENTER')
    return BOARD_D + 116.0
def rulebook_row(y):
    return row_single(y, lambda cx, cy: flat_card("rulebook", f"{ASSETS}/box_top.png",
                                                  (cx, cy, 0.0), BOOK, BOOK, t=6.0), BOOK, BOOK)
FOOD_N = 60 if MODE == "maximal" else 45
def food_row(y):  return row_food(y, FOOD_N)

TITLE = {"maximal": "ORGANISM", "minimal": "ORGANISM"}[MODE]
SUBTITLE = {"maximal": "MAXIMAL EDITION COMPONENTS",
            "minimal": "MINIMAL EDITION COMPONENTS"}[MODE]

if MODE == "maximal":
    emit("MAIN BOARD", board_row)
    emit("20 EATERS", lambda y: row_pieces("eat", y))
    emit("20 MOVERS", lambda y: row_pieces("move", y))
    emit("20 GROWERS", lambda y: row_pieces("grow", y))
    emit(f"{FOOD_N} FOOD", food_row)
    emit("45 PLATFORMS", lambda y: row_blocks(y, "plat", 9, COMPONENTS["PLAT"]["dia"], COMPONENTS["PLAT"]["h"], 3,
                              art=lambda pl: f"{LA}/plats/{pl}.png"))
    emit("15 POWER TOKENS", lambda y: row_blocks(y, "ptok", 3, COMPONENTS["PTOK"]["dia"], COMPONENTS["PTOK"]["h"], 3))
    nmut = [0]
    def mut_row(y):
        h, n = row_mutations(y); nmut[0] = n; return h
    emit("26 MUTATIONS", mut_row)
    emit("POWER BOARD", lambda y: row_single(
        y, lambda cx, cy: disc_art("powerboard", f"{ASSETS}/power_board.png", (cx, cy, 0.0), PWR_D/2, h=4.0), PWR_D, PWR_D))
    emit("5 PLAYER AIDS", lambda y: flow(
        y, [(AID_D+20, GROUP_GAP, (lambda i_: lambda cx, cy: disc_art(f"aid_{i_}", f"{ASSETS}/player_aid.png",
             (cx, cy, 0.0), AID_D/2, h=1.5))(i)) for i in range(5)], AID_D+34))
    emit("RULEBOOK", rulebook_row)
    emit("THE BOX", lambda y: row_single(
        y, lambda cx, cy: flat_card("box", f"{ASSETS}/box_top.png", (cx, cy, 0.0), 240, 240, t=72.0),
        240, 240))
else:
    emit("MAIN BOARD", board_row)
    emit("20 EAT DISKS", lambda y: row_pieces("eat",  y, minimal=True))
    emit("20 MOVE DISKS", lambda y: row_pieces("move", y, minimal=True))
    emit("20 GROW DISKS", lambda y: row_pieces("grow", y, minimal=True))
    emit(f"{FOOD_N} FOOD", food_row)
    emit("RULEBOOK", rulebook_row)

TOTAL_H = -(CURSOR[0]) + 60.0
text(TITLE,    (LABEL_RIGHT, -70,  40.0), 104, LBLMAT)
text(SUBTITLE, (LABEL_RIGHT, -156, 40.0), 34,  SUBMAT)

# ---------------- self-check ----------------
# The labels make a promise ("60 FOOD"). Count what actually got placed, so a
# layout change that silently drops pieces fails here instead of shipping.
import collections
placed = collections.Counter()
for ob in bpy.data.objects:
    for k in ("eat_", "move_", "grow_", "food_", "plat_", "ptok_", "mut_", "aid_"):
        if ob.name.startswith(k): placed[k.rstrip("_")] += 1
EXPECT = ({"eat":20, "move":20, "grow":20, "food":FOOD_N, "plat":45, "ptok":15, "mut":26, "aid":5}
          if MODE == "maximal" else {"eat":20, "move":20, "grow":20, "food":FOOD_N})
bad = {k: (placed.get(k, 0), v) for k, v in EXPECT.items() if placed.get(k, 0) != v}
tex_mb = sum(i.size[0]*i.size[1]*16/1048576.0 for i in bpy.data.images)
print(f"placed: {dict(sorted(placed.items()))}")
print(f"textures: {len(bpy.data.images)} images, {tex_mb:.0f} MB")
if os.environ.get("DEBUG_ROWS"):
    import collections as _c
    for pref in ("food_", "eat_", "mut_"):
        lines = _c.Counter()
        for ob in bpy.data.objects:
            if ob.name.startswith(pref): lines[round(ob.location.y)] += 1
        print(" ", pref, "per line:", [n for _, n in sorted(lines.items(), reverse=True)])
if bad:
    raise SystemExit(f"build_manifest: counts do not match the labels (got, want): {bad}")

# ---------------- camera: orthographic, so every row is at the same scale ----------------
MARGIN = 90.0
vis_w = (X_MAX - X_MIN) + 2*MARGIN
vis_h = TOTAL_H*math.cos(TILT) + 2*MARGIN + 160.0*math.sin(TILT)   # headroom only if tilted
cx = (X_MIN + X_MAX)/2.0; cy = -TOTAL_H/2.0

cam_d = bpy.data.cameras.new("Cam"); cam_d.type = 'ORTHO'
cam_d.ortho_scale = max(vis_w, vis_h); cam_d.clip_start = 1.0; cam_d.clip_end = 40000
cam = bpy.data.objects.new("Cam", cam_d); C().objects.link(cam); sc.camera = cam
cam.rotation_mode = 'QUATERNION'
tgt = Vector((cx, cy, 0.0)); d = 9000.0
loc = tgt + Vector((0.0, -d*math.sin(TILT), d*math.cos(TILT)))
cam.location = loc; cam.rotation_quaternion = (tgt - loc).to_track_quat('-Z', 'Y')

sc.render.resolution_x = RESX
sc.render.resolution_y = max(64, int(round(RESX * vis_h / vis_w)))
sc.render.image_settings.file_format = 'PNG'
sc.render.image_settings.compression = 100    # lossless; the default 15% leaves the files huge
sc.render.filepath = f"{FR}/manifest_{MODE}"
print(f"manifest {MODE}: {sc.render.resolution_x}x{sc.render.resolution_y}  "
      f"world {vis_w:.0f}x{vis_h:.0f}mm  rows to y={CURSOR[0]:.0f}")
bpy.ops.render.render(write_still=True)
print("done ->", sc.render.filepath + ".png")
