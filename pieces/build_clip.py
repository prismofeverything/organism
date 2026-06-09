"""Short, legible gameplay clips for ORGANISM — one mechanic per clip.

A clip is a small hand-authored STORYBOARD (states the board passes through). The
engine reuses the canonical render conventions from build_play_real.py (board
SCALE=43, BROT=30deg lattice, Z-up piece grafts, food stacks) and adds:
  - glide transitions between beats (Bezier-eased), appear/disappear by scale
  - carried food stacks + free-standing / gliding food tokens
  - element glow (animated emission + compositor bloom)
  - a slow, gently-drifting camera at a per-clip angle ("moving, but barely")

Run one clip (frames -> renders/clips/<CLIP>/, then ffmpeg to mp4):
  CLIP=move ~/Downloads/blender-5.1.1-linux-x64/blender -b --python build_clip.py
See make_clips.sh / `make clips` for the batch + ffmpeg wrapper.
"""
import bpy, sys, os, math, re, colorsys
from mathutils import Vector

P = os.path.dirname(os.path.abspath(__file__)); ROOT = os.path.dirname(P)
sys.path.insert(0, P)
import organism_format as ogf

# ---------------- config ----------------
CLIP    = os.environ.get("CLIP", "move")
FR      = os.environ.get("FR", f"{P}/renders/clips/{CLIP}"); os.makedirs(FR, exist_ok=True)
RES     = int(os.environ.get("RES", "1080"))
SAMPLES = int(os.environ.get("SAMPLES", "48"))
FPS     = int(os.environ.get("FPS", "24"))
ASSETS  = f"{P}/clip_assets"
BOARD   = os.environ.get("BOARD", f"{ASSETS}/board_hex.png")

SCALE = 43.0; BROT = math.radians(30)
PSCALE = 0.9; FSCALE = 0.94; FOOD_DZ = 6.4; ELEV = 1.0   # piece base z above board
BOARD_FOOD_Z = 1.4                                       # free food resting on the board

G = ogf.load_ogf(f"{ROOT}/ogf/zach-dan-ryan.json"); LOC = ogf.board_locations(G)
def P2(sp):
    x, y = LOC[sp]
    return (SCALE*(x*math.cos(BROT)-y*math.sin(BROT)), SCALE*(x*math.sin(BROT)+y*math.cos(BROT)))

# ---------------- color ----------------
def hsl_rgb(s):
    h, sa, li = re.match(r"hsl\((\d+),(\d+)%?,(\d+)%?\)", s).groups()
    return colorsys.hls_to_rgb(float(h)/360, float(li)/100, float(sa)/100)
def _l(c): return c/12.92 if c <= 0.04045 else ((c+0.055)/1.055)**2.4
def lin3(rgb): return (_l(rgb[0]), _l(rgb[1]), _l(rgb[2]), 1.0)

PLAYER = {
    "green":  hsl_rgb("hsl(118,45%,68%)"),
    "red":    hsl_rgb("hsl(353,62%,60%)"),
    "purple": hsl_rgb("hsl(266,55%,60%)"),
    "blue":   hsl_rgb("hsl(196,45%,58%)"),
    "yellow": hsl_rgb("hsl(45,78%,62%)"),
    "dark":   hsl_rgb("hsl(220,14%,36%)"),
}
FOOD_RGB = (242/255, 230/255, 158/255)
COLORS = dict(PLAYER)   # color-name -> rgb; real-game player names get added by expand_ogf

# ---------------- scene ----------------
bpy.ops.wm.read_factory_settings(use_empty=True); sc = bpy.context.scene
try: sc.render.engine = "BLENDER_EEVEE_NEXT"
except Exception: sc.render.engine = "BLENDER_EEVEE"
try: sc.eevee.taa_render_samples = SAMPLES
except Exception: pass
sc.render.resolution_x = sc.render.resolution_y = RES; sc.render.fps = FPS
try: sc.view_settings.view_transform = 'Standard'   # true-to-print colors (not AgX milky wash)
except Exception: pass
print("view_transform =", sc.view_settings.view_transform)
w = bpy.data.worlds.new("W"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (0.18, 0.19, 0.22, 1.0)
w.node_tree.nodes["Background"].inputs[1].default_value = float(os.environ.get("WORLDSTR", "1.0"))

def C(): return bpy.context.collection
def colmat(name, rgb, rough=0.45):
    m = bpy.data.materials.new(name); m.use_nodes = True
    b = m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value = lin3(rgb)
    b.inputs["Roughness"].default_value = rough
    return m
def imgmat(name, path, rough=0.9, shadeless=False):
    """Board art. shadeless=True drives the image through Emission so the printed
    colors render faithfully (no lighting wash), with a little Base Color kept so it
    still receives soft piece shadows."""
    m = bpy.data.materials.new(name); m.use_nodes = True; nt = m.node_tree
    b = nt.nodes["Principled BSDF"]; t = nt.nodes.new("ShaderNodeTexImage")
    t.image = bpy.data.images.load(path)
    if shadeless:
        # fully emissive board: renders the printed art faithfully, independent of the
        # scene lights (so the pieces can be lit hard without washing out the board).
        # A saturation/value boost counteracts the desaturation that mipmap minification
        # introduces when the high-contrast board is viewed at an oblique angle.
        hsv = nt.nodes.new("ShaderNodeHueSaturation")
        hsv.inputs["Saturation"].default_value = float(os.environ.get("BOARDSAT", "1.55"))
        hsv.inputs["Value"].default_value = float(os.environ.get("BOARDVAL", "1.12"))
        nt.links.new(t.outputs["Color"], hsv.inputs["Color"])
        nt.links.new(hsv.outputs["Color"], b.inputs["Emission Color"])
        b.inputs["Emission Strength"].default_value = 1.0
        b.inputs["Base Color"].default_value = (0, 0, 0, 1)
        b.inputs["Roughness"].default_value = 1.0
    else:
        nt.links.new(t.outputs["Color"], b.inputs["Base Color"])
        b.inputs["Roughness"].default_value = rough
    return m
def plane(name, sz, mat, loc):
    me = bpy.data.meshes.new(name); o = bpy.data.objects.new(name, me); C().objects.link(o)
    s = sz/2
    me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)], [], [(0,1,2,3)])
    uv = me.uv_layers.new()
    for lp in me.loops: uv.data[lp.index].uv = [(0,0),(1,0),(1,1),(0,1)][lp.vertex_index]
    me.update(); me.materials.append(mat); o.location = loc; return o

for nm, en, rot in [("S", 3.6, (math.radians(52), math.radians(8), math.radians(30))),
                    ("S2", 1.3, (math.radians(64), 0, math.radians(-120)))]:
    d = bpy.data.lights.new(nm, 'SUN'); d.energy = en * float(os.environ.get("SUNE", "1.0")); d.angle = math.radians(3)
    o = bpy.data.objects.new(nm, d); C().objects.link(o); o.rotation_euler = rot

plane("Table", 2400, colmat("table", (0.30, 0.31, 0.34), 0.95), (0, 0, -2))
plane("Board", 540, imgmat("Board", BOARD, shadeless=True), (0, 0, 0))

# ---------------- assets ----------------
def imp(path, name, zup=True):
    if zup: bpy.ops.wm.obj_import(filepath=path, up_axis='Z', forward_axis='Y')
    else:   bpy.ops.wm.obj_import(filepath=path, forward_axis='NEGATIVE_Z', up_axis='Y')
    o = [x for x in bpy.context.selected_objects if x.type == 'MESH'][0]; o.name = name
    o.data.materials.clear(); o.data.materials.append(colmat(name + "_base", (0.7, 0.7, 0.7)))  # 1 slot
    try:
        bpy.ops.object.select_all(action='DESELECT'); o.select_set(True)
        bpy.context.view_layer.objects.active = o; bpy.ops.object.shade_smooth()
    except Exception: pass
    o.hide_render = True; o.location = (9000, 9000, 0); return o

def piece_path(name):
    s = f"{P}/out/{name}_sculpt_graft.obj"; pa = f"{P}/out/{name}_graft.obj"
    return s if os.path.exists(s) else pa
def food_path():
    for p in [f"{P}/renders/food/FOOD_slip.obj", f"{P}/renders/food/FOOD_nosnap.obj",
              f"{P}/renders/food/FOOD_snap.obj"]:
        if os.path.exists(p): return p
    raise SystemExit("no FOOD obj")

TPL  = {t: imp(piece_path(m), "TPL_"+t) for t, m in [("eat", "EAT"), ("move", "MOVE"), ("grow", "GROW")]}
FOODT = imp(food_path(), "TPL_food", zup=False)
def topz(o): return max((o.matrix_world @ v.co).z for v in o.data.vertices)
PEG = {t: (topz(TPL[t]) - 4.3) * PSCALE for t in TPL}   # seat height above element base

RING_STR = float(os.environ.get("RING", "8.0"))
bpy.ops.mesh.primitive_torus_add(major_radius=17.5, minor_radius=1.3, location=(9000, 9000, 0))
RING_T = bpy.context.active_object; RING_T.name = "TPL_ring"
try: bpy.ops.object.shade_smooth()
except Exception: pass
RING_T.data.materials.append(colmat("ring_base", (1, 1, 1))); RING_T.hide_render = True

def glow_ring(aid):
    o = RING_T.copy(); C().objects.link(o); o.hide_render = False; o.name = "ring_" + aid
    o.material_slots[0].link = 'OBJECT'
    m = bpy.data.materials.new("ringm_" + aid); m.use_nodes = True
    pb = m.node_tree.nodes["Principled BSDF"]
    pb.inputs["Emission Color"].default_value = lin3((1.0, 0.93, 0.55))   # warm gold highlight
    pb.inputs["Emission Strength"].default_value = 0.0
    o.material_slots[0].material = m
    o.scale = (0, 0, 0)
    return o, m

def inst(tpl, name, rgb, rough=0.45):
    """A render copy with its OWN (object-linked) material so it can recolor/glow
    without touching the shared template mesh."""
    o = tpl.copy(); C().objects.link(o); o.hide_render = False; o.name = name
    if o.material_slots:
        o.material_slots[0].link = 'OBJECT'
        o.material_slots[0].material = colmat(name + "_m", rgb, rough)
    o.scale = (0, 0, 0)
    return o, (o.material_slots[0].material if o.material_slots else None)

def kf(o, fr, loc=None, s=None):
    if loc is not None: o.location = loc; o.keyframe_insert("location", frame=fr)
    if s is not None:   o.scale = (s, s, s); o.keyframe_insert("scale", frame=fr)

# ---------------- storyboards ----------------
# beat: {"t": frame, "pos": {actor_id: space}, "food": {actor_id: n}, "glow": [actor_id]}
# food_actors: [{"id":..., "keys":[(t, space)]}]  (a token that glides; lands on a piece
#              if one occupies that space at that beat, else rests on the board)
from storyboards import STORY     # noqa: E402  (data lives next door for readability)

def expand_ogf(spec):
    """Turn a {"from_ogf": turn} spec into a normal static storyboard built from a real
    frame of the recorded game — used for the full-board and element-glow shots."""
    fr = G["frames"][spec["from_ogf"]]
    for player, hsl in G["colors"].items(): COLORS[player] = hsl_rgb(hsl)
    actors, pos, food = {}, {}, {}
    for i, (player, etype, space, fd) in enumerate(fr["elements"]):
        aid = f"o{i}"; actors[aid] = (player, etype); pos[aid] = space
        if fd > 0: food[aid] = fd
    glow = [aid for aid, sp in pos.items() if sp == spec.get("glow_space")]
    free = {sp: amt for sp, amt in fr.get("food", {}).items() if amt > 0}
    L = spec["len"]
    beats = [{"t": 0, "pos": pos, "food": food, "free": free, "glow": glow},
             {"t": L, "pos": pos, "food": food, "free": free, "glow": glow}]
    out = dict(spec); out["actors"] = actors; out["beats"] = beats
    if spec.get("glow_space"):                       # frame the highlighted element
        gx, gy = P2(spec["glow_space"]); cam = dict(spec.get("cam", {}))
        cam["tx"], cam["ty"] = gx, gy; out["cam"] = cam
    return out

S = STORY[CLIP]
if "from_ogf" in S: S = expand_ogf(S)
beats = S["beats"]; LEN = S["len"]
sc.frame_start = 0; sc.frame_end = LEN
if os.environ.get("MAXF"): sc.frame_end = min(LEN, int(os.environ["MAXF"]))   # quick test: first N frames

def beat_at(t):
    return min(beats, key=lambda b: abs(b["t"] - t))
def food_z(space, t):
    """Resting height for a gliding food token at `space` at time `t`: on top of a
    piece's stack if one is there, else on the board."""
    b = beat_at(t); occ = {sp: aid for aid, sp in b["pos"].items()}
    aid = occ.get(space)
    if aid is not None:
        ptype = S["actors"][aid][1]; c = b.get("food", {}).get(aid, 0)
        return ELEV + PEG[ptype] + c * FOOD_DZ
    return BOARD_FOOD_Z

# pieces + glow ring (a bright ring on the board around the highlighted element; bloom
# turns it into a soft halo — keeps the piece's own color instead of whiting it out)
for aid, (player, ptype) in S["actors"].items():
    o, _ = inst(TPL[ptype], "pc_" + aid, COLORS[player])
    ring = rmat = None
    if any(aid in b.get("glow", []) for b in beats):
        ring, rmat = glow_ring(aid)
    for b in beats:
        if aid in b["pos"]:
            x, y = P2(b["pos"][aid]); kf(o, b["t"], (x, y, ELEV), PSCALE)
            if ring:
                kf(ring, b["t"], (x, y, 1.25), 1.0)
                e = rmat.node_tree.nodes["Principled BSDF"].inputs["Emission Strength"]
                e.default_value = RING_STR if aid in b.get("glow", []) else 0.0
                e.keyframe_insert("default_value", frame=b["t"])
        else:
            kf(o, b["t"], s=0.0)
            if ring: kf(ring, b["t"], s=0.0)

# carried food stacks
for aid, (player, ptype) in S["actors"].items():
    maxc = max([b.get("food", {}).get(aid, 0) for b in beats] + [0])
    for k in range(maxc):
        fo, _ = inst(FOODT, f"food_{aid}_{k}", FOOD_RGB, rough=0.5)
        for b in beats:
            if aid in b["pos"]:
                x, y = P2(b["pos"][aid])
                vis = b.get("food", {}).get(aid, 0) > k
                kf(fo, b["t"], (x, y, ELEV + PEG[ptype] + k * FOOD_DZ), FSCALE if vis else 0.0)
            else:
                kf(fo, b["t"], s=0.0)

# free-standing food on the board (per beat) — stacked into a pile of `count` tokens
free_spaces = sorted({sp for b in beats for sp in b.get("free", {})})
for si, sp in enumerate(free_spaces):
    x, y = P2(sp)
    maxc = max([b.get("free", {}).get(sp, 0) for b in beats] + [0])
    for k in range(maxc):
        fo, _ = inst(FOODT, f"free_{si}_{k}", FOOD_RGB, rough=0.5)
        for b in beats:
            vis = b.get("free", {}).get(sp, 0) > k
            kf(fo, b["t"], (x, y, BOARD_FOOD_Z + k * FOOD_DZ), FSCALE if vis else 0.0)

# gliding food tokens (eat / circulate)
for fa in S.get("food_actors", []):
    fo, _ = inst(FOODT, "fa_" + fa["id"], FOOD_RGB, rough=0.5)
    keys = fa["keys"]; t0 = keys[0][0]; koff = fa.get("k", 0) * FOOD_DZ   # stack offset at endpoints
    if t0 > 0:                              # pop in just before it starts moving
        x, y = P2(keys[0][1]); kf(fo, max(0, t0 - 6), (x, y, food_z(keys[0][1], t0) + koff), 0.0)
    for (t, sp) in keys:
        x, y = P2(sp); kf(fo, t, (x, y, food_z(sp, t) + koff), FSCALE)

# ---------------- compositor bloom (makes emission read as glow in EEVEE Next) ----------------
def add_bloom():
    sc.use_nodes = True
    nt = getattr(sc, "node_tree", None)                       # Blender <=4.x
    if nt is None and hasattr(sc, "compositing_node_group"):  # Blender 5.x: a node-group datablock
        nt = sc.compositing_node_group
        if nt is None:
            nt = bpy.data.node_groups.new("Comp", "CompositorNodeTree")
            sc.compositing_node_group = nt
    if nt is None: return "no compositor api"
    for n in list(nt.nodes): nt.nodes.remove(n)
    src = nt.nodes.new("CompositorNodeRLayers")
    try: out = nt.nodes.new("CompositorNodeComposite")        # classic output
    except Exception: out = nt.nodes.new("NodeGroupOutput")   # node-group output
    gl = nt.nodes.new("CompositorNodeGlare")
    # 5.1: glare params are input sockets. High threshold so only the bright glow rings
    # bloom (strength ~8), not the board emission (<=~1.3).
    socks = {"Type": 'Bloom', "Highlights Threshold": float(os.environ.get("BLOOMTHRESH", "1.6")),
             "Strength": 0.8, "Size": 0.45}
    for name, val in socks.items():
        if name in gl.inputs:
            try: gl.inputs[name].default_value = val
            except Exception: pass
    nt.links.new(src.outputs["Image"], gl.inputs["Image"])
    nt.links.new(gl.outputs["Image"], out.inputs[0])
    return "ok type=" + str(gl.inputs["Type"].default_value if "Type" in gl.inputs else "?")
if os.environ.get("BLOOM", "1") != "0":
    try: print("bloom:", add_bloom())
    except Exception as e: print("bloom skipped:", e)
else:
    print("bloom: disabled")

# ---------------- slow drifting camera ----------------
ca = S["cam"]; drift = ca.get("drift", {})
cam_d = bpy.data.cameras.new("Cam"); cam_d.lens = ca.get("lens", 52); cam_d.clip_end = 20000
cam = bpy.data.objects.new("Cam", cam_d); C().objects.link(cam); sc.camera = cam
cam.rotation_mode = 'QUATERNION'
def smooth(u): return u * u * (3 - 2 * u)
def cam_at(u):
    az = math.radians(ca["az"] + smooth(u) * drift.get("az", 0))
    el = math.radians(ca["el"] + smooth(u) * drift.get("el", 0))
    dist = ca["dist"] + smooth(u) * drift.get("dist", 0)
    tz = ca.get("targz", 6) + smooth(u) * drift.get("targz", 0)
    loc = Vector((dist*math.cos(el)*math.cos(az), dist*math.cos(el)*math.sin(az), tz + dist*math.sin(el)))
    target = Vector((ca.get("tx", 0), ca.get("ty", 0), tz))
    return loc, (target - loc).to_track_quat('-Z', 'Y')
prev_q = None
for f in range(LEN + 1):
    loc, q = cam_at(f / LEN)
    if prev_q is not None and prev_q.dot(q) < 0: q = -q
    prev_q = q
    cam.location = loc; cam.rotation_quaternion = q
    cam.keyframe_insert("location", frame=f); cam.keyframe_insert("rotation_quaternion", frame=f)

sc.render.image_settings.file_format = 'PNG'
single = os.environ.get("FRAME")
if single:                                   # fast iteration: one still
    f = int(single); sc.frame_set(f)
    sc.render.filepath = f"{FR}/single_{f:04d}"
    print(f"CLIP={CLIP} single frame {f} -> {FR}")
    bpy.ops.render.render(write_still=True)
else:
    sc.render.filepath = f"{FR}/f"
    print(f"CLIP={CLIP} actors={len(S['actors'])} beats={len(beats)} frames={LEN+1} -> {FR}")
    bpy.ops.render.render(animation=True)
print("done")
