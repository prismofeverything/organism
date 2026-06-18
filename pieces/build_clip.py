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
RINGCOL  = tuple(float(x) for x in os.environ.get("RINGCOL", "1.0,0.85,0.45").split(","))  # ring emission (warm gold)
# golden "plasma pillar" highlight (replaces the flat ring when PLASMA=1): a transparent
# fire column on a cylinder, denser at the base, scrolling up + morphing, emissive -> blooms.
USE_PLASMA = os.environ.get("PLASMA", "1") != "0"
PL_R     = float(os.environ.get("PL_R", "21.0"))     # pillar radius (mm) — wider than the 16.5mm piece (no intersect)
PL_H     = float(os.environ.get("PL_H", "18.0"))     # pillar height (mm) — short: highlights the base, doesn't surround
PL_STR   = float(os.environ.get("PL_STR", "28.0"))   # emission strength (denser cells/edges keep a gentle gold glow)
PL_OPAC  = float(os.environ.get("PL_OPAC", "0.067")) # max opacity (~1/3 of prior: very transparent; piece reads through)
PL_SPEED = float(os.environ.get("PL_SPEED", "0.16")) # upward scroll per frame (vapor rise)
PL_MORPH = float(os.environ.get("PL_MORPH", "0.15")) # 4D shimmer per frame
PL_NSCALE= float(os.environ.get("PL_NSCALE", "0.18"))# noise scale (LOW -> a few big slow shapes, not fine fuzz)
PL_BASE  = float(os.environ.get("PL_BASE", "0.2"))   # base z (just above the board)
PL_LEAD  = float(os.environ.get("PL_LEAD", "10"))    # frames the plasma bell leads in / trails out around the move
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
    pb.inputs["Emission Color"].default_value = lin3(RINGCOL)   # warm gold highlight
    pb.inputs["Emission Strength"].default_value = 0.0
    o.material_slots[0].material = m
    o.scale = (0, 0, 0)
    return o, m

def vivid(rgb):
    """A saturated, mid-light version of a color so a tinted plasma blooms in that hue."""
    h, l, s = colorsys.rgb_to_hls(*rgb[:3])
    return colorsys.hls_to_rgb(h, 0.5, 1.0)

def plasma_mat(aid, basecol=None):
    """Transparent plasma fire on a cylinder: 4D noise that scrolls upward and
    morphs over time, masked into wisps, denser at the base, fading at the top. Emissive
    so the compositor bloom turns it into a soft shimmering glow."""
    m = bpy.data.materials.new("plasma_" + aid); m.use_nodes = True; nt = m.node_tree
    for n in list(nt.nodes): nt.nodes.remove(n)
    out = nt.nodes.new("ShaderNodeOutputMaterial"); tc = nt.nodes.new("ShaderNodeTexCoord")
    # height 0(base)->1(top) drives the vertical profile + breakup + color
    sep = nt.nodes.new("ShaderNodeSeparateXYZ"); nt.links.new(tc.outputs["Generated"], sep.inputs[0])
    # animated noise: object coords scrolled up (Location.Z) + 4D morph (W)
    mp = nt.nodes.new("ShaderNodeMapping"); mp.inputs["Scale"].default_value = (1.0, 1.0, 0.5)
    nt.links.new(tc.outputs["Object"], mp.inputs["Vector"])
    noise = nt.nodes.new("ShaderNodeTexNoise"); noise.noise_dimensions = '4D'
    noise.inputs["Scale"].default_value = PL_NSCALE
    noise.inputs["Detail"].default_value = 1.5; noise.inputs["Roughness"].default_value = 0.40
    nt.links.new(mp.outputs["Vector"], noise.inputs["Vector"])
    # KEYFRAME the scroll + morph (reliable in headless; drivers can silently no-op).
    # LINEAR (constant-speed shimmer); save/restore so the piece glides stay smooth Bezier.
    _pe = bpy.context.preferences.edit; _ki = _pe.keyframe_new_interpolation_type
    _pe.keyframe_new_interpolation_type = 'LINEAR'
    mp.inputs["Location"].default_value[2] = 0.0
    mp.inputs["Location"].keyframe_insert("default_value", index=2, frame=0)
    mp.inputs["Location"].default_value[2] = -LEN * PL_SPEED
    mp.inputs["Location"].keyframe_insert("default_value", index=2, frame=LEN)
    noise.inputs["W"].default_value = 0.0
    noise.inputs["W"].keyframe_insert("default_value", frame=0)
    noise.inputs["W"].default_value = LEN * PL_MORPH
    noise.inputs["W"].keyframe_insert("default_value", frame=LEN)
    _pe.keyframe_new_interpolation_type = _ki
    # vertical profile: a fat "pool" at the base tapering to a thin rising tail (vapor)
    prof = nt.nodes.new("ShaderNodeValToRGB"); pr = prof.color_ramp
    pr.elements[0].position = 0.0; pr.elements[0].color = (1, 1, 1, 1)        # base pool (dense)
    pr.elements[1].position = 1.0; pr.elements[1].color = (0.0, 0.0, 0.0, 1)  # top edge invisible (no cylinder cutoff)
    pmid = pr.elements.new(0.22); pmid.color = (0.7, 0.7, 0.7, 1)             # fat pool taper
    ptop = pr.elements.new(0.80); ptop.color = (0.0, 0.0, 0.0, 1)             # vapor faded out before the geometric top
    nt.links.new(sep.outputs["Z"], prof.inputs[0])
    # wispy flame mask from the noise
    flame = nt.nodes.new("ShaderNodeValToRGB")
    flame.color_ramp.elements[0].position = 0.30; flame.color_ramp.elements[0].color = (0, 0, 0, 1)
    flame.color_ramp.elements[1].position = 0.68; flame.color_ramp.elements[1].color = (1, 1, 1, 1)
    nt.links.new(noise.outputs["Fac"], flame.inputs[0])
    # density = vertical profile (pool low -> vapor high) * big-cell shapes; gaps stay fully clear
    pw = nt.nodes.new("ShaderNodeMath"); pw.operation = 'MULTIPLY'
    nt.links.new(prof.outputs["Color"], pw.inputs[0]); nt.links.new(flame.outputs["Color"], pw.inputs[1])
    dens = nt.nodes.new("ShaderNodeMath"); dens.operation = 'MULTIPLY'        # * max opacity (<1 -> piece shows through)
    nt.links.new(pw.outputs[0], dens.inputs[0]); dens.inputs[1].default_value = PL_OPAC
    # emission color by HEIGHT (hot base -> color -> deep tip). Gold by default; if basecol is
    # given (a player color) tint the whole gradient that hue so the bloom reads in that color.
    ecol = nt.nodes.new("ShaderNodeValToRGB"); cr = ecol.color_ramp
    if basecol is None:
        cr.elements[0].position = 0.0; cr.elements[0].color = lin3((1.0, 0.72, 0.22))   # low blue -> bloom stays gold
        mid = cr.elements.new(0.5); mid.color = lin3((1.0, 0.50, 0.12))
        cr.elements[2].position = 1.0; cr.elements[2].color = lin3((1.0, 0.34, 0.06))
    else:
        mix3 = lambda c, d, t: tuple(c[i] * (1 - t) + d[i] * t for i in range(3))
        cr.elements[0].position = 0.0; cr.elements[0].color = lin3(mix3(basecol, (1, 1, 1), 0.30))  # hot core
        mid = cr.elements.new(0.5); mid.color = lin3(basecol)
        cr.elements[2].position = 1.0; cr.elements[2].color = lin3(mix3(basecol, (0, 0, 0), 0.35))  # deep tip
    nt.links.new(sep.outputs["Z"], ecol.inputs[0])
    emi = nt.nodes.new("ShaderNodeEmission"); emi.inputs["Strength"].default_value = PL_STR
    nt.links.new(ecol.outputs["Color"], emi.inputs["Color"])
    tr = nt.nodes.new("ShaderNodeBsdfTransparent"); mix = nt.nodes.new("ShaderNodeMixShader")
    nt.links.new(dens.outputs[0], mix.inputs[0])        # density -> opacity
    nt.links.new(tr.outputs[0], mix.inputs[1]); nt.links.new(emi.outputs[0], mix.inputs[2])
    nt.links.new(mix.outputs[0], out.inputs["Surface"])
    for attr, val in [("surface_render_method", "BLENDED"), ("blend_method", "BLEND"),
                      ("use_backface_culling", False), ("use_transparent_shadow", False),
                      ("shadow_method", "NONE")]:
        try: setattr(m, attr, val)
        except Exception: pass
    return m

def plasma_obj(aid, basecol=None):
    bpy.ops.mesh.primitive_cylinder_add(radius=PL_R, depth=PL_H, end_fill_type='NOTHING', location=(9000, 9000, 0))
    o = bpy.context.active_object; o.name = "plasma_" + aid
    for v in o.data.vertices: v.co.z += PL_H / 2.0     # base at object origin -> z-scale rises from the board
    o.data.update()
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    o.data.materials.clear(); o.data.materials.append(plasma_mat(aid, basecol))
    o.scale = (1.0, 1.0, 0.0)
    return o

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
MOVES = {m["id"]: m for m in S.get("moves", [])}   # per-element glides on their OWN timeline (overlap/stagger)
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
    if (not USE_PLASMA) and any(aid in b.get("glow", []) for b in beats):
        ring, rmat = glow_ring(aid)
    mv = MOVES.get(aid)                            # if set, position comes from an explicit glide
    for b in beats:
        if aid in b["pos"]:
            x, y = P2(b["pos"][aid]); s = b.get("scale", {}).get(aid, PSCALE)   # per-beat scale (e.g. a capture morph-out)
            if mv: kf(o, b["t"], s=s)              # mover: scale here, position from the glide below
            else:  kf(o, b["t"], (x, y, ELEV), s)
            if ring:
                kf(ring, b["t"], (x, y, 1.25), 1.0)
                e = rmat.node_tree.nodes["Principled BSDF"].inputs["Emission Strength"]
                e.default_value = RING_STR if aid in b.get("glow", []) else 0.0
                e.keyframe_insert("default_value", frame=b["t"])
        else:
            kf(o, b["t"], s=0.0)
            if ring: kf(ring, b["t"], s=0.0)
    if mv:                                          # explicit glide on its OWN timeline (staggered/overlapping moves)
        fx, fy = P2(mv["from"]); tx, ty = P2(mv["to"])
        kf(o, 0, (fx, fy, ELEV)); kf(o, mv["t0"], (fx, fy, ELEV))
        kf(o, mv["t1"], (tx, ty, ELEV)); kf(o, LEN, (tx, ty, ELEV))

# plasma pillars (the "energy" rising from the board under a glowing element). Position follows
# the element. Scale = one smooth Bezier bell PER glow EPISODE (a contiguous run of glow beats),
# so an element can be highlighted more than once (e.g. a move-bell then a conflict-bell). Each
# bell brackets the element's move within that episode (peak at move midpoint) or its glow span.
# With "glow_player_color" the plasma is tinted each element's player color instead of gold.
if USE_PLASMA:
    gpc = S.get("glow_player_color", False)
    ts = sorted(b["t"] for b in beats)
    for aid in ({a for b in beats for a in b.get("glow", [])} | set(MOVES)):
        basecol = vivid(COLORS[S["actors"][aid][0]]) if gpc else None
        po = plasma_obj(aid, basecol)
        mv = MOVES.get(aid)
        if mv:                                     # plasma follows the glide; one bell over [t0,t1]
            fx, fy = P2(mv["from"]); tx, ty = P2(mv["to"])
            po.location = (fx, fy, PL_BASE); po.keyframe_insert("location", frame=0)
            po.keyframe_insert("location", frame=mv["t0"])
            po.location = (tx, ty, PL_BASE); po.keyframe_insert("location", frame=mv["t1"])
            po.keyframe_insert("location", frame=LEN)
            po.scale = (1.0, 1.0, 0.0); po.keyframe_insert("scale", frame=0)
            peak = (mv["t0"] + mv["t1"]) / 2.0
            for t, s in [(max(0, mv["t0"] - PL_LEAD), 0.0), (peak, 1.0), (min(LEN, mv["t1"] + PL_LEAD), 0.0)]:
                po.scale = (1.0, 1.0, s); po.keyframe_insert("scale", frame=int(round(t)))
            continue
        pos_at = {b["t"]: b["pos"].get(aid) for b in beats}
        for b in beats:                            # position follows the element
            if aid in b["pos"]:
                x, y = P2(b["pos"][aid]); po.location = (x, y, PL_BASE)
                po.keyframe_insert("location", frame=b["t"])
        on = {b["t"]: (aid in b.get("glow", [])) for b in beats}
        episodes = []; i = 0                        # contiguous runs of glow beats
        while i < len(ts):
            if on[ts[i]]:
                j = i
                while j + 1 < len(ts) and on[ts[j + 1]]: j += 1
                episodes.append((ts[i], ts[j])); i = j + 1
            else: i += 1
        po.scale = (1.0, 1.0, 0.0); po.keyframe_insert("scale", frame=0)   # start collapsed
        for (e0, e1) in episodes:
            inner = [t for t in ts if e0 <= t <= e1]
            chg = [(inner[k - 1], inner[k]) for k in range(1, len(inner)) if pos_at[inner[k]] != pos_at[inner[k - 1]]]
            m0, m1 = (min(a for a, _ in chg), max(b2 for _, b2 in chg)) if chg else (e0, e1)
            peak = (m0 + m1) / 2.0
            for t, s in [(max(0, m0 - PL_LEAD), 0.0), (peak, 1.0), (min(LEN, m1 + PL_LEAD), 0.0)]:
                po.scale = (1.0, 1.0, s); po.keyframe_insert("scale", frame=int(round(t)))

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

# gliding / hopping food tokens (eat / circulate). A key is (t, space) or (t, space, dz),
# where dz lifts the token above its resting height (a hop arc apex). "grow" = scale-in
# frames (default 6); used so a newly-created food materializes over a curve, not a pop.
for fa in S.get("food_actors", []):
    fo, _ = inst(FOODT, "fa_" + fa["id"], FOOD_RGB, rough=0.5)
    keys = fa["keys"]; t0 = keys[0][0]; koff = fa.get("k", 0) * FOOD_DZ   # stack offset at endpoints
    grow = fa.get("grow", 6)
    fxy = lambda sp: P2(sp) if isinstance(sp, str) else tuple(sp)         # board space id OR raw (x,y) apex
    fz  = lambda sp, t: food_z(sp, t) if isinstance(sp, str) else BOARD_FOOD_Z
    if t0 > 0:                              # scale in (grow) just before it appears / starts moving
        sp0 = keys[0][1]; x, y = fxy(sp0); kf(fo, max(0, t0 - grow), (x, y, fz(sp0, t0) + koff), 0.0)
    for key in keys:
        t, sp = key[0], key[1]; dz = key[2] if len(key) > 2 else 0.0
        x, y = fxy(sp); kf(fo, t, (x, y, fz(sp, t) + koff + dz), FSCALE)

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
    socks = {"Type": 'Bloom', "Highlights Threshold": float(os.environ.get("BLOOMTHRESH", "2.5")),
             "Strength": float(os.environ.get("BLOOMSTR", "2.0")), "Size": float(os.environ.get("BLOOMSIZE", "0.9"))}
    for name, val in socks.items():
        if name in gl.inputs:
            try: gl.inputs[name].default_value = val
            except Exception: pass
    nt.links.new(src.outputs["Image"], gl.inputs["Image"])
    nt.links.new(gl.outputs["Image"], out.inputs[0])
    si = gl.inputs["Size"] if "Size" in gl.inputs else None
    sinfo = f" size={si.default_value} range=[{getattr(si,'min_value','?')},{getattr(si,'max_value','?')}]" if si else ""
    return "ok type=" + str(gl.inputs["Type"].default_value if "Type" in gl.inputs else "?") + sinfo
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
