"""#4 POWER BOARD (score track) clip -- SUM-OF-TOKENS model.

Each player holds a STACK OF THREE power tokens; a player's score = the SUM of its
tokens' ring positions (rings 0..5 up the nested-arc pyramid, 0 wide purple base ->
5 gold apex). Tokens advance ONE AT A TIME, each in its own STEP.

This clip opens mid-game -- green has one token up on ring 3; orange & purple each have
one on ring 4 -- and plays four staggered advances:
  1. orange  4 -> 5     (orange climbs to the LEFT of the gold apex -> orange = 5)
  2. purple  4 -> 5     (purple climbs to the RIGHT of the apex; the two FLANK it, both visible)
  3. green   3 -> 4     (green advances its ring-3 token into the vacated pink node -> green = 4)
  4. purple  0 -> 1     (purple peels a token off its base stack -> purple = 6)   <- climax
Final score: green 4, orange 5, purple 6.  An ascending trio; orange & purple flank the apex.

Every token sits OFF its ring's centre node (phi != 0) so the printed VALUE-DOTS there stay
visible; a player's idle reserve tokens are PILED into a leaned per-player coin-stack on the
base ring. The orange|purple pair straddles its shared node (one each side) on rings 4 and 5.
Every MOVING token carries the player-tinted PLASMA highlight: one smooth Bezier "bell"
per move, bracketing it (peak at the move midpoint, PL_LEAD frames of lead-in/out), so the
staggered moves read as overlapping curves of different lengths -- the same highlight
language as the gameplay clips (ported from build_clip.py).

  CLIP frames -> renders/clips/power/ ; CHECK=1 drops a marker on every ring (alignment).
  FRAME=<n> renders a single still to renders/clips/power/single_<n>.png while tuning.
"""
import bpy, os, math, colorsys, re
from mathutils import Vector

P = os.path.dirname(os.path.abspath(__file__))
ASSETS = f"{P}/clip_assets"
FR = os.environ.get("FR", f"{P}/renders/clips/power"); os.makedirs(FR, exist_ok=True)
RES = int(os.environ.get("RES", "1080")); SAMPLES = int(os.environ.get("SAMPLES", "24"))
FPS = int(os.environ.get("FPS", "24")); LEN = int(os.environ.get("LEN", "185"))

def _l(c): return c/12.92 if c <= 0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]), _l(c[1]), _l(c[2]), 1.0)
def hsl(s):
    h, sa, li = re.match(r"hsl\((\d+),(\d+)%?,(\d+)%?\)", s).groups()
    return colorsys.hls_to_rgb(float(h)/360, float(li)/100, float(sa)/100)
# orange replaces red as the third player; matches the board's golden-orange band
PCOL = {"green": hsl("hsl(118,48%,55%)"), "orange": hsl("hsl(30,92%,55%)"), "purple": hsl("hsl(266,60%,60%)")}
def vivid(rgb):
    """A saturated, mid-light version of a color so a tinted plasma blooms in that hue."""
    h, l, s = colorsys.rgb_to_hls(*rgb[:3])
    return colorsys.hls_to_rgb(h, 0.5, 1.0)

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
# 3-light rig so tokens read as 3D coins (top gradient + a grazing light that rakes the side/rim),
# not flat colour circles. KEY upper-front-right, FILL opposite-soft, GRAZE low to catch the rims.
for nm, en, rot in [("KEY",   4.6, (math.radians(48), 0, math.radians(38))),
                    ("FILL",  1.4, (math.radians(62), 0, math.radians(-120))),
                    ("GRAZE", 2.3, (math.radians(15), 0, math.radians(-32)))]:
    d = bpy.data.lights.new(nm, 'SUN'); d.energy = en; d.angle = math.radians(5)
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

# ---- ring geometry: the score rings are CONCENTRIC ARCS about a centre near the board centre
# (fitted from the art). Ring v is a band at radius RING_R[v]; rings 0/1/2 sit BELOW that centre,
# 3/4/5 ABOVE. A token is placed by ANGLE phi (deg) along its ring's arc (phi=0 = the centreline
# node), so several tokens spread along the whole length of the band instead of piling at the node.
OARC = (0.0, -9.5)                                    # arc centre (world mm)
RING_R   = {0: 134.5, 1: 81.8, 2: 25.2, 3: 32.4, 4: 90.5, 5: 146.3}   # band radius per ring (mm)
RING_DOWN= {0: True, 1: True, 2: True, 3: False, 4: False, 5: False}  # node below(True)/above(False) the centre
DISC_Z = 4.0; DISC_R = 13.0                           # coin radius + resting centre-height
def slot(v, phi=0.0):                                 # world pos of a token on ring v at arc-angle phi (deg)
    th = math.radians(phi); R = RING_R[v]
    y = OARC[1] + (-R*math.cos(th) if RING_DOWN[v] else R*math.cos(th))
    return (R*math.sin(th), y, DISC_Z)

# ---------------- plasma pillar highlight (ported from build_clip.py) ----------------
# transparent player-tinted "fire" on a short cylinder: 4D noise scrolling up + morphing,
# denser at the base, emissive -> the compositor bloom turns it into a soft shimmer.
PL_R     = float(os.environ.get("PL_R", "17.0"))     # pillar radius (mm) -- wider than the disc (no intersect)
PL_H     = float(os.environ.get("PL_H", "15.0"))     # pillar height (mm) -- short: highlights the base
PL_STR   = float(os.environ.get("PL_STR", "22.0"))   # emission strength
PL_OPAC  = float(os.environ.get("PL_OPAC", "0.09"))  # max opacity (very transparent; token reads through)
PL_SPEED = float(os.environ.get("PL_SPEED", "0.16")) # upward scroll per frame (vapor rise)
PL_MORPH = float(os.environ.get("PL_MORPH", "0.15")) # 4D shimmer per frame
PL_NSCALE= float(os.environ.get("PL_NSCALE", "0.20"))# noise scale (LOW -> a few big slow shapes)
PL_BASE  = float(os.environ.get("PL_BASE", "0.3"))   # base z (just above the board)
PL_LEAD  = float(os.environ.get("PL_LEAD", "9"))     # frames the bell leads in / trails out around the move
USE_PLASMA = os.environ.get("PLASMA", "1") != "0"

def plasma_mat(aid, basecol):
    m = bpy.data.materials.new("plasma_" + aid); m.use_nodes = True; nt = m.node_tree
    for n in list(nt.nodes): nt.nodes.remove(n)
    out = nt.nodes.new("ShaderNodeOutputMaterial"); tc = nt.nodes.new("ShaderNodeTexCoord")
    sep = nt.nodes.new("ShaderNodeSeparateXYZ"); nt.links.new(tc.outputs["Generated"], sep.inputs[0])
    mp = nt.nodes.new("ShaderNodeMapping"); mp.inputs["Scale"].default_value = (1.0, 1.0, 0.5)
    nt.links.new(tc.outputs["Object"], mp.inputs["Vector"])
    noise = nt.nodes.new("ShaderNodeTexNoise"); noise.noise_dimensions = '4D'
    noise.inputs["Scale"].default_value = PL_NSCALE
    noise.inputs["Detail"].default_value = 1.5; noise.inputs["Roughness"].default_value = 0.40
    nt.links.new(mp.outputs["Vector"], noise.inputs["Vector"])
    # KEYFRAME the scroll + morph as LINEAR (drivers can silently no-op headless); save/restore.
    _pe = bpy.context.preferences.edit; _ki = _pe.keyframe_new_interpolation_type
    _pe.keyframe_new_interpolation_type = 'LINEAR'
    mp.inputs["Location"].default_value[2] = 0.0
    mp.inputs["Location"].keyframe_insert("default_value", index=2, frame=0)
    mp.inputs["Location"].default_value[2] = -LEN * PL_SPEED
    mp.inputs["Location"].keyframe_insert("default_value", index=2, frame=LEN)
    noise.inputs["W"].default_value = 0.0; noise.inputs["W"].keyframe_insert("default_value", frame=0)
    noise.inputs["W"].default_value = LEN * PL_MORPH; noise.inputs["W"].keyframe_insert("default_value", frame=LEN)
    _pe.keyframe_new_interpolation_type = _ki
    # vertical profile: fat pool at the base -> thin rising vapor tail, faded before the geometric top
    prof = nt.nodes.new("ShaderNodeValToRGB"); pr = prof.color_ramp
    pr.elements[0].position = 0.0; pr.elements[0].color = (1, 1, 1, 1)
    pr.elements[1].position = 1.0; pr.elements[1].color = (0, 0, 0, 1)
    pr.elements.new(0.22).color = (0.7, 0.7, 0.7, 1); pr.elements.new(0.80).color = (0, 0, 0, 1)
    nt.links.new(sep.outputs["Z"], prof.inputs[0])
    flame = nt.nodes.new("ShaderNodeValToRGB")
    flame.color_ramp.elements[0].position = 0.30; flame.color_ramp.elements[0].color = (0, 0, 0, 1)
    flame.color_ramp.elements[1].position = 0.68; flame.color_ramp.elements[1].color = (1, 1, 1, 1)
    nt.links.new(noise.outputs["Fac"], flame.inputs[0])
    pw = nt.nodes.new("ShaderNodeMath"); pw.operation = 'MULTIPLY'
    nt.links.new(prof.outputs["Color"], pw.inputs[0]); nt.links.new(flame.outputs["Color"], pw.inputs[1])
    dens = nt.nodes.new("ShaderNodeMath"); dens.operation = 'MULTIPLY'
    nt.links.new(pw.outputs[0], dens.inputs[0]); dens.inputs[1].default_value = PL_OPAC
    # emission color by HEIGHT, tinted the player hue so the bloom reads in that color
    ecol = nt.nodes.new("ShaderNodeValToRGB"); cr = ecol.color_ramp
    mix3 = lambda c, d, t: tuple(c[i] * (1 - t) + d[i] * t for i in range(3))
    cr.elements[0].position = 0.0; cr.elements[0].color = lin3(mix3(basecol, (1, 1, 1), 0.30))
    cr.elements.new(0.5).color = lin3(basecol)
    cr.elements[2].position = 1.0; cr.elements[2].color = lin3(mix3(basecol, (0, 0, 0), 0.35))
    nt.links.new(sep.outputs["Z"], ecol.inputs[0])
    emi = nt.nodes.new("ShaderNodeEmission"); emi.inputs["Strength"].default_value = PL_STR
    nt.links.new(ecol.outputs["Color"], emi.inputs["Color"])
    tr = nt.nodes.new("ShaderNodeBsdfTransparent"); mix = nt.nodes.new("ShaderNodeMixShader")
    nt.links.new(dens.outputs[0], mix.inputs[0])
    nt.links.new(tr.outputs[0], mix.inputs[1]); nt.links.new(emi.outputs[0], mix.inputs[2])
    nt.links.new(mix.outputs[0], out.inputs["Surface"])
    for attr, val in [("surface_render_method", "BLENDED"), ("blend_method", "BLEND"),
                      ("use_backface_culling", False), ("use_transparent_shadow", False),
                      ("shadow_method", "NONE")]:
        try: setattr(m, attr, val)
        except Exception: pass
    return m

def plasma_obj(aid, basecol):
    bpy.ops.mesh.primitive_cylinder_add(radius=PL_R, depth=PL_H, end_fill_type='NOTHING', location=(9000, 9000, 0))
    o = bpy.context.active_object; o.name = "plasma_" + aid
    for v in o.data.vertices: v.co.z += PL_H / 2.0
    o.data.update()
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    o.data.materials.clear(); o.data.materials.append(plasma_mat(aid, basecol))
    o.scale = (1.0, 1.0, 0.0)
    return o

def plasma_bell(aid, pcol, frm_xy, to_xy, t0, t1):
    """A plasma pillar that follows a token's glide with one z-scale bell over [t0,t1]."""
    if not USE_PLASMA: return
    po = plasma_obj(aid, vivid(pcol))
    (fx, fy), (tx, ty) = frm_xy, to_xy
    po.location = (fx, fy, PL_BASE)
    po.keyframe_insert("location", frame=0); po.keyframe_insert("location", frame=int(t0))
    po.location = (tx, ty, PL_BASE)
    po.keyframe_insert("location", frame=int(t1)); po.keyframe_insert("location", frame=LEN)
    peak = (t0 + t1) / 2.0
    po.scale = (1.0, 1.0, 0.0); po.keyframe_insert("scale", frame=0)
    for f, sv in [(max(0, t0 - PL_LEAD), 0.0), (peak, 1.0), (min(LEN, t1 + PL_LEAD), 0.0)]:
        po.scale = (1.0, 1.0, sv); po.keyframe_insert("scale", frame=int(round(f)))

# ---------------- tokens ----------------
DISC_H = 7.0                                               # chunky coin so the side/rim reads as 3D
def disc(name, rgb):
    bpy.ops.mesh.primitive_cylinder_add(vertices=64, radius=DISC_R, depth=DISC_H, location=(9000, 9000, 0))
    o = bpy.context.active_object; o.name = name
    bev = o.modifiers.new("bev", 'BEVEL'); bev.width = 1.1; bev.segments = 3   # rounded edge catches a highlight
    try: bpy.ops.object.shade_auto_smooth()
    except Exception:
        try: bpy.ops.object.shade_smooth()
        except Exception: pass
    o.data.materials.append(colmat(name + "_m", rgb, rough=0.34))   # matte plastic, LIT (no self-emission) -> reads 3D
    return o
def place(o, fr, xyz):
    o.location = xyz; o.keyframe_insert("location", frame=fr)

def static_tok(name, rgb, pos):                            # a token that never moves
    o = disc(name, rgb); place(o, 0, pos); place(o, LEN, pos); return o
def moving_tok(name, pcol, p0, p1, t0, t1, hop=8.0):       # glides p0 -> p1 over [t0,t1] + a peel-off z-hop
    rgb = PCOL[pcol]; o = disc(name, rgb)
    place(o, 0, p0); place(o, int(t0), p0)
    mid = ((p0[0]+p1[0])/2, (p0[1]+p1[1])/2, max(p0[2], p1[2]) + hop)
    place(o, int((t0+t1)//2), mid)
    place(o, int(t1), p1); place(o, LEN, p1)
    plasma_bell(name, rgb, (p0[0], p0[1]), (p1[0], p1[1]), t0, t1)
    return o

# Several tokens on ONE node are PILED into a coin-stack: each coin lifted one coin-height and
# nudged a hair sideways, so from the 3/4 camera you read distinct coins (a leaning pile), not one
# fat disc. k=0 is the bottom coin; a moving token lands on the TOP of its destination stack.
STACK_DZ   = DISC_H + 0.15                                 # vertical step (coins rest flush on each other)
STACK_LEAN = (3.9, 2.3)                                    # per-level sideways nudge (~screen-horizontal) -> rims read
def stack_pos(base, k):
    return (base[0] + STACK_LEAN[0]*k, base[1] + STACK_LEAN[1]*k, base[2] + STACK_DZ*k)

sc.frame_start = 0; sc.frame_end = LEN

if os.environ.get("CHECK"):
    for v in range(6):
        static_tok(f"chk{v}", (1, 0.2, 0.2), slot(v))
else:
    # PLACEMENT: every token sits OFF its ring's centre node (phi != 0) so the printed VALUE-DOTS at
    # the node stay visible. Idle reserves are PILED in a leaned per-player coin-stack on the base ring
    # (green left | purple just right of the node | orange right). The orange & purple climbers STRADDLE
    # their shared node -- one each side -- on ring 4 (start) and ring 5 (apex), so they read as two and
    # never overlap each other or the value. Small inner rings need a wider angle for the same gap (A3 >> A0).
    A3, A1, A0 = 36.0, 18.0, 15.0          # lone-token offsets: ring 3 (green start) / ring 1 (purple peel) / ring 0 (purple reserve)
    PAIR4, PAIR5 = 15.0, 9.0               # orange|purple half-split: ring 4 (pink node) / ring 5 (gold apex)
    ZL, ZR = -34.0, 34.0                   # green / orange reserve stacks, wide on the base arc
    zbase = {"green": slot(0, ZL), "purple": slot(0, A0), "orange": slot(0, ZR)}
    # FOUR staggered advances (bells overlap, different lengths); green 3->4 lands BEFORE purple's 0->1 climax
    M_O1 = (20, 52); M_P1 = (56, 88); M_G3 = (94, 126); M_P2 = (132, 170)

    # GREEN -- climbs ring 3 -> 4 (3rd move, into the now-vacated LEFT flank of the pink node); a 2-coin base reserve (score 4)
    moving_tok("g1", "green", slot(3, -A3), slot(4, -PAIR4), *M_G3, hop=14)
    static_tok("g2", PCOL["green"],  stack_pos(zbase["green"], 0))
    static_tok("g3", PCOL["green"],  stack_pos(zbase["green"], 1))
    # ORANGE -- a 2-coin base reserve; one token climbs ring 4 -> 5 to the LEFT of the gold apex (score 5)
    static_tok("o2", PCOL["orange"], stack_pos(zbase["orange"], 0))
    static_tok("o3", PCOL["orange"], stack_pos(zbase["orange"], 1))
    moving_tok("o1", "orange", slot(4, -PAIR4), slot(5, -PAIR5), *M_O1, hop=14)
    # PURPLE -- one base reserve; one token climbs 4 -> 5 to the RIGHT of the apex, one peels 0 -> 1 (score 6, climax)
    static_tok("p3", PCOL["purple"], stack_pos(zbase["purple"], 0))
    moving_tok("p1", "purple", slot(4, PAIR4), slot(5, PAIR5), *M_P1, hop=14)
    moving_tok("p2", "purple", stack_pos(zbase["purple"], 1), slot(1, A1), *M_P2, hop=36)

# camera: a 3/4 angle (off-centre, lower) so the board reads dimensional + the coins show their sides
cam_d = bpy.data.cameras.new("Cam"); cam_d.lens = 52; cam_d.clip_end = 20000
cam = bpy.data.objects.new("Cam", cam_d); C().objects.link(cam); sc.camera = cam
cam.rotation_mode = 'QUATERNION'
def smooth(u): return u*u*(3-2*u)
for f in range(LEN+1):
    u = f/LEN
    az = math.radians(-62 + smooth(u)*9); el = math.radians(55 - smooth(u)*3); dist = 545 - smooth(u)*20
    loc = Vector((dist*math.cos(el)*math.cos(az), dist*math.cos(el)*math.sin(az), dist*math.sin(el)))
    cam.location = loc; cam.rotation_quaternion = (Vector((0, 0, 6)) - loc).to_track_quat('-Z', 'Y')
    cam.keyframe_insert("location", frame=f); cam.keyframe_insert("rotation_quaternion", frame=f)

# ---------------- compositor bloom (makes the plasma emission read as glow) ----------------
def add_bloom():
    sc.use_nodes = True
    nt = getattr(sc, "node_tree", None)
    if nt is None and hasattr(sc, "compositing_node_group"):
        nt = sc.compositing_node_group
        if nt is None:
            nt = bpy.data.node_groups.new("Comp", "CompositorNodeTree"); sc.compositing_node_group = nt
    if nt is None: return "no compositor api"
    for n in list(nt.nodes): nt.nodes.remove(n)
    src = nt.nodes.new("CompositorNodeRLayers")
    try: out = nt.nodes.new("CompositorNodeComposite")
    except Exception: out = nt.nodes.new("NodeGroupOutput")
    gl = nt.nodes.new("CompositorNodeGlare")
    socks = {"Type": 'Bloom', "Highlights Threshold": float(os.environ.get("BLOOMTHRESH", "2.5")),
             "Strength": float(os.environ.get("BLOOMSTR", "2.0")), "Size": float(os.environ.get("BLOOMSIZE", "0.9"))}
    for name, val in socks.items():
        if name in gl.inputs:
            try: gl.inputs[name].default_value = val
            except Exception: pass
    nt.links.new(src.outputs["Image"], gl.inputs["Image"])
    nt.links.new(gl.outputs["Image"], out.inputs[0])
    return "ok"
if os.environ.get("BLOOM", "1") != "0":
    try: print("bloom:", add_bloom())
    except Exception as e: print("bloom skipped:", e)

single = os.environ.get("FRAME")
sc.render.image_settings.file_format = 'PNG'
if single:
    sc.frame_set(int(single)); sc.render.filepath = f"{FR}/single_{int(single):04d}"
    bpy.ops.render.render(write_still=True)
else:
    sc.render.filepath = f"{FR}/f"; bpy.ops.render.render(animation=True)
print("power done")
