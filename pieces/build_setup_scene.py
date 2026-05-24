"""Build the ORGANISM game-at-setup scene in Blender (5.1.x).

Pulls the 2D print art from ~/Downloads/organism/prototype + the 3D pieces
(EAT/MOVE/GROW capped + FOOD) and lays out a tabletop:
  - Pentagon board (5-player side) centered
  - Box standing up behind
  - Player pieces lined up: N of each (EAT/MOVE/GROW) per player color (5 colors)
  - Power board, one player aid, rulebook, mutation-card stack
Outputs pieces/scene/organism_setup.blend + .png

Run:
  ~/Downloads/blender-5.1.2-linux-x64/blender --background --python build_setup_scene.py
"""
import bpy, bmesh, math, os, random
from mathutils import Vector

random.seed(7)
HERE   = os.path.dirname(os.path.abspath(__file__))
ART    = os.path.expanduser("~/Downloads/organism/prototype")
SCENE  = os.path.join(HERE, "scene"); os.makedirs(SCENE, exist_ok=True)

PER_PIECE = 4          # of each piece type per color (rulebook says 5; user asked 4)
COLORS = [             # name : sRGB 0-255 (sampled from the named color sheets)
    ("Purple", (156,110,144)),
    ("Blue",   (109,162,178)),
    ("Green",  ( 75,161,102)),
    ("Yellow", (220,173, 63)),
    ("Dark",   ( 79,101,115)),
]

def s2l(c):  # sRGB(0-255) -> linear
    c/=255.0
    return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin(rgb): return (s2l(rgb[0]), s2l(rgb[1]), s2l(rgb[2]), 1.0)

# ---------------- scene reset / engine ----------------
bpy.ops.wm.read_factory_settings(use_empty=True)
sc = bpy.context.scene
try:    sc.render.engine = 'BLENDER_EEVEE_NEXT'
except Exception: sc.render.engine = 'BLENDER_EEVEE'
try:    sc.eevee.taa_render_samples = 64
except Exception: pass
sc.render.resolution_x, sc.render.resolution_y = 1920, 1200
sc.render.film_transparent = False
# world ambient
w = bpy.data.worlds.new("W"); sc.world = w; w.use_nodes = True
w.node_tree.nodes["Background"].inputs[0].default_value = (0.62,0.63,0.66,1)
w.node_tree.nodes["Background"].inputs[1].default_value = 1.0

def C(): return bpy.context.collection

# ---------------- materials ----------------
def color_mat(name, rgb, rough=0.5):
    m = bpy.data.materials.new(name); m.use_nodes=True
    b = m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value = lin(rgb)
    b.inputs["Roughness"].default_value = rough
    return m

def img_mat(name, path, alpha=False, projection='FLAT'):
    m = bpy.data.materials.new(name); m.use_nodes=True
    nt = m.node_tree; b = nt.nodes["Principled BSDF"]
    tex = nt.nodes.new("ShaderNodeTexImage")
    tex.image = bpy.data.images.load(path); tex.projection = projection
    b.inputs["Roughness"].default_value = 0.85
    nt.links.new(tex.outputs["Color"], b.inputs["Base Color"])
    if projection == 'BOX':
        coord = nt.nodes.new("ShaderNodeTexCoord")
        nt.links.new(coord.outputs["Generated"], tex.inputs["Vector"])
    if alpha:
        nt.links.new(tex.outputs["Alpha"], b.inputs["Alpha"])
        for attr,val in (("blend_method",'CLIP'),("surface_render_method",'DITHERED'),
                         ("shadow_method",'CLIP')):
            try: setattr(m, attr, val)
            except Exception: pass
    return m

# ---------------- primitives ----------------
def plane(name, w, h, mat, loc=(0,0,0), rotz=0.0):
    me = bpy.data.meshes.new(name); o = bpy.data.objects.new(name, me); C().objects.link(o)
    me.from_pydata([(-w/2,-h/2,0),(w/2,-h/2,0),(w/2,h/2,0),(-w/2,h/2,0)],[],[(0,1,2,3)])
    uvl = me.uv_layers.new(); uvco=[(0,0),(1,0),(1,1),(0,1)]
    for lp in me.loops: uvl.data[lp.index].uv = uvco[lp.vertex_index]
    me.update(); me.materials.append(mat)
    o.location = loc; o.rotation_euler = (0,0,rotz); return o

def box(name, w, d, h, mat, loc=(0,0,0)):
    bpy.ops.mesh.primitive_cube_add(size=1, location=loc)
    o = bpy.context.active_object; o.name = name
    o.scale = (w, d, h)
    o.data.materials.append(mat)
    return o

# ---------------- table ----------------
plane("Table", 4000, 4000, color_mat("Table",(150,148,142),0.9), loc=(0,0,-0.5))

# ---------------- board (Pent, 5-player side) ----------------
BOARD = 540.0
plane("Board_Pent", BOARD, BOARD, img_mat("Board", f"{ART}/27_Pent_54cm_01.png", alpha=True), loc=(0,0,0))

# ---------------- box standing up behind ----------------
bx = box("Box", 330, 95, 300, img_mat("BoxArt", f"{ART}/Packaging_Print_01_300dpi.png", projection='BOX'),
         loc=(0, 470, 150))

# ---------------- import piece templates ----------------
def import_obj(path, name):
    bpy.ops.wm.obj_import(filepath=path, forward_axis='NEGATIVE_Z', up_axis='Y')
    objs=[o for o in bpy.context.selected_objects if o.type=='MESH']
    o=objs[0]; o.name=name
    if len(objs)>1:
        bpy.ops.object.select_all(action='DESELECT')
        for x in objs: x.select_set(True)
        bpy.context.view_layer.objects.active=o; bpy.ops.object.join()
    o.data.materials.clear(); o.data.materials.append(color_mat(name+"_def",(180,180,180)))
    o.hide_render = True; o.location=(5000,5000,0)   # park template off-camera
    return o

templates = {
    "EAT":  import_obj(f"{HERE}/EAT_connected.obj",  "EAT_T"),
    "MOVE": import_obj(f"{HERE}/MOVE_connected.obj", "MOVE_T"),
    "GROW": import_obj(f"{HERE}/GROW_connected.obj", "GROW_T"),
}
food_T = import_obj(f"{HERE}/FOOD.obj", "FOOD_T")

def place(template, mat, loc, rotz=0.0):
    o = template.copy()                       # shares mesh data (linked dup)
    C().objects.link(o)
    o.hide_render = False; o.location = loc; o.rotation_euler = (0,0,rotz)
    o.material_slots[0].link = 'OBJECT'; o.material_slots[0].material = mat
    return o

# ---------------- player supplies: 5 rows in front of board ----------------
cmats = {n: color_mat("M_"+n, rgb, 0.45) for n,rgb in COLORS}
order = ["EAT","MOVE","GROW"]
xstep, gap = 40.0, 22.0
row_w = PER_PIECE*len(order)*xstep + 2*gap
y0 = -350.0; ystep = -60.0
for ci,(cname,_) in enumerate(COLORS):
    y = y0 + ci*ystep
    x = -row_w/2 + xstep/2
    for pi,pname in enumerate(order):
        for k in range(PER_PIECE):
            place(templates[pname], cmats[cname], (x, y, 0), rotz=random.uniform(-0.25,0.25))
            x += xstep
        x += gap
    # a few food discs at the end of each row
    for k in range(3):
        place(food_T, cmats[cname], (x + k*16, y, 0))

# ---------------- power board (left), player aid (right) ----------------
plane("PowerBoard", 165,165, img_mat("PowerArt", f"{ART}/ScoreCard_01_print.png"), loc=(-470, 40, 0.2))
plane("PlayerAid", 190,190, img_mat("AidArt", f"{ART}/25_PLAYER-AID_09_print.png"), loc=(470, 40, 0.2))

# ---------------- rulebook (book) back-left ----------------
cover = f"{ART}/Manual_cover-01.png"
if os.path.exists(cover):
    rb = box("Rulebook", 200, 200, 16, img_mat("Cover", cover, projection='BOX'), loc=(-500, 360, 8))
    rb.rotation_euler = (0,0,math.radians(8))

# ---------------- mutation deck (back-right) ----------------
muts = sorted([f for f in os.listdir(f"{ART}/mutations") if f.endswith(".png")])
cream = color_mat("CardEdge",(225,222,210),0.7)
deckx, decky = 500, 360
for i in range(min(13,len(muts))):
    th = 0.5
    c = box(f"Mut{i}", 92, 71, th, cream, loc=(deckx, decky, th/2 + i*th))
    c.rotation_euler = (0,0,math.radians(90+random.uniform(-3,3)))
top = box("MutTop", 92, 71, 0.5, img_mat("MutFace", f"{ART}/mutations/{muts[0]}", projection='BOX'),
          loc=(deckx, decky, 13*0.5 + 0.3))
top.rotation_euler = (0,0,math.radians(90))

# ---------------- lights (suns: distance-independent at mm scale) ----------------
def sun(name, energy, rot):
    d=bpy.data.lights.new(name,'SUN'); d.energy=energy; d.angle=math.radians(3)
    o=bpy.data.objects.new(name,d); C().objects.link(o); o.rotation_euler=rot; return o
sun("Key", 4.0, (math.radians(50), math.radians(12), math.radians(40)))
sun("Fill",1.3, (math.radians(60), math.radians(-20), math.radians(-120)))

# ---------------- camera ----------------
tgt = bpy.data.objects.new("Target", None); C().objects.link(tgt); tgt.location=(0, -40, 0)
cam_d = bpy.data.cameras.new("Cam"); cam_d.lens = 40; cam_d.clip_end = 20000
cam = bpy.data.objects.new("Cam", cam_d); C().objects.link(cam); sc.camera = cam
cam.location = (0, -900, 1250)
con = cam.constraints.new('TRACK_TO'); con.target = tgt; con.track_axis='TRACK_NEGATIVE_Z'; con.up_axis='UP_Y'

# ---------------- render + save ----------------
sc.render.filepath = f"{SCENE}/organism_setup.png"
bpy.ops.render.render(write_still=True)
bpy.ops.wm.save_as_mainfile(filepath=f"{SCENE}/organism_setup.blend")
print("WROTE", f"{SCENE}/organism_setup.png", "and .blend")
