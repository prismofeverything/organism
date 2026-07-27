"""EVERYTHING IN THE BOX — the real 11.3in insert packed, at real scale (1u=1mm). Three tiers:
floor = quad-folded board; middle = nested stacks (disks/food/tokens/platforms) on the board;
top = the 60 player pieces standing on a shelf (the hero layer you see with the lid off).
This is the frame-0 the unboxing will animate FROM.  Uses only the proven render path.

  ~/Downloads/.../blender -b --threads 4 --python build_packed.py
Env: RESX/RESY, SAMPLES, FRAME(ignored), VIEW(iso|top)
"""
import bpy, os, math, colorsys, sys
from mathutils import Vector
def ck(s): print("CK:",s); sys.stdout.flush()
P = os.path.dirname(os.path.abspath(__file__)); ASSETS = f"{P}/clip_assets"
FR = os.environ.get("FR", "/mnt/data/archive/organism-renders/packed"); os.makedirs(FR, exist_ok=True)
RESX = int(os.environ.get("RESX","1400")); RESY = int(os.environ.get("RESY","950")); SAMPLES=int(os.environ.get("SAMPLES","24"))

def _l(c): return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]),_l(c[1]),_l(c[2]),1.0)
def hsl(h,s,l): return colorsys.hls_to_rgb(h/360.0,l/100.0,s/100.0)
PLAYERS=["red","yellow","blue","purple","green"]
PCOL={"red":hsl(353,62,58),"yellow":hsl(45,78,60),"blue":hsl(196,45,56),"purple":hsl(266,55,60),"green":hsl(118,45,66)}
GOLD=(242/255,230/255,158/255)

bpy.ops.wm.read_factory_settings(use_empty=True); sc=bpy.context.scene
sc.render.engine="CYCLES"                     # CPU ray-tracer: most robust for this scene
try: sc.cycles.device='CPU'; sc.cycles.samples=24; sc.cycles.use_denoising=True
except Exception: pass
sc.render.resolution_x=RESX; sc.render.resolution_y=RESY
try: sc.view_settings.view_transform='Standard'
except Exception: pass
w=bpy.data.worlds.new("W"); sc.world=w; w.use_nodes=True
w.node_tree.nodes["Background"].inputs[0].default_value=(0.05,0.055,0.065,1.0)
def C(): return bpy.context.collection
def sun(nm,en,rx,rz):
    d=bpy.data.lights.new(nm,'SUN'); d.energy=en; d.angle=math.radians(4)
    o=bpy.data.objects.new(nm,d); C().objects.link(o); o.rotation_euler=(math.radians(rx),0,math.radians(rz))
sun("Key",4.6,46,38); sun("Fill",2.1,60,-120); sun("Rim",2.6,116,175)
def colmat(name,rgb,rough=0.45):
    m=bpy.data.materials.new(name); m.use_nodes=True
    b=m.node_tree.nodes["Principled BSDF"]; b.inputs["Base Color"].default_value=lin3(rgb); b.inputs["Roughness"].default_value=rough
    m.diffuse_color=lin3(rgb)                  # workbench MATERIAL colour
    return m
def imgmat(name,path):
    m=bpy.data.materials.new(name); m.use_nodes=True; nt=m.node_tree
    b=nt.nodes["Principled BSDF"]; t=nt.nodes.new("ShaderNodeTexImage"); t.image=bpy.data.images.load(path)
    nt.links.new(t.outputs["Color"],b.inputs["Base Color"]); b.inputs["Roughness"].default_value=0.6; return m
# table
me=bpy.data.meshes.new("T"); o=bpy.data.objects.new("T",me); C().objects.link(o)
s=3000; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)],[],[(0,1,2,3)]); me.update()
me.materials.append(colmat("table",(0.12,0.13,0.15),0.85)); o.location=(0,0,-0.1)

def load(path,zup=True):
    if zup: bpy.ops.wm.obj_import(filepath=path,up_axis='Z',forward_axis='Y')
    else: bpy.ops.wm.obj_import(filepath=path,up_axis='Y',forward_axis='NEGATIVE_Z')
    o=[x for x in bpy.context.selected_objects if x.type=='MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active=o
    bpy.ops.object.transform_apply(location=False,rotation=True,scale=True)
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    if not o.data.materials: o.data.materials.append(colmat(o.name+"_b",(0.5,0.5,0.5)))
    o.hide_render=True; o.location=(9000,9000,0); return o
TPL={t:load(f"{P}/out/{t}_sculpt_graft.obj") for t in ["EAT","MOVE","GROW"]}
MIN={t:load(f"{P}/out/{t}_mindisk.obj") for t in ["EAT","MOVE","GROW"]}
FOODT=load(f"{P}/renders/food/FOOD_slip.obj",zup=False)
ck("loaded")
def dup(tpl,name,mat,loc,rotz=0.0):
    o=bpy.data.objects.new(name,tpl.data); C().objects.link(o)
    o.location=loc; o.rotation_euler=(0,0,rotz); o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o
def boxmesh(name,x0,y0,x1,y1,z0,z1,mat,faces=None):
    v=[(x0,y0,z0),(x1,y0,z0),(x1,y1,z0),(x0,y1,z0),(x0,y0,z1),(x1,y0,z1),(x1,y1,z1),(x0,y1,z1)]
    f=faces if faces else [(0,1,2,3),(4,5,6,7),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]
    m=bpy.data.meshes.new(name); ob=bpy.data.objects.new(name,m); C().objects.link(ob)
    m.from_pydata(v,[],f); m.update(); m.materials.append(mat)
    for p in m.polygons: p.use_smooth=False
    return ob

# ===== geometry: box 318x286x135, board floor / stacks mid / pieces top =====
IW,ID,BD = 312.0, 280.0, 132.0
BOARD_H, STACK_Z, SHELF_Z, PIECE_Z = 14.0, 15.0, 72.0, 75.0
# folded board (solid proxy)
boxmesh("board",-135,-140,135,140,0,BOARD_H,colmat("bd",(0.16,0.28,0.33)))
ck("before board"); pmat={pl:colmat("p_"+pl,PCOL[pl]) for pl in PLAYERS}

# --- middle tier: nested stacks on the board (hidden under the pieces, present) ---
def stackcol(mkdup, n, pitch, x, y, z):
    for k in range(n): mkdup(k,(x,y,z+k*pitch))
ck("board done"); SX=-135; SY=-125; col=0
def slot():
    global col; x=SX+ (col%9)*30; y=SY+(col//9)*30; col+=1; return x,y
for pi,pl in enumerate(PLAYERS):                                   # disks: 3 stacks of 4 per player
    for t in ["EAT","MOVE","GROW"]:
        x,y=slot(); stackcol(lambda k,L,t=t,pl=pl:dup(MIN[t],f"d{pl}{t}{k}",pmat[pl],L,rotz=0.3*k), 4, 7.5, x,y, STACK_Z)
for i in range(8):                                                # food: 8 nested stacks
    x,y=slot(); stackcol(lambda k,L:dup(FOODT,f"food{L[0]:.0f}{k}",colmat("g",GOLD),L), 8, 6.9, x,y, STACK_Z)

ck("stacks done")
# --- top tier: 60 pieces standing on a shelf (the HERO layer, lid-off view) ---
boxmesh("shelf",-IW/2,-ID/2,IW/2,ID/2,SHELF_Z,SHELF_Z+2,colmat("sh",(0.14,0.17,0.2)))
i=0; PIT=39.0
for pl in PLAYERS:
    for t in (["EAT"]*4+["MOVE"]*4+["GROW"]*4):
        gx,gy = i%8, i//8
        x=(gx-3.5)*PIT + (PIT/2 if gy%2 else 0); y=(gy-3.5)*33.8
        dup(TPL[t],f"pc{i}",pmat[pl],(x,y,PIECE_Z+2),rotz=math.radians((i*41)%360)); i+=1

ck("pieces done")
# --- box walls (cutaway: keep back +Y and right +X so we see in from front-left) ---
bw=colmat("box",(0.10,0.14,0.16),0.6); WALL=3
boxmesh("floor",-IW/2-WALL,-ID/2-WALL,IW/2+WALL,ID/2+WALL,-WALL,0,bw)
boxmesh("wR",IW/2,-ID/2-WALL,IW/2+WALL,ID/2+WALL,0,BD,bw)
boxmesh("wB",-IW/2-WALL,ID/2,IW/2+WALL,ID/2+WALL,0,BD,bw)
# lid beside, cover up
lid=boxmesh("lid",-IW/2-WALL,-ID/2-WALL,IW/2+WALL,ID/2+WALL,0,20,bw); lid.location=(0,-ID-90,0)
lt=bpy.data.meshes.new("lt"); lto=bpy.data.objects.new("lt",lt); C().objects.link(lto)
lt.from_pydata([(-IW/2-WALL,-ID/2-WALL,20),(IW/2+WALL,-ID/2-WALL,20),(IW/2+WALL,ID/2+WALL,20),(-IW/2-WALL,ID/2+WALL,20)],[],[(0,1,2,3)])
lt.update(); lt.materials.append(imgmat("cov",f"{ASSETS}/box_top.png")); uv=lt.uv_layers.new()
for i2,l in enumerate(lt.polygons[0].loop_indices): uv.data[l].uv=[(0,0),(1,0),(1,1),(0,1)][i2]
lto.location=(0,-ID-90,0)

ck("walls done"); cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=42; cam_d.clip_end=12000
cam=bpy.data.objects.new("Cam",cam_d); C().objects.link(cam); sc.camera=cam
loc=Vector((-300,-470,430)); q=(Vector((-10,-30,70))-loc).to_track_quat('-Z','Y')
cam.location=loc; cam.rotation_euler=q.to_euler()
sc.render.image_settings.file_format='PNG'; sc.render.filepath=f"{FR}/packed_final"
print(f"BOX {IW+2*WALL:.0f} x {ID+2*WALL:.0f} x {BD:.0f} mm ({(IW+2*WALL)/25.4:.1f} x {(ID+2*WALL)/25.4:.1f} x {BD/25.4:.1f} in)")
bpy.ops.render.render(write_still=True); print("wrote",sc.render.filepath)
