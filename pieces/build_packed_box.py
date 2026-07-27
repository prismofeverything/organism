"""Frame-0: the REAL box with a 6-COMPARTMENT INSERT (real scale, 1u=1mm).

5 player compartments (each = that player's 12 pieces standing + 3 nested disk stacks + a power-
token stack + a platform stack, all in the player colour) + a 6th compartment for ALL 60 food.
The quad-folded board is the bottom layer under the insert. Mirrors the hero's 6 sections.

  ~/Downloads/.../blender -b --threads 4 --python build_packed_box.py
Env: RESX/RESY, SAMPLES.
"""
import bpy, os, sys, math, colorsys
from mathutils import Vector
P = os.path.dirname(os.path.abspath(__file__)); sys.path.insert(0, P)
ASSETS = f"{P}/clip_assets"; LA = f"{P}/layout_assets"
FR = os.environ.get("FR", "/mnt/data/archive/organism-renders/packed"); os.makedirs(FR, exist_ok=True)
RESX = int(os.environ.get("RESX","1500")); RESY = int(os.environ.get("RESY","1000")); SAMPLES=int(os.environ.get("SAMPLES","28"))

def _l(c): return c/12.92 if c <= 0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]), _l(c[1]), _l(c[2]), 1.0)
def hsl(h,s,l): return colorsys.hls_to_rgb(h/360.0,l/100.0,s/100.0)
PLAYERS = ["red","yellow","blue","purple","green"]
PCOL = {"red":hsl(353,62,60),"yellow":hsl(45,78,62),"blue":hsl(196,45,58),"purple":hsl(266,55,60),"green":hsl(118,45,68)}
GOLD = (242/255,230/255,158/255)

bpy.ops.wm.read_factory_settings(use_empty=True); sc = bpy.context.scene
sc.render.engine="BLENDER_WORKBENCH"                    # stable + fast for a packing diagram
sc.display.shading.light='STUDIO'; sc.display.shading.color_type='MATERIAL'
sc.display.shading.show_shadows=True; sc.display.shading.show_cavity=True
try: sc.display.render_aa='8'
except Exception: pass
sc.render.resolution_x=RESX; sc.render.resolution_y=RESY
try: sc.view_settings.view_transform='Standard'
except Exception: pass
w=bpy.data.worlds.new("W"); sc.world=w; w.use_nodes=True
w.node_tree.nodes["Background"].inputs[0].default_value=(0.05,0.055,0.065,1.0)
def Coll(): return bpy.context.collection
def sun(nm,en,rx,rz):
    d=bpy.data.lights.new(nm,'SUN'); d.energy=en; d.angle=math.radians(4)
    o=bpy.data.objects.new(nm,d); Coll().objects.link(o); o.rotation_euler=(math.radians(rx),0,math.radians(rz))
sun("Key",4.6,48,35); sun("Fill",2.2,60,-120); sun("Rim",2.4,116,170)
def colmat(name,rgb,rough=0.45):
    m=bpy.data.materials.new(name); m.use_nodes=True
    b=m.node_tree.nodes["Principled BSDF"]; b.inputs["Base Color"].default_value=lin3(rgb); b.inputs["Roughness"].default_value=rough
    m.diffuse_color=lin3(rgb)                            # workbench MATERIAL colour
    return m
def imgmat(name,path):
    m=bpy.data.materials.new(name); m.use_nodes=True; nt=m.node_tree
    b=nt.nodes["Principled BSDF"]; t=nt.nodes.new("ShaderNodeTexImage"); t.image=bpy.data.images.load(path)
    nt.links.new(t.outputs["Color"],b.inputs["Base Color"]); b.inputs["Roughness"].default_value=0.6; return m
me=bpy.data.meshes.new("T"); o=bpy.data.objects.new("T",me); Coll().objects.link(o)
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
def dup(tpl,name,mat,loc,rotz=0.0):
    o=bpy.data.objects.new(name,tpl.data); Coll().objects.link(o)
    o.location=loc; o.rotation_euler=(0,0,rotz); o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o
bpy.ops.mesh.primitive_cylinder_add(vertices=36,radius=1,depth=1,location=(9000,9000,0))  # ONE op
CYL=bpy.context.active_object; CYL.name="cyltpl"
if not CYL.data.materials: CYL.data.materials.append(colmat("ct",(0.5,0.5,0.5)))
CYL.hide_render=True
def coin(name,mat,loc,r,h):                              # instance the template (no ops in loop)
    o=bpy.data.objects.new(name,CYL.data); Coll().objects.link(o)
    o.location=(loc[0],loc[1],loc[2]+h/2); o.scale=(r,r,h)
    o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o

# ---------- ONE player compartment (inner CW x CD), z=0 at its floor ----------
CW, CD = 150.0, 168.0                          # inner compartment footprint
def fill_player(ox, oy, pl):
    m = colmat("pm_"+pl, PCOL[pl]); md = colmat("dm_"+pl, PCOL[pl])
    PIT=37.0
    for ti,t in enumerate(["EAT","MOVE","GROW"]):          # 12 pieces: 3 type-columns x 4
        for r in range(4):
            dup(TPL[t], f"{pl}{t}{r}", m, (ox+(ti-1)*PIT, oy+62-r*33, 0), rotz=math.radians((r*57)%360))
    for di,t in enumerate(["EAT","MOVE","GROW"]):           # 3 nested disk stacks (4 each)
        for k in range(4): dup(MIN[t], f"{pl}md{di}{k}", md, (ox+(di-1)*40, oy-78, 0.3+k*7.5))
    for k in range(3): coin(f"{pl}tk{k}", md, (ox+52, oy-78, k*5), 15, 5)     # power tokens
    for k in range(9): coin(f"{pl}pl{k}", md, (ox+52, oy-40, k*2.2), 18, 2.2) # platforms

def fill_food(ox, oy):
    n=0; g=colmat("gold",GOLD)
    for gx in range(4):
        for gy in range(3):
            for k in range(5):
                if n>=60: break
                dup(FOODT, f"food{n}", g, (ox+(gx-1.5)*34, oy+(gy-1)*40, k*6.92)); n+=1

# ---------- 6 compartments in a 3x2 grid (5 players + food) ----------
WALL=3.0; cw, cd = CW+WALL, CD+WALL
cols, rows = 3, 2
IW, ID = cols*cw, rows*cd
def cc(gx,gy): return ((gx-(cols-1)/2)*cw, ((rows-1)/2-gy)*cd)
slots = [(0,0,"red"),(1,0,"yellow"),(2,0,"blue"),(0,1,"purple"),(1,1,"green"),(2,1,"food")]
for gx,gy,who in slots:
    ox,oy = cc(gx,gy)
    if who=="food": fill_food(ox,oy)
    else: fill_player(ox,oy,who)
    print("filled", who); sys.stdout.flush()

# insert dividers (thin walls between compartments) + box + folded board (bottom layer, shown at edge)
CH=60.0
insert_mat=colmat("insert",(0.14,0.17,0.2),0.5)
def wallbox(x0,y0,x1,y1,z0,z1,mat,name):
    v=[(x0,y0,z0),(x1,y0,z0),(x1,y1,z0),(x0,y1,z0),(x0,y0,z1),(x1,y0,z1),(x1,y1,z1),(x0,y1,z1)]
    m=bpy.data.meshes.new(name); ob=bpy.data.objects.new(name,m); Coll().objects.link(ob)
    m.from_pydata(v,[],[(0,1,2,3),(4,5,6,7),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); m.update(); m.materials.append(mat)
    for p in m.polygons: p.use_smooth=False
    return ob
for i in range(1,cols): wallbox(-IW/2+i*cw-WALL/2,-ID/2,-IW/2+i*cw+WALL/2,ID/2,0,CH,insert_mat,f"vd{i}")
for j in range(1,rows): wallbox(-IW/2,-ID/2+j*cd-WALL/2,IW/2,-ID/2+j*cd+WALL/2,0,CH,insert_mat,f"hd{j}")
wallbox(-IW/2-WALL,-ID/2-WALL,IW/2+WALL,ID/2+WALL,-16,0,colmat("floor",(0.1,0.13,0.15)),"floor")  # box floor
# outer box walls
bw=colmat("box",(0.10,0.15,0.17),0.6)
for (x0,y0,x1,y1) in [(-IW/2-WALL,-ID/2-WALL,IW/2+WALL,-ID/2),(-IW/2-WALL,ID/2,IW/2+WALL,ID/2+WALL),
                      (-IW/2-WALL,-ID/2,-IW/2,ID/2),(IW/2,-ID/2,IW/2+WALL,ID/2)]:
    wallbox(x0,y0,x1,y1,-16,CH,bw,"ow")
# quad-folded board leaning behind (bottom layer in the real box). SOLID proxy -- the 6324px
# pentaboard texture is an OOM trigger in headless (see feedback_light_blender), skip it here.
fb=wallbox(-135,-135,135,135,0,14,colmat("bde",(0.20,0.34,0.40)),"boardf")
fb.location=(0,ID/2+180,0); fb.rotation_euler=(math.radians(-72),0,0)

cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=40; cam_d.clip_end=12000
cam=bpy.data.objects.new("Cam",cam_d); Coll().objects.link(cam); sc.camera=cam
loc=Vector((0,-560,620)); q=(Vector((0,0,20))-loc).to_track_quat('-Z','Y')
cam.location=loc; cam.rotation_euler=q.to_euler()
sc.render.image_settings.file_format='PNG'; sc.render.filepath=f"{FR}/packed_insert"
EXT_W, EXT_D, EXT_H = IW+2*WALL, ID+2*WALL, CH+16+4
print(f"INSERT box  {EXT_W:.0f} x {EXT_D:.0f} x {EXT_H:.0f} mm  ({EXT_W/25.4:.1f} x {EXT_D/25.4:.1f} x {EXT_H/25.4:.1f} in)")
print(f"  compartment inner {CW:.0f} x {CD:.0f} mm, 6 in a {cols}x{rows} grid"); sys.stdout.flush()
bpy.ops.render.render(write_still=True); print("wrote", sc.render.filepath)
