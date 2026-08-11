"""Frame-0 packing diagram — REAL measured dimensions, 13x13in cap, 2 tiers (no minimal set).

BOTTOM SLAB: folded board + power-board + rulebook + player aids stacked centrally, the
  26-card mutation DECK (REAL ⌀123mm round ability cards) on top, and platform (⌀46) +
  power-token (⌀30) stacks tucked into the corner margins.
TOP INSERT (drawn EXPLODED up so both tiers read): 6 compartments = 5 player sets of 12
  pieces STANDING + all 60 FOOD nested. ~34-40mm pitch (pieces touch, never interpenetrate).

  ~/Downloads/blender-5.1.2-linux-x64/blender -b --threads 4 --python build_packed_box.py
Env: RESX/RESY, SAMPLES, EXPLODE (tier gap mm, default 175), FR.
"""
import bpy, os, sys, math, colorsys
from mathutils import Vector
P = os.path.dirname(os.path.abspath(__file__)); sys.path.insert(0, P)
ASSETS=f"{P}/clip_assets"; LA=f"{P}/layout_assets"
FR=os.environ.get("FR","/mnt/data/archive/organism-renders/packed"); os.makedirs(FR,exist_ok=True)
RESX=int(os.environ.get("RESX","1500")); RESY=int(os.environ.get("RESY","1100")); SAMPLES=int(os.environ.get("SAMPLES","28"))
EXPLODE=float(os.environ.get("EXPLODE","175"))

def _l(c): return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]),_l(c[1]),_l(c[2]),1.0)
def hsl(h,s,l): return colorsys.hls_to_rgb(h/360.0,l/100.0,s/100.0)
PLAYERS=["red","yellow","blue","purple","green"]
PCOL={"red":hsl(353,62,60),"yellow":hsl(45,78,62),"blue":hsl(196,45,58),"purple":hsl(266,55,60),"green":hsl(118,45,68)}
GOLD=(242/255,230/255,158/255); MUTC=(0.42,0.62,0.55)

bpy.ops.wm.read_factory_settings(use_empty=True); sc=bpy.context.scene
sc.render.engine="BLENDER_WORKBENCH"
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
    m.diffuse_color=lin3(rgb); return m
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
FOODT=load(f"{P}/renders/food/FOOD_slip.obj",zup=False)
def dup(tpl,name,mat,loc,rotz=0.0):
    o=bpy.data.objects.new(name,tpl.data); Coll().objects.link(o)
    o.location=loc; o.rotation_euler=(0,0,rotz); o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o
bpy.ops.mesh.primitive_cylinder_add(vertices=48,radius=1,depth=1,location=(9000,9000,0))
CYL=bpy.context.active_object; CYL.name="cyltpl"
if not CYL.data.materials: CYL.data.materials.append(colmat("ct",(0.5,0.5,0.5)))
CYL.hide_render=True
def coin(name,mat,loc,r,h):
    o=bpy.data.objects.new(name,CYL.data); Coll().objects.link(o)
    o.location=(loc[0],loc[1],loc[2]+h/2); o.scale=(r,r,h)
    o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o
def slab(name,x0,y0,x1,y1,z0,z1,mat):
    v=[(x0,y0,z0),(x1,y0,z0),(x1,y1,z0),(x0,y1,z0),(x0,y0,z1),(x1,y0,z1),(x1,y1,z1),(x0,y1,z1)]
    m=bpy.data.meshes.new(name); ob=bpy.data.objects.new(name,m); Coll().objects.link(ob)
    m.from_pydata(v,[],[(0,1,2,3),(4,5,6,7),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); m.update(); m.materials.append(mat)
    for p in m.polygons: p.use_smooth=False
    return ob

HW=165.0; WALL=3.0                 # interior half = 165 -> 330mm floor = 13.0 in
# ============ BOTTOM SLAB (real flats + mutation deck + platform/token stacks) ============
z=0.0
slab("rulebook",-105,-105,105,105,z,z+3,colmat("rule",(0.86,0.84,0.78))); z+=3          # 210x210 rulebook
coin("powerboard",colmat("pboard",(0.30,0.42,0.36)),(0,0,z),115,4); z+=4                 # ⌀230 power-board
slab("board_fold",-118,-118,118,118,z,z+8,colmat("board",(0.20,0.34,0.40))); z+=8        # folded board proxy ~236
for k in range(5): coin(f"aid{k}",colmat("aid",(0.80,0.78,0.70)),(0,0,z),52,1.4); z+=1.4 # 5 player aids ⌀104
mz=z
for k in range(26): coin(f"mut{k}",colmat("mut",MUTC),(0,0,mz),61.5,0.5); mz+=0.5         # ⌀123 mutation DECK (real)
SLAB_TOP=max(z,mz)
CN=[(-1,-1,"red"),(1,-1,"yellow"),(-1,1,"blue"),(1,1,"purple")]
for sx,sy,pl in CN:                                                                       # platform+token stacks in corners
    cx,cy=sx*(HW-28),sy*(HW-28)
    for k in range(9): coin(f"pl_{pl}{k}",colmat("pl_"+pl,PCOL[pl]),(cx,cy,k*2.0),23,2.0)  # 9 platforms ⌀46
    tx=cx+(30 if sx<0 else -30)
    for k in range(3): coin(f"tk_{pl}{k}",colmat("tk_"+pl,PCOL[pl]),(tx,cy,k*5.0),15,5.0)  # 3 power tokens ⌀30
for k in range(9): coin(f"pl_green{k}",colmat("pl_green",PCOL["green"]),(0,-(HW-28),k*2.0),23,2.0)
for k in range(3): coin(f"tk_green{k}",colmat("tk_green",PCOL["green"]),(34,-(HW-28),k*5.0),15,5.0)
# short outer walls around the slab (open tray)
BW=colmat("box",(0.10,0.15,0.17),0.55)
for (x0,y0,x1,y1) in [(-HW-WALL,-HW-WALL,HW+WALL,-HW),(-HW-WALL,HW,HW+WALL,HW+WALL),
                      (-HW-WALL,-HW,-HW,HW),(HW,-HW,HW+WALL,HW)]:
    slab("ow",x0,y0,x1,y1,0,SLAB_TOP+6,BW)

# ============ TOP INSERT (exploded up): 6 compartments (5 players + food) ============
INS=SLAB_TOP+EXPLODE
insert_mat=colmat("insert",(0.14,0.17,0.2),0.5)
slab("insfloor",-HW,-HW,HW,HW,INS-6,INS,insert_mat)
CW,CD=160.0,106.0
def cellc(gx,gy): return ((gx-0.5)*(CW+WALL),(1-gy)*(CD+WALL))
def fill_player(ox,oy,pl):
    m=colmat("pm_"+pl,PCOL[pl]); types=["EAT"]*4+["MOVE"]*4+["GROW"]*4
    for idx,t in enumerate(types):
        x=ox+((idx%4)-1.5)*40.0; y=oy+((idx//4)-1)*34.0
        dup(TPL[t],f"{pl}{idx}",m,(x,y,INS),rotz=math.radians((idx*47)%360))
def fill_food(ox,oy):
    g=colmat("gold",GOLD); n=0
    for gx in range(4):
        for gy in range(2):
            for k in range(8):
                if n>=60: break
                dup(FOODT,f"food{n}",g,(ox+(gx-1.5)*38.0,oy+(gy-0.5)*50.0,INS+k*6.92)); n+=1
SLOTS=[(0,0,"red"),(1,0,"yellow"),(0,1,"blue"),(1,1,"purple"),(0,2,"green"),(1,2,"food")]
for gx,gy,who in SLOTS:
    ox,oy=cellc(gx,gy)
    (fill_food if who=="food" else (lambda a,b: fill_player(a,b,who)))(ox,oy)
    print("filled",who); sys.stdout.flush()
# insert dividers
for i in (1,): slab(f"vd{i}",-WALL/2,-HW,WALL/2,HW,INS,INS+58,insert_mat)
for j in (1,2): slab(f"hd{j}",-HW,(1.5-j)*(CD+WALL)-WALL/2,HW,(1.5-j)*(CD+WALL)+WALL/2,INS,INS+58,insert_mat)

cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=42; cam_d.clip_end=14000
cam=bpy.data.objects.new("Cam",cam_d); Coll().objects.link(cam); sc.camera=cam
loc=Vector((0,-700,540)); q=(Vector((0,0,INS*0.42))-loc).to_track_quat('-Z','Y')
cam.location=loc; cam.rotation_euler=q.to_euler()
sc.render.image_settings.file_format='PNG'; sc.render.filepath=f"{FR}/packed_real"
EXTW=2*HW+2*WALL
print(f"REAL 13in BOX  interior {2*HW:.0f} x {2*HW:.0f} mm  (13.0 x 13.0 in)")
print(f"  bottom slab height {SLAB_TOP:.1f}mm ; tallest piece ~53mm ; food stack ~60mm -> interior depth ~{SLAB_TOP+62:.0f}mm ({(SLAB_TOP+62)/25.4:.1f}in)")
print(f"  EXTERIOR ~{EXTW:.0f} x {EXTW:.0f} x {SLAB_TOP+62+5:.0f} mm"); sys.stdout.flush()
bpy.ops.render.render(write_still=True); print("wrote",sc.render.filepath)
