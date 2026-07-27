"""FINAL packing -> the manufacturable <=13x13 box, shown EXPLODED in 3 tiers:
  tier0 floor : quad-folded board
  tier1       : 4 tipped-piece cradle compartments (players 1-4)
  tier2       : 5th compartment + ALL nested stacks (food/disks/tokens/platforms)
Everything nested where it can be, spaced by real mesh bounds (no overlap). Cycles.
  ~/Downloads/.../blender -b --threads 4 --python build_final.py
"""
import bpy, os, math, colorsys
from mathutils import Vector
P=os.path.dirname(os.path.abspath(__file__))
FR=os.environ.get("FR","/mnt/data/archive/organism-renders/packed"); os.makedirs(FR,exist_ok=True)
RESX=int(os.environ.get("RESX","1500")); RESY=int(os.environ.get("RESY","1050")); SAMPLES=int(os.environ.get("SAMPLES","20"))
VOX=float(os.environ.get("VOX","7.0")); BH=float(os.environ.get("BH","16.0"))
def _l(c): return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]),_l(c[1]),_l(c[2]),1.0)
def hsl(h,s,l): return colorsys.hls_to_rgb(h/360.0,l/100.0,s/100.0)
PLAYERS=["red","yellow","blue","purple","green"]
PCOL={"red":hsl(353,62,58),"yellow":hsl(45,78,60),"blue":hsl(196,45,56),"purple":hsl(266,55,60),"green":hsl(118,45,66)}
GOLD=(242/255,230/255,158/255)
bpy.ops.wm.read_factory_settings(use_empty=True); sc=bpy.context.scene
sc.render.engine="CYCLES"
try: sc.cycles.device='CPU'; sc.cycles.samples=SAMPLES; sc.cycles.use_denoising=True
except Exception: pass
sc.render.resolution_x=RESX; sc.render.resolution_y=RESY
try: sc.view_settings.view_transform='Standard'
except Exception: pass
w=bpy.data.worlds.new("W"); sc.world=w; w.use_nodes=True
w.node_tree.nodes["Background"].inputs[0].default_value=(0.05,0.055,0.065,1.0)
def C(): return bpy.context.collection
def sun(nm,en,rx,rz):
    d=bpy.data.lights.new(nm,'SUN'); d.energy=en; d.angle=math.radians(5)
    o=bpy.data.objects.new(nm,d); C().objects.link(o); o.rotation_euler=(math.radians(rx),0,math.radians(rz))
sun("K",4.0,54,35); sun("F",1.8,62,-120); sun("R",2.0,116,175)
def colmat(name,rgb,rough=0.45):
    m=bpy.data.materials.new(name); m.use_nodes=True
    b=m.node_tree.nodes["Principled BSDF"]; b.inputs["Base Color"].default_value=lin3(rgb); b.inputs["Roughness"].default_value=rough
    return m
me=bpy.data.meshes.new("T"); o=bpy.data.objects.new("T",me); C().objects.link(o)
s=2500; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)],[],[(0,1,2,3)]); me.update()
me.materials.append(colmat("tab",(0.12,0.13,0.15),0.85)); o.location=(0,0,-40)
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
bpy.ops.mesh.primitive_cylinder_add(vertices=32,radius=1,depth=1,location=(9000,9000,0))
CYL=bpy.context.active_object; CYL.name="cyl"; CYL.data.materials.append(colmat("cy",(0.5,0.5,0.5))); CYL.hide_render=True
DIM={t:TPL[t].dimensions.copy() for t in TPL}
GAP=2.0; colw=max(DIM[t].x for t in DIM)+GAP; order=["EAT","GROW","MOVE"]
CW=4*colw; CD=sum(DIM[t].z for t in order)+3*GAP

def dup(tpl,mat,loc,rot=(0,0,0),name="o"):
    o=bpy.data.objects.new(name,tpl.data); C().objects.link(o)
    o.location=loc; o.rotation_euler=rot; o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o
def coin(mat,loc,r,h):
    o=bpy.data.objects.new("k",CYL.data); C().objects.link(o)
    o.location=(loc[0],loc[1],loc[2]+h/2); o.scale=(r,r,h); o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o

def cradle_from(cutters, z):
    """union cutters -> voxel remesh -> sweep up -> boolean a block sized to the cutters."""
    xs=[]; ys=[]
    for d in cutters:
        for cnr in d.bound_box: xs.append(cnr[0]); ys.append(cnr[1])
    block_bounds=(min(xs)-6, min(ys)-6, max(xs)+6, max(ys)+6)
    bpy.ops.object.select_all(action='DESELECT')
    for d in cutters: d.select_set(True)
    bpy.context.view_layer.objects.active=cutters[0]; bpy.ops.object.join(); cr=bpy.context.view_layer.objects.active
    cr.data.remesh_voxel_size=VOX; bpy.ops.object.voxel_remesh()
    cps=[]
    for i in range(1,8):
        d=cr.copy(); d.data=cr.data.copy(); C().objects.link(d); d.location.z=z+i*5.0; cps.append(d)
    bpy.ops.object.select_all(action='DESELECT'); cr.select_set(True)
    for d in cps: d.select_set(True)
    bpy.context.view_layer.objects.active=cr; bpy.ops.object.join()
    cr.data.remesh_voxel_size=VOX; bpy.ops.object.voxel_remesh()
    sm=cr.modifiers.new("s",'SMOOTH'); sm.iterations=6; sm.factor=1.0; bpy.ops.object.modifier_apply(modifier="s")
    x0,y0,x1,y1=block_bounds
    bl=bpy.data.meshes.new("bl"); bo=bpy.data.objects.new("insert",bl); C().objects.link(bo)
    v=[(x0,y0,z),(x1,y0,z),(x1,y1,z),(x0,y1,z),(x0,y0,z+BH),(x1,y0,z+BH),(x1,y1,z+BH),(x0,y1,z+BH)]
    bl.from_pydata(v,[],[(0,1,2,3),(7,6,5,4),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); bl.update(); bl.materials.append(colmat("ins",(0.62,0.64,0.68),0.4))
    m=bo.modifiers.new("b",'BOOLEAN'); m.operation='DIFFERENCE'; m.object=cr; m.solver='EXACT'
    bpy.ops.object.select_all(action='DESELECT'); bo.select_set(True); bpy.context.view_layer.objects.active=bo
    bpy.ops.object.modifier_apply(modifier="b"); bpy.data.objects.remove(cr,do_unlink=True)
    return bo

def compartment(pl, ox, oy, z, cutters):
    mat=colmat("pm_"+pl,PCOL[pl])
    for cc in range(4):
        cx=(cc-1.5)*colw; y=oy+CD/2                            # CENTER the columns in the compartment
        for t in order:
            dup(TPL[t],mat,(ox+cx,y,z+DIM[t].y/2),rot=(math.radians(90),0,0),name=f"{pl}{cc}{t}")
            d=bpy.data.objects.new("c",TPL[t].data.copy()); C().objects.link(d)
            d.location=(ox+cx,y,z+DIM[t].y/2); d.rotation_euler=(math.radians(90),0,0)
            bpy.ops.object.select_all(action='DESELECT'); d.select_set(True); bpy.context.view_layer.objects.active=d
            bpy.ops.object.transform_apply(location=True,rotation=True,scale=True); cutters.append(d)
            y-=DIM[t].z+GAP

WALL=5; cellw,celld=CW+WALL, CD+WALL
Z1, Z2 = 120.0, 240.0                                          # exploded tier heights for the render
# ---- tier 1: players 1-4, one per cell of the 2x2 (compartments centred on cell centres) ----
cut1=[]
for i,pl in enumerate(PLAYERS[:4]):
    gx,gy=i%2,i//2; compartment(pl,(gx-0.5)*cellw,-gy*celld,Z1,cut1)
cradle_from(cut1, Z1)
# ---- tier 2: 5th player in cell(0,0); the 3 LEFTOVER cells get the stacks, spread EVENLY (water level) ----
cut2=[]
compartment(PLAYERS[4], -0.5*cellw, 0, Z2, cut2)
cradle_from(cut2, Z2)
free_cells=[(0.5*cellw,0),(-0.5*cellw,-celld),(0.5*cellw,-celld)]
def one_stack(kind,mat,n,pitch,r,x,y,z):
    for k in range(n):
        if kind=='food': dup(FOODT,mat,(x,y,z+k*pitch))
        elif kind=='disk': dup(MIN[["EAT","MOVE","GROW"][k%3]],mat,(x,y,z+0.3+k*pitch))
        else: coin(mat,(x,y,z+k*pitch),r,pitch)
stacks=[]; g=colmat("gold",GOLD)
for _ in range(12): stacks.append(('food',g,5,6.92,14))                       # 60 food -> 12 short stacks
for pl in PLAYERS:
    m=colmat("d_"+pl,PCOL[pl])
    for _ in range(3): stacks.append(('disk',m,4,7.5,18.5))                   # 60 disks -> 3x4/player
for pl in PLAYERS: stacks.append(('coin',colmat("f_"+pl,PCOL[pl]),9,2.2,18))  # 45 platforms -> 9/player
for pl in PLAYERS: stacks.append(('coin',colmat("t_"+pl,PCOL[pl]),3,5.0,15))  # 15 tokens -> 3/player
per=[0,0,0]
for si,(kind,mat,n,pitch,r) in enumerate(stacks):                             # round-robin -> even water level
    ci=si%3; ox,oy=free_cells[ci]; idx=per[ci]; per[ci]+=1
    gx,row=idx%4, idx//4
    one_stack(kind,mat,n,pitch,r, ox+(gx-1.5)*37, oy+CD/2-18-row*34, Z2)
# ---- tier 0: folded board ----
bd=bpy.data.meshes.new("bd"); bdo=bpy.data.objects.new("board",bd); C().objects.link(bdo)
h=135; bd.from_pydata([(-h,-h,0),(h,-h,0),(h,h,0),(-h,h,0),(-h,-h,14),(h,-h,14),(h,h,14),(-h,h,14)],[],
    [(0,1,2,3),(7,6,5,4),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); bd.update(); bd.materials.append(colmat("bdm",(0.16,0.28,0.33)))
bdo.location=(0,-celld/2,0)

cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=40; cam_d.clip_end=12000
cam=bpy.data.objects.new("Cam",cam_d); C().objects.link(cam); sc.camera=cam
tgt=Vector((0,-celld*0.6,150)); loc=Vector((-40,-celld*0.6-660,480))
cam.rotation_mode='QUATERNION'; cam.location=loc; cam.rotation_quaternion=(tgt-loc).to_track_quat('-Z','Y')
sc.render.image_settings.file_format='PNG'; sc.render.filepath=f"{FR}/final"
BW=2*cellw+2*WALL; BD_=2*celld+2*WALL; BOXH=14+40+34   # board + piece layer + flattened stacks
print(f"ASSEMBLED box ~ {BW:.0f} x {BD_:.0f} x {BOXH:.0f} mm ({BW/25.4:.1f} x {BD_/25.4:.1f} x {BOXH/25.4:.1f} in), tiers water-levelled")
bpy.ops.render.render(write_still=True); print("wrote",sc.render.filepath)
