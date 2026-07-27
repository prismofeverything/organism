"""FULL packed insert: 5 player 3D cradle compartments (tipped pieces) + a food compartment, on the
folded board, at real scale. ONE combined smooth cradle (union all pieces -> coarse voxel-remesh ->
sweep UP for lift-out -> boolean the insert block once). More-open cradle for handleability. Cycles.
  ~/Downloads/.../blender -b --threads 4 --python build_full.py
Env: VOX(cradle coarseness 7), BH(cradle depth 16), RESX/RESY, SAMPLES
"""
import bpy, os, math, colorsys
from mathutils import Vector
P=os.path.dirname(os.path.abspath(__file__))
FR=os.environ.get("FR","/mnt/data/archive/organism-renders/packed"); os.makedirs(FR,exist_ok=True)
RESX=int(os.environ.get("RESX","1500")); RESY=int(os.environ.get("RESY","1000")); SAMPLES=int(os.environ.get("SAMPLES","20"))
VOX=float(os.environ.get("VOX","7.0")); BH=float(os.environ.get("BH","16.0"))     # more open: coarser + shallower
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
sun("K",4.2,52,35); sun("F",1.8,62,-120); sun("R",2.0,116,175)
def colmat(name,rgb,rough=0.45):
    m=bpy.data.materials.new(name); m.use_nodes=True
    b=m.node_tree.nodes["Principled BSDF"]; b.inputs["Base Color"].default_value=lin3(rgb); b.inputs["Roughness"].default_value=rough
    return m
me=bpy.data.meshes.new("T"); o=bpy.data.objects.new("T",me); C().objects.link(o)
s=2000; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)],[],[(0,1,2,3)]); me.update()
me.materials.append(colmat("tab",(0.12,0.13,0.15),0.85))
def load(path,zup=True):
    if zup: bpy.ops.wm.obj_import(filepath=path,up_axis='Z',forward_axis='Y')
    else: bpy.ops.wm.obj_import(filepath=path,up_axis='Y',forward_axis='NEGATIVE_Z')
    o=[x for x in bpy.context.selected_objects if x.type=='MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active=o
    bpy.ops.object.transform_apply(location=False,rotation=True,scale=True)
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    if not o.data.materials: o.data.materials.append(colmat(o.name+"_b",(0.5,0.5,0.5)))   # slot for OBJECT override
    o.hide_render=True; o.location=(9000,9000,0); return o
TPL={t:load(f"{P}/out/{t}_sculpt_graft.obj") for t in ["EAT","MOVE","GROW"]}
FOODT=load(f"{P}/renders/food/FOOD_slip.obj",zup=False)
DIM={t:TPL[t].dimensions.copy() for t in TPL}
GAP=2.0; colw=max(DIM[t].x for t in DIM)+GAP; order=["EAT","GROW","MOVE"]
CW=4*colw; CD=sum(DIM[t].z for t in order)+3*GAP
pieces=[]; cutters=[]
def tipped(t,mat,ox,cx,ytop):                                    # real object + a cutter dup
    o=bpy.data.objects.new(f"p{len(pieces)}",TPL[t].data); C().objects.link(o)
    o.location=(ox+cx,ytop,DIM[t].y/2); o.rotation_euler=(math.radians(90),0,0)
    o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; pieces.append(o)   # per-object colour
    d=bpy.data.objects.new("c",TPL[t].data.copy()); C().objects.link(d)
    d.location=o.location.copy(); d.rotation_euler=o.rotation_euler.copy()
    bpy.ops.object.select_all(action='DESELECT'); d.select_set(True); bpy.context.view_layer.objects.active=d
    bpy.ops.object.transform_apply(location=True,rotation=True,scale=True); cutters.append(d)
    return DIM[t].z+GAP

# --- 6 sections (5 players + food) in a 3x2 grid on the board ---
WALL=4.0; cellw, celld = CW+WALL*2, CD+WALL*2
def cell_origin(gx,gy): return (gx-1)*cellw - CW/2 + colw/2, -((gy)*celld)
slots=[(0,0,"red"),(1,0,"yellow"),(2,0,"blue"),(0,1,"purple"),(1,1,"green"),(2,1,"food")]
for gx,gy,who in slots:
    ox,oy=cell_origin(gx,gy)
    if who=="food":
        g=colmat("gold",GOLD); FD=FOODT.dimensions
        fp=FD.x+3.0; n=0                                         # real bounds spacing -> NO overlap
        for si in range(8):                                     # 8 NESTED stacks of ~8 (food stacks into itself)
            sx,sy=si%4,si//4; bx,by=ox-1.5*fp+sx*fp, oy-20-sy*fp
            for k in range(8):
                if n>=60: break
                o=bpy.data.objects.new(f"f{n}",FOODT.data); C().objects.link(o)
                o.location=(bx,by,3+k*6.92)                     # 6.92mm nested pitch (measured)
                o.material_slots[0].link='OBJECT'; o.material_slots[0].material=g; pieces.append(o); n+=1
    else:
        mat=colmat("pm_"+who,PCOL[who])
        for cc in range(4):
            cx=cc*colw; y=oy
            for t in order: y-=tipped(t,mat,ox,cx,y)

# --- ONE combined cradle: join cutters -> voxel remesh -> sweep up -> boolean the insert block ---
bpy.ops.object.select_all(action='DESELECT')
for d in cutters: d.select_set(True)
bpy.context.view_layer.objects.active=cutters[0]; bpy.ops.object.join(); cr=bpy.context.view_layer.objects.active
cr.data.remesh_voxel_size=VOX; bpy.ops.object.voxel_remesh()
copies=[]
for i in range(1,9):
    d=cr.copy(); d.data=cr.data.copy(); C().objects.link(d); d.location.z=i*5.0; copies.append(d)
bpy.ops.object.select_all(action='DESELECT'); cr.select_set(True)
for d in copies: d.select_set(True)
bpy.context.view_layer.objects.active=cr; bpy.ops.object.join()
cr.data.remesh_voxel_size=VOX; bpy.ops.object.voxel_remesh()
sm=cr.modifiers.new("s",'SMOOTH'); sm.iterations=6; sm.factor=1.0; bpy.ops.object.modifier_apply(modifier="s")
# insert block spanning all cells
xs=[p.location.x for p in pieces]; ys=[p.location.y for p in pieces]
x0,x1=min(xs)-colw,max(xs)+colw; y0,y1=min(ys)-CD*0.6,max(ys)+40
INS=colmat("insert",(0.62,0.64,0.68),0.4)
bl=bpy.data.meshes.new("bl"); bo=bpy.data.objects.new("insert",bl); C().objects.link(bo)
v=[(x0,y0,0),(x1,y0,0),(x1,y1,0),(x0,y1,0),(x0,y0,BH),(x1,y0,BH),(x1,y1,BH),(x0,y1,BH)]
bl.from_pydata(v,[],[(0,1,2,3),(7,6,5,4),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); bl.update(); bl.materials.append(INS)
m=bo.modifiers.new("b",'BOOLEAN'); m.operation='DIFFERENCE'; m.object=cr; m.solver='EXACT'
bpy.ops.object.select_all(action='DESELECT'); bo.select_set(True); bpy.context.view_layer.objects.active=bo
bpy.ops.object.modifier_apply(modifier="b"); bpy.data.objects.remove(cr,do_unlink=True)
# folded board on the floor under the insert
bd=bpy.data.meshes.new("bd"); bdo=bpy.data.objects.new("board",bd); C().objects.link(bdo)
bd.from_pydata([(x0,y0,-16),(x1,y0,-16),(x1,y1,-16),(x0,y1,-16),(x0,y0,-2),(x1,y0,-2),(x1,y1,-2),(x0,y1,-2)],[],
               [(0,1,2,3),(7,6,5,4),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); bd.update(); bd.materials.append(colmat("bdm",(0.16,0.28,0.33)))

cx=(x0+x1)/2; cy=(y0+y1)/2
cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=42; cam_d.clip_end=9000
cam=bpy.data.objects.new("Cam",cam_d); C().objects.link(cam); sc.camera=cam
tgt=Vector((cx,cy+10,8)); loc=Vector((cx-30,cy-(y1-y0)*0.9,(x1-x0)*0.85))
cam.rotation_mode='QUATERNION'; cam.location=loc; cam.rotation_quaternion=(tgt-loc).to_track_quat('-Z','Y')
sc.render.image_settings.file_format='PNG'; sc.render.filepath=f"{FR}/full"
print(f"FULL insert {x1-x0:.0f} x {y1-y0:.0f} x {BH+16:.0f} mm ({(x1-x0)/25.4:.1f}x{(y1-y0)/25.4:.1f}x{(BH+16)/25.4:.1f}in), {len(pieces)} parts")
bpy.ops.render.render(write_still=True); print("wrote",sc.render.filepath)
