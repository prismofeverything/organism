"""MESH-SPECIFIC insert: a solid block with each tipped piece's shape BOOLEANED out (dilated
~1.5mm for clearance) -> the thermoform negative. Pieces shown lifted above their exact pockets.
One player's 12-piece 3D compartment.  Cycles.
  ~/Downloads/.../blender -b --threads 4 --python build_cavity.py
"""
import bpy, os, math
from mathutils import Vector
P=os.path.dirname(os.path.abspath(__file__))
FR=os.environ.get("FR","/mnt/data/archive/organism-renders/packed"); os.makedirs(FR,exist_ok=True)
RESX=int(os.environ.get("RESX","1300")); RESY=int(os.environ.get("RESY","950"))
def _l(c): return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]),_l(c[1]),_l(c[2]),1.0)
bpy.ops.wm.read_factory_settings(use_empty=True); sc=bpy.context.scene
sc.render.engine="CYCLES"
try: sc.cycles.device='CPU'; sc.cycles.samples=24; sc.cycles.use_denoising=True
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
s=1000; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)],[],[(0,1,2,3)]); me.update()
me.materials.append(colmat("tab",(0.12,0.13,0.15),0.85))
def load(path):
    bpy.ops.wm.obj_import(filepath=path,up_axis='Z',forward_axis='Y')
    o=[x for x in bpy.context.selected_objects if x.type=='MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active=o
    bpy.ops.object.transform_apply(location=False,rotation=True,scale=True)
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    o.hide_render=True; o.location=(9000,9000,0); return o
TPL={t:load(f"{P}/out/{t}_sculpt_graft.obj") for t in ["EAT","MOVE","GROW"]}
DIM={t:TPL[t].dimensions.copy() for t in TPL}
RED=colmat("red",(0.83,0.30,0.34)); INS=colmat("insert",(0.55,0.57,0.62),0.4)

GAP=2.0; colw=max(DIM[t].x for t in DIM)+GAP; order=["EAT","GROW","MOVE"]
L3=sum(DIM[t].z for t in order)+3*GAP; H3=max(DIM[t].y for t in DIM)
def piece(t,name,cx,ytop):                                        # tipped: rot Rx90, rest on floor
    o=bpy.data.objects.new(name,TPL[t].data); C().objects.link(o)
    o.location=(cx,ytop,DIM[t].y/2); o.rotation_euler=(math.radians(90),0,0)
    o.data.materials.clear(); o.data.materials.append(RED); return o,DIM[t].z+GAP
placed=[]
for c in range(4):
    cx=c*colw; y=0
    for t in order:
        o,adv=piece(t,f"p{c}{t}",cx,y); placed.append((o,t,cx,y)); y-=adv

# insert block filling the compartment
x0,x1=-colw/2-3, 4*colw-colw/2+3; y0,y1=-L3-3, 4
block=bpy.data.meshes.new("block"); bo=bpy.data.objects.new("insert",block); C().objects.link(bo)
BH=30.0                                                           # BELOW piece tops -> pockets OPEN at the top
v=[(x0,y0,0),(x1,y0,0),(x1,y1,0),(x0,y1,0),(x0,y0,BH),(x1,y0,BH),(x1,y1,BH),(x0,y1,BH)]
block.from_pydata(v,[],[(0,1,2,3),(7,6,5,4),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); block.update()
block.materials.append(INS)

# boolean each piece (dilated by scaling ~1.05 about its own centre) OUT of the block
for o,t,cx,y in placed:
    cut=bpy.data.objects.new("cut",TPL[t].data.copy()); C().objects.link(cut)   # single-user for apply
    cut.location=o.location.copy(); cut.rotation_euler=o.rotation_euler.copy()
    ctr=o.location.copy(); cut.scale=(1.06,1.06,1.06)          # ~1.5mm clearance on a ~37mm piece
    bpy.ops.object.select_all(action='DESELECT'); cut.select_set(True)   # only the cutter
    bpy.context.view_layer.objects.active=cut; bpy.ops.object.transform_apply(location=False,rotation=True,scale=True)
    m=bo.modifiers.new("b",'BOOLEAN'); m.operation='DIFFERENCE'; m.object=cut; m.solver='EXACT'
    bpy.context.view_layer.objects.active=bo; bpy.ops.object.modifier_apply(modifier="b")
    bpy.data.objects.remove(cut,do_unlink=True)
    o.location=(9000,9000,0)                                    # move pieces off-frame -> show the pockets

cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=50; cam_d.clip_end=6000
cam=bpy.data.objects.new("Cam",cam_d); C().objects.link(cam); sc.camera=cam
W3=4*colw; tgt=Vector((W3*0.4,-L3/2,2)); loc=Vector((W3*0.4-10,-L3/2-40,330))   # high, looking down into pockets
cam.rotation_mode='QUATERNION'; cam.location=loc; cam.rotation_quaternion=(tgt-loc).to_track_quat('-Z','Y')
sc.render.image_settings.file_format='PNG'; sc.render.filepath=f"{FR}/cavity"
print(f"mesh-cavity insert {x1-x0:.0f} x {y1-y0:.0f} x {BH:.0f} mm, 12 booleaned pockets")
bpy.ops.render.render(write_still=True); print("wrote",sc.render.filepath)
