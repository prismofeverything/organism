"""SMOOTH CRADLE insert: instead of wrapping each tipped piece exactly (narrow traps), union the
pieces, COARSE voxel-remesh (smooths detail + merges into a continuous blob), then boolean that out
of the block -> a smooth continuous cradle the pieces nestle into and lift straight out of.
One player's 3D compartment. Cycles.
  ~/Downloads/.../blender -b --threads 4 --python build_cradle.py   (VOX=5 cradle coarseness)
"""
import bpy, os, math
from mathutils import Vector
P=os.path.dirname(os.path.abspath(__file__))
FR=os.environ.get("FR","/mnt/data/archive/organism-renders/packed"); os.makedirs(FR,exist_ok=True)
RESX=int(os.environ.get("RESX","1300")); RESY=int(os.environ.get("RESY","950")); VOX=float(os.environ.get("VOX","5.0"))
LIFT=os.environ.get("LIFT","0")=="1"                             # default: pieces RESTING in the cradle
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
RED=colmat("red",(0.83,0.30,0.34)); INS=colmat("insert",(0.62,0.64,0.68),0.4)
GAP=2.0; colw=max(DIM[t].x for t in DIM)+GAP; order=["EAT","GROW","MOVE"]
L3=sum(DIM[t].z for t in order)+3*GAP; H3=max(DIM[t].y for t in DIM)

placed=[]
def piece(t,name,cx,ytop):
    o=bpy.data.objects.new(name,TPL[t].data); C().objects.link(o)
    o.location=(cx,ytop,DIM[t].y/2); o.rotation_euler=(math.radians(90),0,0)
    o.data.materials.clear(); o.data.materials.append(RED); return o,DIM[t].z+GAP
for c in range(4):
    cx=c*colw; y=0
    for t in order:
        o,adv=piece(t,f"p{c}{t}",cx,y); placed.append((o,t,cx,y)); y-=adv

# ---- cradle tool: dup all pieces, join, COARSE voxel remesh (smooth + continuous), inflate a touch ----
dups=[]
for o,t,cx,y in placed:
    d=bpy.data.objects.new("cd",TPL[t].data.copy()); C().objects.link(d)
    d.location=o.location.copy(); d.rotation_euler=o.rotation_euler.copy()
    bpy.ops.object.select_all(action='DESELECT'); d.select_set(True); bpy.context.view_layer.objects.active=d
    bpy.ops.object.transform_apply(location=True,rotation=True,scale=True); dups.append(d)
bpy.ops.object.select_all(action='DESELECT')
for d in dups: d.select_set(True)
bpy.context.view_layer.objects.active=dups[0]; bpy.ops.object.join(); cradle=bpy.context.view_layer.objects.active
cradle.data.remesh_voxel_size=VOX
bpy.ops.object.voxel_remesh()                                    # -> smooth continuous blob
# SWEEP the blob straight UP and merge -> every cavity only widens upward (no overhang / no trap),
# so a piece lifts straight out. This removes the "carbonite" material above each bulge.
copies=[]
for i in range(1,8):
    d=cradle.copy(); d.data=cradle.data.copy(); C().objects.link(d); d.location.z=i*6.0; copies.append(d)
bpy.ops.object.select_all(action='DESELECT'); cradle.select_set(True)
for d in copies: d.select_set(True)
bpy.context.view_layer.objects.active=cradle; bpy.ops.object.join()
cradle.data.remesh_voxel_size=VOX; bpy.ops.object.voxel_remesh()  # merge swept copies into one solid
sm=cradle.modifiers.new("s",'SMOOTH'); sm.iterations=6; sm.factor=1.0; bpy.ops.object.modifier_apply(modifier="s")

# ---- block, boolean the smooth blob out -> continuous cradle ----
x0,x1=-colw/2-4,4*colw-colw/2+4; y0,y1=-L3-4,5; BH=22.0            # SHALLOW: pieces sink in, tops exposed
block=bpy.data.meshes.new("bl"); bo=bpy.data.objects.new("insert",block); C().objects.link(bo)
v=[(x0,y0,0),(x1,y0,0),(x1,y1,0),(x0,y1,0),(x0,y0,BH),(x1,y0,BH),(x1,y1,BH),(x0,y1,BH)]
block.from_pydata(v,[],[(0,1,2,3),(7,6,5,4),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); block.update(); block.materials.append(INS)
m=bo.modifiers.new("b",'BOOLEAN'); m.operation='DIFFERENCE'; m.object=cradle; m.solver='EXACT'
bpy.ops.object.select_all(action='DESELECT'); bo.select_set(True); bpy.context.view_layer.objects.active=bo
bpy.ops.object.modifier_apply(modifier="b")
bpy.data.objects.remove(cradle,do_unlink=True)
if LIFT:
    for o,t,cx,y in placed: o.location.z+=70                    # lift pieces to reveal the cradle

cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=52; cam_d.clip_end=6000
cam=bpy.data.objects.new("Cam",cam_d); C().objects.link(cam); sc.camera=cam
W3=4*colw; tgt=Vector((W3*0.4,-L3*0.55,10)); loc=Vector((W3*0.4-15,-L3-70,300))   # high angle into the cradle
cam.rotation_mode='QUATERNION'; cam.location=loc; cam.rotation_quaternion=(tgt-loc).to_track_quat('-Z','Y')
sc.render.image_settings.file_format='PNG'; sc.render.filepath=f"{FR}/cradle"
print(f"cradle insert {x1-x0:.0f} x {y1-y0:.0f} x {BH:.0f} mm, voxel {VOX}mm smooth-continuous")
bpy.ops.render.render(write_still=True); print("wrote",sc.render.filepath)
