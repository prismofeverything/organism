"""One player's COMPARTMENT unit: 12 3D pieces TIPPED on their sides (4 columns x EAT+GROW+MOVE
end-to-end) in a walled compartment, + a separate walled sub-compartment for their 12 2D disks.
Placed by REAL mesh bounds (not centre points), so nothing pokes through the walls. Cycles.
  ~/Downloads/.../blender -b --threads 4 --python build_unit.py
"""
import bpy, os, math, colorsys
from mathutils import Vector
P=os.path.dirname(os.path.abspath(__file__))
FR=os.environ.get("FR","/mnt/data/archive/organism-renders/packed"); os.makedirs(FR,exist_ok=True)
RESX=int(os.environ.get("RESX","1300")); RESY=int(os.environ.get("RESY","950"))
def _l(c): return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]),_l(c[1]),_l(c[2]),1.0)
RED=(0.83,0.30,0.34); WALLC=(0.16,0.19,0.22)
bpy.ops.wm.read_factory_settings(use_empty=True); sc=bpy.context.scene
sc.render.engine="CYCLES"
try: sc.cycles.device='CPU'; sc.cycles.samples=28; sc.cycles.use_denoising=True
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
sun("K",4.4,50,35); sun("F",1.8,62,-120); sun("R",2.2,116,175)
def colmat(name,rgb,rough=0.45):
    m=bpy.data.materials.new(name); m.use_nodes=True
    b=m.node_tree.nodes["Principled BSDF"]; b.inputs["Base Color"].default_value=lin3(rgb); b.inputs["Roughness"].default_value=rough
    return m
me=bpy.data.meshes.new("T"); o=bpy.data.objects.new("T",me); C().objects.link(o)
s=1000; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)],[],[(0,1,2,3)]); me.update()
me.materials.append(colmat("tab",(0.12,0.13,0.15),0.85))
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
DIM={t:TPL[t].dimensions.copy() for t in TPL}                     # REAL mesh bounds
mat=colmat("red",RED)
def dup(tpl,name,loc,rot=(0,0,0)):
    o=bpy.data.objects.new(name,tpl.data); C().objects.link(o)
    o.location=loc; o.rotation_euler=rot; o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o

# ---- 3D compartment: 4 columns, each EAT->GROW->MOVE tipped on side, packed by real bounds ----
GAP=2.0; colw=max(DIM[t].x for t in DIM)+GAP
def tip(t,name,cx,ytop):                                          # tipped: occupies y[ytop-h, ytop], z[0,d]
    d=DIM[t].y; dup(TPL[t],name,(cx,ytop,d/2),rot=(math.radians(90),0,0)); return DIM[t].z+GAP
order=["EAT","GROW","MOVE"]
for c in range(4):
    cx=c*colw; y=0
    for t in order: y-=tip(t,f"p{c}{t}",cx,y)
W3=4*colw; L3=sum(DIM[t].z for t in order)+3*GAP; H3=max(DIM[t].y for t in DIM)
def wallbox(x0,y0,x1,y1,z0,z1,name):
    v=[(x0,y0,z0),(x1,y0,z0),(x1,y1,z0),(x0,y1,z0),(x0,y0,z1),(x1,y0,z1),(x1,y1,z1),(x0,y1,z1)]
    m=bpy.data.meshes.new(name); ob=bpy.data.objects.new(name,m); C().objects.link(ob)
    m.from_pydata(v,[],[(0,1,2,3),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); m.update(); m.materials.append(colmat(name+"_w",WALLC))
    for p in m.polygons: p.use_smooth=False
    return ob
TW=2.5
wallbox(-colw/2-TW,-L3-TW,W3-colw/2+TW,3+TW,-TW,0,"floor3")       # compartment floor
for xx in [-colw/2-TW,W3-colw/2]: wallbox(xx,-L3-TW,xx+TW,3,0,H3+3,"wx")
for yy in [-L3-TW,3]: wallbox(-colw/2-TW,yy,W3-colw/2,yy+TW,0,H3+3,"wy")

# ---- 2D sub-compartment beside it: 12 disks as 3 nested stacks of 4 (standing) ----
ox=W3+18
for i,t in enumerate(order):
    for k in range(4): dup(MIN[t],f"d{t}{k}",(ox+i*40,-30,0.3+k*7.5))
wallbox(ox-22,-72,ox+3*40-18,12,-TW,0,"floor2")
for xx in [ox-22,ox+3*40-18]: wallbox(xx,-72,xx+TW,12,0,44,"wx2")
for yy in [-72,12]: wallbox(ox-22,yy,ox+3*40-18,yy+TW,0,44,"wy2")

cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=52; cam_d.clip_end=6000
cam=bpy.data.objects.new("Cam",cam_d); C().objects.link(cam); sc.camera=cam
tgt=Vector((W3*0.6,-L3/2,15)); loc=Vector((W3*0.5-40,-L3-210,190))
cam.rotation_mode='QUATERNION'; cam.location=loc; cam.rotation_quaternion=(tgt-loc).to_track_quat('-Z','Y')
sc.render.image_settings.file_format='PNG'; sc.render.filepath=f"{FR}/unit"
print(f"3D compartment {W3:.0f} x {L3:.0f} x {H3:.0f} mm (tipped, real bounds)")
bpy.ops.render.render(write_still=True); print("wrote",sc.render.filepath)
