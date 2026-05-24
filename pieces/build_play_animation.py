import bpy, sys, math, re, colorsys, os
from mathutils import Vector
sys.path.insert(0, "/home/youdonotexist/code/organism/pieces")
import organism_format as ogf
ROOT="/home/youdonotexist/code/organism"; P=f"{ROOT}/pieces"
FR="/tmp/play_frames"; os.makedirs(FR, exist_ok=True)
G=ogf.load_ogf(f"{ROOT}/ogf/zach-dan-ryan.json"); LAY=ogf.layout(G)
maxr=max((math.hypot(x,y) for x,y in LAY.values()),default=1) or 1
SCALE=250.0/maxr
def P2(sp): x,y=LAY[sp]; return (x*SCALE,y*SCALE)
def hsl_rgb(s):
    h,sa,li=re.match(r"hsl\((\d+),(\d+)%?,(\d+)%?\)",s).groups()
    return colorsys.hls_to_rgb(float(h)/360,float(li)/100,float(sa)/100)
def lin(c): return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin3(rgb): return (lin(rgb[0]),lin(rgb[1]),lin(rgb[2]),1.0)
bpy.ops.wm.read_factory_settings(use_empty=True)
sc=bpy.context.scene
try: sc.render.engine='BLENDER_EEVEE_NEXT'
except Exception: sc.render.engine='BLENDER_EEVEE'
try: sc.eevee.taa_render_samples=12
except Exception: pass
sc.render.resolution_x=sc.render.resolution_y=720
w=bpy.data.worlds.new("W"); sc.world=w; w.use_nodes=True
w.node_tree.nodes["Background"].inputs[0].default_value=(0.16,0.17,0.2,1)
for nm,en,rot in [("S",4,(math.radians(55),math.radians(8),math.radians(30))),("S2",1.6,(math.radians(65),0,math.radians(-120)))]:
    d=bpy.data.lights.new(nm,'SUN'); d.energy=en; o=bpy.data.objects.new(nm,d); sc.collection.objects.link(o); o.rotation_euler=rot
def C(): return bpy.context.collection
def colmat(name,rgb,rough=0.5):
    m=bpy.data.materials.new(name); m.use_nodes=True; b=m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value=lin3(rgb); b.inputs["Roughness"].default_value=rough; return m
ZONE={0:(0.95,0.80,0.55),1:(0.80,0.46,0.52),2:(0.52,0.64,0.74),3:(0.55,0.72,0.52),4:(0.46,0.40,0.56)}
dist=ogf.ring_distances(G)
me=bpy.data.meshes.new("T");o=bpy.data.objects.new("T",me);C().objects.link(o)
me.from_pydata([(-1000,-1000,0),(1000,-1000,0),(1000,1000,0),(-1000,1000,0)],[],[(0,1,2,3)]);me.update()
me.materials.append(colmat("table",(0.11,0.12,0.15),0.95)); o.location=(0,0,-2)
cellmat={d:colmat(f"z{d}",ZONE.get(d,(0.4,0.4,0.4)),0.85) for d in set(dist.values())}
for s in G["board"]["spaces"]:
    x,y=P2(s); bpy.ops.mesh.primitive_cylinder_add(radius=26,depth=3,location=(x,y,0),vertices=24)
    bpy.context.active_object.data.materials.append(cellmat[dist[s]])
PM={p:colmat("pc"+str(i),hsl_rgb(c),0.4) for i,(p,c) in enumerate(G["colors"].items())}
def imp(name):
    bpy.ops.wm.obj_import(filepath=f"{P}/{name}.obj",forward_axis='NEGATIVE_Z',up_axis='Y')
    o=[x for x in bpy.context.selected_objects if x.type=='MESH'][0];o.name=name+"_T"
    o.data.materials.clear();o.data.materials.append(colmat(name+"_d",(0.7,0.7,0.7)))
    o.hide_render=True;o.location=(9000,9000,0);return o
T={t:imp(t) for t in ["EAT","MOVE","GROW"]};TYPE={"eat":"EAT","move":"MOVE","grow":"GROW"}
PSCALE=0.6;placed=[]
cd=bpy.data.cameras.new("C");cd.lens=46;cd.clip_end=20000
cam=bpy.data.objects.new("C",cd);C().objects.link(cam);sc.camera=cam
cam.location=(0,-430,650);cam.rotation_euler=(Vector((0,0,0))-Vector(cam.location)).to_track_quat('-Z','Y').to_euler()
frames=G["frames"]
for i,fr in enumerate(frames):
    for o in placed: bpy.data.objects.remove(o,do_unlink=True)
    placed=[]
    for player,etype,space,food in fr["elements"]:
        tm=T[TYPE[etype]];o=tm.copy();C().objects.link(o);o.hide_render=False
        x,y=P2(space);o.location=(x,y,1.5);o.scale=(PSCALE,PSCALE,PSCALE)
        o.material_slots[0].link='OBJECT';o.material_slots[0].material=PM[player]
        placed.append(o)
    sc.render.filepath=f"{FR}/f{i:04d}.png";bpy.ops.render.render(write_still=True)
print("rendered", len(frames), "frames")
