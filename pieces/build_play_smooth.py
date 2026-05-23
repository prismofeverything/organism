import bpy, sys, math, re, colorsys, os
from mathutils import Vector
sys.path.insert(0, "/home/youdonotexist/code/organism/pieces")
import organism_format as ogf
ROOT="/home/youdonotexist/code/organism"; P=f"{ROOT}/pieces"
FR="/tmp/play3"; os.makedirs(FR, exist_ok=True)
TURN_LIMIT=int(os.environ.get("TURN_LIMIT","0")); FPT=4
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
try: sc.eevee.taa_render_samples=6
except Exception: pass
sc.render.resolution_x=sc.render.resolution_y=560
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
cm={dd:colmat(f"z{dd}",ZONE.get(dd,(0.4,0.4,0.4)),0.85) for dd in set(dist.values())}
for s in G["board"]["spaces"]:
    x,y=P2(s); bpy.ops.mesh.primitive_cylinder_add(radius=26,depth=3,location=(x,y,0),vertices=24)
    bpy.context.active_object.data.materials.append(cm[dist[s]])
PM={p:colmat("pc"+str(i),hsl_rgb(c),0.4) for i,(p,c) in enumerate(G["colors"].items())}
foodmat=colmat("food",(0.93,0.88,0.6),0.4)
def imp(path,name):
    bpy.ops.wm.obj_import(filepath=path,forward_axis='NEGATIVE_Z',up_axis='Y')
    o=[x for x in bpy.context.selected_objects if x.type=='MESH'][0];o.name=name
    o.data.materials.clear();o.data.materials.append(colmat(name+"_d",(0.7,0.7,0.7)))
    o.hide_render=True;o.location=(9000,9000,0);return o
T={t:imp(f"{P}/{m}_connected.obj",t) for t,m in [("eat","EAT"),("move","MOVE"),("grow","GROW")]}
FOODT=imp(f"{P}/FOOD.obj","FOODT")
PSCALE=0.5; FSCALE=0.42
def topz(o): return max((o.matrix_world@v.co).z for v in o.data.vertices)
PHTOP={t:topz(T[t])*PSCALE for t in T}
tracks=ogf.track(G)
last_turn=(len(G["frames"])-1) if TURN_LIMIT==0 else min(TURN_LIMIT,len(G["frames"]))-1
def keyscale(o,frame,s): o.scale=(s,s,s); o.keyframe_insert("scale",frame=frame)
for tr in tracks:
    if tr["appear"]>last_turn: continue
    turns=sorted(t for t in tr["path"] if t<=last_turn)
    if not turns: continue
    t0,t1=turns[0],turns[-1]
    o=T[tr["type"]].copy();C().objects.link(o);o.hide_render=False
    o.material_slots[0].link='OBJECT';o.material_slots[0].material=PM[tr["player"]]
    for t in turns:
        x,y=P2(tr["path"][t][0]);o.location=(x,y,1.5);o.keyframe_insert("location",frame=t*FPT)
    keyscale(o,max(0,t0*FPT-FPT),0.001);keyscale(o,t0*FPT,PSCALE);keyscale(o,t1*FPT,PSCALE)
    if t1<last_turn: keyscale(o,t1*FPT+FPT,0.001)
    peg=PHTOP[tr["type"]]
    for k in range(3):
        if not any(tr["path"][t][1]>=k+1 for t in turns): continue
        fo=FOODT.copy();C().objects.link(fo);fo.hide_render=False
        fo.material_slots[0].link='OBJECT';fo.material_slots[0].material=foodmat
        for t in turns:
            x,y=P2(tr["path"][t][0]);fo.location=(x,y,peg+k*3.0);fo.keyframe_insert("location",frame=t*FPT)
            keyscale(fo,t*FPT, FSCALE if tr["path"][t][1]>=k+1 else 0.001)
        keyscale(fo,max(0,t0*FPT-FPT),0.001)
        if t1<last_turn: keyscale(fo,t1*FPT+FPT,0.001)
cd=bpy.data.cameras.new("C");cd.lens=46;cd.clip_end=20000
cam=bpy.data.objects.new("C",cd);C().objects.link(cam);sc.camera=cam
cam.location=(0,-430,650);cam.rotation_euler=(Vector((0,0,0))-Vector(cam.location)).to_track_quat('-Z','Y').to_euler()
sc.frame_start=0; sc.frame_end=last_turn*FPT+FPT; sc.render.fps=24
sc.render.filepath=f"{FR}/f"; sc.render.image_settings.file_format='PNG'
print("tracks:",len(tracks),"turns:",last_turn+1,"frames:",sc.frame_end+1)
bpy.ops.render.render(animation=True)
print("done")
