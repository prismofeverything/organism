import bpy, sys, math, re, colorsys, os
from mathutils import Vector
sys.path.insert(0, "/home/youdonotexist/code/organism/pieces")
import organism_format as ogf
ROOT="/home/youdonotexist/code/organism"; P=f"{ROOT}/pieces"; ART="/home/youdonotexist/Downloads/organism/prototype"
FR="/tmp/play4"; os.makedirs(FR, exist_ok=True)
TURN_LIMIT=int(os.environ.get("TURN_LIMIT","0")); FPT=4; SCALE=43.0
G=ogf.load_ogf(f"{ROOT}/ogf/zach-dan-ryan.json"); LOC=ogf.board_locations(G)
BROT=math.radians(30)   # printed art lattice is 30deg off board.cljc tau/12 beam phase
def P2(sp):
    x,y=LOC[sp]
    xr=x*math.cos(BROT)-y*math.sin(BROT); yr=x*math.sin(BROT)+y*math.cos(BROT)
    return (xr*SCALE, yr*SCALE)
def hsl_rgb(s):
    h,sa,li=re.match(r"hsl\((\d+),(\d+)%?,(\d+)%?\)",s).groups()
    return colorsys.hls_to_rgb(float(h)/360,float(li)/100,float(sa)/100)
def lin(c): return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin3(rgb): return (lin(rgb[0]),lin(rgb[1]),lin(rgb[2]),1.0)
bpy.ops.wm.read_factory_settings(use_empty=True); sc=bpy.context.scene
try: sc.render.engine='BLENDER_EEVEE_NEXT'
except Exception: sc.render.engine='BLENDER_EEVEE'
try: sc.eevee.taa_render_samples=6
except Exception: pass
sc.render.resolution_x=sc.render.resolution_y=600
w=bpy.data.worlds.new("W"); sc.world=w; w.use_nodes=True; w.node_tree.nodes["Background"].inputs[0].default_value=(0.45,0.46,0.5,1)
for nm,en,rot in [("S",4.2,(math.radians(52),math.radians(8),math.radians(30))),("S2",1.5,(math.radians(64),0,math.radians(-120)))]:
    d=bpy.data.lights.new(nm,'SUN'); d.energy=en; o=bpy.data.objects.new(nm,d); sc.collection.objects.link(o); o.rotation_euler=rot
def C(): return bpy.context.collection
def colmat(name,rgb,rough=0.45):
    m=bpy.data.materials.new(name); m.use_nodes=True; b=m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value=lin3(rgb); b.inputs["Roughness"].default_value=rough; return m
def imgmat(name,path):
    m=bpy.data.materials.new(name); m.use_nodes=True; nt=m.node_tree; b=nt.nodes["Principled BSDF"]
    t=nt.nodes.new("ShaderNodeTexImage"); t.image=bpy.data.images.load(path)
    nt.links.new(t.outputs["Color"],b.inputs["Base Color"]); b.inputs["Roughness"].default_value=0.9
    nt.links.new(t.outputs["Alpha"],b.inputs["Alpha"])
    for a,v in (("blend_method",'CLIP'),("surface_render_method",'DITHERED')):
        try: setattr(m,a,v)
        except Exception: pass
    return m
def plane(name,sz,mat,loc):
    me=bpy.data.meshes.new(name);o=bpy.data.objects.new(name,me);C().objects.link(o);s=sz/2
    me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)],[],[(0,1,2,3)])
    uv=me.uv_layers.new()
    for lp in me.loops: uv.data[lp.index].uv=[(0,0),(1,0),(1,1),(0,1)][lp.vertex_index]
    me.update();me.materials.append(mat);o.location=loc;return o
plane("Table",2400,colmat("table",(0.3,0.31,0.34),0.95),(0,0,-2))
plane("Board",540,imgmat("Board",f"{P}/board_hex_2000.png"),(0,0,0))
PM={p:colmat("pc"+str(i),hsl_rgb(c)) for i,(p,c) in enumerate(G["colors"].items())}
foodmat=colmat("food",(0.95,0.9,0.62),0.4)
def imp(path,name):
    bpy.ops.wm.obj_import(filepath=path,forward_axis='NEGATIVE_Z',up_axis='Y')
    o=[x for x in bpy.context.selected_objects if x.type=='MESH'][0];o.name=name
    o.data.materials.clear();o.data.materials.append(colmat(name+"_d",(0.7,0.7,0.7)))
    o.hide_render=True;o.location=(9000,9000,0);return o
T={t:imp(f"{P}/{m}_connected.obj",t) for t,m in [("eat","EAT"),("move","MOVE"),("grow","GROW")]}
FOODT=imp(os.environ.get("FOOD_OBJ",f"{P}/renders/food/FOOD_nosnap.obj"),"FOODT"); PSCALE=0.9; FSCALE=0.94
def topz(o): return max((o.matrix_world@v.co).z for v in o.data.vertices)
PHTOP={t:(topz(T[t])-4.3)*PSCALE for t in T}   # plateau (peg base), so the food's socket swallows the peg instead of perching on its tip
tracks=ogf.track(G)
last_turn=(len(G["frames"])-1) if TURN_LIMIT==0 else min(TURN_LIMIT,len(G["frames"]))-1
def ks(o,fr,s): o.scale=(s,s,s); o.keyframe_insert("scale",frame=fr)
for tr in tracks:
    if tr["appear"]>last_turn: continue
    turns=sorted(t for t in tr["path"] if t<=last_turn)
    if not turns: continue
    t0,t1=turns[0],turns[-1]
    o=T[tr["type"]].copy();C().objects.link(o);o.hide_render=False
    o.material_slots[0].link='OBJECT';o.material_slots[0].material=PM[tr["player"]]
    for t in turns:
        x,y=P2(tr["path"][t][0]);o.location=(x,y,1.0);o.keyframe_insert("location",frame=t*FPT)
    ks(o,max(0,t0*FPT-FPT),0.001);ks(o,t0*FPT,PSCALE);ks(o,t1*FPT,PSCALE)
    if t1<last_turn: ks(o,t1*FPT+FPT,0.001)
    peg=PHTOP[tr["type"]]
    for k in range(3):
        if not any(tr["path"][t][1]>=k+1 for t in turns): continue
        fo=FOODT.copy();C().objects.link(fo);fo.hide_render=False
        fo.material_slots[0].link='OBJECT';fo.material_slots[0].material=foodmat
        for t in turns:
            x,y=P2(tr["path"][t][0]);fo.location=(x,y,1.0+peg+k*6.4);fo.keyframe_insert("location",frame=t*FPT)
            ks(fo,t*FPT, FSCALE if tr["path"][t][1]>=k+1 else 0.001)
        ks(fo,max(0,t0*FPT-FPT),0.001)
        if t1<last_turn: ks(fo,t1*FPT+FPT,0.001)
cd=bpy.data.cameras.new("C");cd.lens=38;cd.clip_end=20000
cam=bpy.data.objects.new("C",cd);C().objects.link(cam);sc.camera=cam
cam.rotation_mode='QUATERNION'
sc.frame_start=0; sc.frame_end=last_turn*FPT+FPT; sc.render.fps=24
# Orbiting camera: 1.5 revolutions + elevation sweep 14..86deg (2 cycles) to read depth
N=sc.frame_end; target=Vector((0,0,12)); dist=500.0; orbits=1.5; ecyc=2.0
fr_set=list(range(0,N+1,10))
if fr_set[-1]!=N: fr_set.append(N)
for f in fr_set:
    t=f/N if N else 0.0
    az=math.radians(360*orbits*t); el=math.radians(50+36*math.sin(2*math.pi*ecyc*t))
    loc=Vector((dist*math.cos(el)*math.cos(az), dist*math.cos(el)*math.sin(az), 12+dist*math.sin(el)))
    cam.location=loc; cam.rotation_quaternion=(target-loc).to_track_quat('-Z','Y')
    cam.keyframe_insert("location",frame=f); cam.keyframe_insert("rotation_quaternion",frame=f)
sc.render.filepath=f"{FR}/f"; sc.render.image_settings.file_format='PNG'
print("tracks:",len(tracks),"turns:",last_turn+1,"frames:",sc.frame_end+1)
bpy.ops.render.render(animation=True)
print("done")
