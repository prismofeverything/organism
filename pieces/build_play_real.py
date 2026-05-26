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
def imp(path,name,zup=True):
    # sculpt + new parametric grafts are Z-up files (written by trimesh / our Blender
    # export with up_axis='Z'); the old *_connected.obj was Y-up (Blender default).
    if zup: bpy.ops.wm.obj_import(filepath=path, up_axis='Z', forward_axis='Y')
    else:   bpy.ops.wm.obj_import(filepath=path, forward_axis='NEGATIVE_Z', up_axis='Y')
    o=[x for x in bpy.context.selected_objects if x.type=='MESH'][0];o.name=name
    o.data.materials.clear();o.data.materials.append(colmat(name+"_d",(0.7,0.7,0.7)))
    o.hide_render=True;o.location=(9000,9000,0);return o
def piece_path(name):
    """Sculpt graft if present, else fall back to the new parametric graft."""
    sculpt = f"{P}/out/{name}_sculpt_graft.obj"
    parametric = f"{P}/out/{name}_graft.obj"
    if os.path.exists(sculpt): print(f"  {name}: SCULPT  {os.path.basename(sculpt)}"); return sculpt
    print(f"  {name}: PARAMETRIC  {os.path.basename(parametric)}"); return parametric
T={t:imp(piece_path(m),t) for t,m in [("eat","EAT"),("move","MOVE"),("grow","GROW")]}
# FOOD is Y-up (Blender default export from build_food.py); pass zup=False so the
# importer rotates Y->Z and the peg stands upright.
FOODT=imp(os.environ.get("FOOD_OBJ",f"{P}/renders/food/FOOD_nosnap.obj"),"FOODT",zup=False); PSCALE=0.9; FSCALE=0.94
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
# Random-position camera with a FIBONACCI RHYTHM for the intervals between changes.
# Building the iteration pattern:
#   step 1: [5]
#   step 2: [5] + [5, 8]              = [5, 5, 8]
#   step 3: prev + [5, 8, 13]         = [5, 5, 8, 5, 8, 13]
#   step 4: prev + [5, 8, 13, 21]     = ...
# Keep iterating until the sum >= total turns. Then shuffle to get a varied,
# rhythmically-uneven sequence of camera hold-times (some short, some long).
# At each interval boundary, the camera eases (Bezier auto-clamped, Blender default)
# to a new random azimuth/elevation/distance — closer and further away alternate.
import random
random.seed(1234)
target=Vector((0,0,12))
n_turns=last_turn+1
FIB=[5, 8, 13, 21, 34, 55, 89, 144]                         # Fibonacci-ish, starting at 5
intervals=[]; step=1
while sum(intervals) < n_turns:
    intervals.extend(FIB[:step])
    step += 1
random.shuffle(intervals)
# turn-positions for the keyframes (cumulative sum, clamped to n_turns)
key_turns=[0]
for iv in intervals:
    nxt=key_turns[-1]+iv
    if nxt >= n_turns:
        key_turns.append(n_turns); break
    key_turns.append(nxt)
if key_turns[-1] != n_turns: key_turns.append(n_turns)
print(f"camera holds (turns): {[key_turns[i+1]-key_turns[i] for i in range(len(key_turns)-1)]}")
prev_q=None
for kt in key_turns:
    f=min(kt*FPT, sc.frame_end)
    az=math.radians(random.uniform(0, 360))
    el=math.radians(random.uniform(22, 62))                 # gimbal-safe band
    dist=random.uniform(280, 700)                           # closer and further
    tz=random.uniform(8, 20)
    target=Vector((0, 0, tz))
    loc=Vector((dist*math.cos(el)*math.cos(az),
                dist*math.cos(el)*math.sin(az),
                tz + dist*math.sin(el)))
    q=(target-loc).to_track_quat('-Z','Y')
    if prev_q is not None and prev_q.dot(q) < 0:
        q = -q
    prev_q = q
    cam.location=loc; cam.rotation_quaternion=q
    cam.keyframe_insert("location",       frame=f)
    cam.keyframe_insert("rotation_quaternion", frame=f)
# Blender's default Bezier auto-clamped interpolation handles the smooth ease.
sc.render.filepath=f"{FR}/f"; sc.render.image_settings.file_format='PNG'
print("tracks:",len(tracks),"turns:",last_turn+1,"frames:",sc.frame_end+1)
bpy.ops.render.render(animation=True)
print("done")
