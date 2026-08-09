"""build_reveal.py — ending REVEAL beat: after the swoop drifts into nothing but red, the three action
word/symbol pairs (EAT/MOVE/GROW — word on top, symbol centered below, in a row) come UP through the red
surface toward the viewer (in Z/depth) and CLARIFY from red into white IN PLACE — orthographic camera so
there is no sliding, no spreading, no size-pop. They surface slowly, one by one, then float gently.

Standalone Cycles scene (red-to-red cut from the unbox swoop). Env knobs:
  RESX RESY SAMPLES FIRST LAST FR(out)  FONT SYMDIR  WSIZE SYMD GAP COLGAP  LEAD STAGGER FADE HOLD
  FLOATZ FLOATX ZOOM EMIT FOGDENS
Run: blender -b --python build_reveal.py
"""
import bpy, os, math, mathutils as mu

RESX=int(os.environ.get("RESX","640")); RESY=int(os.environ.get("RESY","360"))
SAMPLES=int(os.environ.get("SAMPLES","32"))
FIRST=int(os.environ.get("FIRST","0"))
FR=os.environ.get("FR","/mnt/data/archive/organism-renders/reveal")
HERE=os.path.dirname(os.path.abspath(__file__))
FONT=os.environ.get("FONT",os.path.join(HERE,"inputs/JTEnergyVF.ttf"))
SYMDIR=os.environ.get("SYMDIR",os.path.join(HERE,"inputs/sym_png"))

# timing (24fps)
LEAD=int(os.environ.get("LEAD","16")); STAGGER=int(os.environ.get("STAGGER","20"))
FADE=int(os.environ.get("FADE","40"))          # slow clarify per element
SETTLE=int(os.environ.get("SETTLE","72"))      # gradual "lock in": float decays over this (longer than FADE)
LOCKMIN=float(os.environ.get("LOCKMIN","0.13"))# residual float once locked (never ENTIRELY still)
HOLD=int(os.environ.get("HOLD","50")); NEL=6
LAST=int(os.environ.get("LAST", str(LEAD+(NEL-1)*STAGGER+FADE+HOLD)))

# look / layout
WSIZE=float(os.environ.get("WSIZE","0.62")); SYMD=float(os.environ.get("SYMD","3.5"))
GAP=float(os.environ.get("GAP","0.16")); COLGAP=float(os.environ.get("COLGAP","0.42"))
MARGIN=float(os.environ.get("MARGIN","0.6")); EMIT=float(os.environ.get("EMIT","1.1"))
ZOOM=float(os.environ.get("ZOOM","0.87"))      # ortho_scale end factor: continuous slow push-in, continues
                                               # the swoop's approach (LINEAR from frame 0 = no ease-in stop)
# gentle float (only X/Z read under an ortho cam; keep small so it never reads as a directional slide)
FLOATZ=float(os.environ.get("FLOATZ","0.03")); FLOATX=float(os.environ.get("FLOATX","0.017"))
FTZ=3.6; FTX=4.7

def smoother(x): x=max(0.0,min(1.0,x)); return x*x*x*(x*(x*6-15)+10)

# ---- scene reset ----
sc=bpy.context.scene
for o in list(bpy.data.objects): bpy.data.objects.remove(o,do_unlink=True)
sc.render.engine='CYCLES'; sc.cycles.device='CPU'; sc.cycles.samples=SAMPLES
sc.render.resolution_x=RESX; sc.render.resolution_y=RESY; sc.render.fps=24
sc.render.image_settings.file_format='PNG'; sc.frame_start=FIRST; sc.frame_end=LAST
sc.render.filepath=os.path.join(FR,"r")
try: sc.view_settings.view_transform='Standard'
except Exception: pass
world=bpy.data.worlds.new("w"); sc.world=world; world.use_nodes=True
world.node_tree.nodes["Background"].inputs[0].default_value=(0,0,0,1)

def _clear(nt):
    for n in list(nt.nodes): nt.nodes.remove(n)
def _fcurves(owner):
    ad=getattr(owner,"animation_data",None)
    if not ad or not ad.action: return []
    act=ad.action
    if hasattr(act,"fcurves") and len(getattr(act,"fcurves",[])): return list(act.fcurves)
    fcs=[]
    for layer in getattr(act,"layers",[]):
        for strip in getattr(layer,"strips",[]):
            try:
                cb=strip.channelbag(ad.action_slot)
                if cb: fcs.extend(cb.fcurves)
            except Exception: pass
    return fcs
def set_lin(owner):
    for fc in _fcurves(owner):
        for k in fc.keyframe_points: k.interpolation='LINEAR'

# ---- red field (a plane facing the camera = the surface of the red, seen face-on) ----
bpy.ops.mesh.primitive_plane_add(size=90, location=(0,4.0,0))
bg=bpy.context.active_object; bg.rotation_euler=(math.pi/2,0,0); bg.name="red"
m=bpy.data.materials.new("red_m"); bg.data.materials.append(m); m.use_nodes=True
nt=m.node_tree; _clear(nt)
tc=nt.nodes.new("ShaderNodeTexCoord"); sep=nt.nodes.new("ShaderNodeSeparateXYZ")
nt.links.new(tc.outputs["Generated"],sep.inputs[0])
ramp=nt.nodes.new("ShaderNodeValToRGB")     # gentle vertical gradient for depth (no waterline)
ramp.color_ramp.elements[0].position=0.0; ramp.color_ramp.elements[0].color=(0.34,0.040,0.075,1)
ramp.color_ramp.elements[1].position=1.0; ramp.color_ramp.elements[1].color=(0.55,0.066,0.120,1)
nt.links.new(sep.outputs["Y"],ramp.inputs[0])
em=nt.nodes.new("ShaderNodeEmission"); nt.links.new(ramp.outputs["Color"],em.inputs["Color"]); em.inputs["Strength"].default_value=1.0
out=nt.nodes.new("ShaderNodeOutputMaterial"); nt.links.new(em.outputs[0],out.inputs["Surface"])

# ---- faint red atmosphere (submerged murk) ----
bpy.ops.mesh.primitive_cube_add(size=20, location=(0,1.5,0))
vol=bpy.context.active_object; vol.name="fog"
vm=bpy.data.materials.new("fog_m"); vol.data.materials.append(vm); vm.use_nodes=True
vnt=vm.node_tree; _clear(vnt)
pv=vnt.nodes.new("ShaderNodeVolumePrincipled"); pv.inputs["Color"].default_value=(0.58,0.055,0.105,1)
pv.inputs["Density"].default_value=float(os.environ.get("FOGDENS","0.02"))
vo=vnt.nodes.new("ShaderNodeOutputMaterial"); vnt.links.new(pv.outputs[0],vo.inputs["Volume"])

font=bpy.data.fonts.load(FONT)

def fade_mat(name,image=None):
    """white emission gated by a keyframable FADE (0=transparent, red shows -> 1=solid white). Returns (mat, fade_node)."""
    m=bpy.data.materials.new(name); m.use_nodes=True; nt=m.node_tree; _clear(nt)
    e=nt.nodes.new("ShaderNodeEmission"); e.inputs["Color"].default_value=(1,1,1,1); e.inputs["Strength"].default_value=EMIT
    tr=nt.nodes.new("ShaderNodeBsdfTransparent")
    fade=nt.nodes.new("ShaderNodeValue"); fade.outputs[0].default_value=0.0; fade.label="fade"
    mix=nt.nodes.new("ShaderNodeMixShader")
    nt.links.new(tr.outputs[0],mix.inputs[1]); nt.links.new(e.outputs[0],mix.inputs[2])
    o=nt.nodes.new("ShaderNodeOutputMaterial"); nt.links.new(mix.outputs[0],o.inputs["Surface"])
    if image is not None:
        tex=nt.nodes.new("ShaderNodeTexImage"); tex.image=image; tex.interpolation='Cubic'
        mul=nt.nodes.new("ShaderNodeMath"); mul.operation='MULTIPLY'
        nt.links.new(tex.outputs["Alpha"],mul.inputs[0]); nt.links.new(fade.outputs[0],mul.inputs[1])
        nt.links.new(mul.outputs[0],mix.inputs[0])
    else:
        nt.links.new(fade.outputs[0],mix.inputs[0])
    return m,fade

def word_obj(txt):
    cu=bpy.data.curves.new(txt,'FONT'); cu.body=txt; cu.font=font
    cu.align_x='CENTER'; cu.align_y='CENTER'; cu.size=WSIZE
    ob=bpy.data.objects.new(txt,cu); bpy.context.collection.objects.link(ob)
    ob.rotation_euler=(math.pi/2,0,0); m,fade=fade_mat(txt+"_m"); cu.materials.append(m)
    return ob,fade
def sym_obj(name,diam):
    img=bpy.data.images.load(os.path.join(SYMDIR,name+".png")); iw,ih=img.size
    if iw>=ih: w=diam; h=diam*ih/iw
    else:      h=diam; w=diam*iw/ih
    bpy.ops.mesh.primitive_plane_add(size=1); ob=bpy.context.active_object; ob.name=name+"_sym"
    ob.rotation_euler=(math.pi/2,0,0); ob.scale=(w,h,1)
    m,fade=fade_mat(name+"_sm",image=img); ob.data.materials.append(m)
    return ob,fade

# ---- measure letter width -> symbol diameter = SYMD letters ----
def word_width(txt):
    ob,_=word_obj(txt); bpy.context.view_layer.update()
    dg=bpy.context.evaluated_depsgraph_get(); w=ob.evaluated_get(dg).dimensions.x
    bpy.data.objects.remove(ob,do_unlink=True); return w
ww={w:word_width(w) for w in ["EAT","MOVE","GROW"]}
letter=(ww["EAT"]/3+ww["MOVE"]/4+ww["GROW"]/4)/3.0
SYMW=SYMD*letter; maxword=max(ww.values()); colW=max(maxword,SYMW)
capH=0.70*WSIZE
_a=bpy.data.images.load(os.path.join(SYMDIR,"eat.png")); _iw,_ih=_a.size
symH=SYMW if _ih>=_iw else SYMW*_ih/_iw
total=capH+GAP+symH; WZ=total/2-capH/2; SZ=-total/2+symH/2
spacing=colW+COLGAP; COLX=[-spacing,0.0,spacing]

# ---- ORTHOGRAPHIC camera (no perspective -> no spread/size-pop); subtle push-in via ortho_scale ----
needX=spacing+colW/2+MARGIN
cam_d=bpy.data.cameras.new("cam"); cam=bpy.data.objects.new("cam",cam_d)
bpy.context.collection.objects.link(cam); sc.camera=cam
cam_d.type='ORTHO'; cam_d.clip_start=0.01; cam_d.clip_end=80
cam.location=(0,-9.0,0.0); cam.rotation_euler=(math.pi/2,0,0)
cam_d.ortho_scale=2*needX;      cam_d.keyframe_insert("ortho_scale",frame=FIRST)
cam_d.ortho_scale=2*needX*ZOOM; cam_d.keyframe_insert("ortho_scale",frame=LAST)
set_lin(cam_d)

# ---- build: fixed rest positions; clarify red->white in place (slow, one by one) + gentle float ----
plan=[("EAT",0,WZ,"word"),("eat",0,SZ,"sym"),
      ("MOVE",1,WZ,"word"),("move",1,SZ,"sym"),
      ("GROW",2,WZ,"word"),("grow",2,SZ,"sym")]
for i,(label,col,restZ,kind) in enumerate(plan):
    x=COLX[col]
    ob,fade=word_obj(label) if kind=="word" else sym_obj(label,SYMW)
    t0=FIRST+LEAD+i*STAGGER; ph=i*1.7
    for fr in range(FIRST,LAST+1):
        f=smoother((fr-t0)/FADE) if fr>=t0 else 0.0
        fade.outputs[0].default_value=f; fade.outputs[0].keyframe_insert("default_value",frame=fr)
        # gradual lock-in: float at full amplitude when it surfaces, decaying to LOCKMIN as it settles
        lock=smoother((fr-t0)/SETTLE) if fr>=t0 else 0.0
        amp=LOCKMIN+(1.0-LOCKMIN)*(1.0-lock)
        fx=amp*FLOATX*math.sin(2*math.pi*(fr/24.0)/FTX+ph)
        fz=amp*FLOATZ*math.sin(2*math.pi*(fr/24.0)/FTZ+ph*1.3)
        ob.location=(x+fx,0.0,restZ+fz); ob.keyframe_insert("location",frame=fr)
    set_lin(ob); set_lin(fade.id_data)

# ---- render ----
FRAME=os.environ.get("FRAME")
if FRAME:
    sc.frame_set(int(FRAME)); sc.render.filepath=os.path.join(FR,"single_%04d"%int(FRAME))
    bpy.ops.render.render(write_still=True)
elif not os.environ.get("NORENDER"):
    bpy.ops.render.render(animation=True)
print("reveal done LAST=%d letter=%.3f SYMW=%.3f spacing=%.3f ortho=%.2f WZ=%.3f SZ=%.3f"%(
    LAST,letter,SYMW,spacing,2*needX,WZ,SZ))
