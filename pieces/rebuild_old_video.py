import bpy, os, mathutils
from collections import defaultdict
sc=bpy.context.scene
V=mathutils.Vector

# ---- 1. relink missing board textures from backup + downscale working copies to 2048 (originals untouched) ----
SRCH=["/mnt/data/home/youdonotexist/Downloads","/mnt/data/home/youdonotexist/Downloads/organism board",
      "/home/youdonotexist/Downloads","/home/youdonotexist/Downloads/organism board","/home/youdonotexist/Downloads/organism/prototype"]
def find(base):
    for d in SRCH:
        c=os.path.join(d,base)
        if os.path.exists(c): return c
    return None
rel=0
for img in list(bpy.data.images):
    if img.source=='FILE' and img.size[0]==0:
        c=find(os.path.basename((img.filepath or img.name).replace("\\","/")))
        if c: img.filepath=c;
        try: img.reload()
        except Exception: pass
        if c: rel+=1
for img in bpy.data.images:
    w,h=img.size
    if max(w,h)>2048:
        f=2048/max(w,h); img.scale(max(1,int(w*f)),max(1,int(h*f)))
print("relinked",rel)

# ---- 2. import new sculpts as shared meshes (clean origin at base), one dummy slot for object-colour override ----
NP="/home/youdonotexist/code/organism/pieces/out"
newmesh={}; Hnew={}; Wnew={}
for t in ["EAT","MOVE","GROW"]:
    pre=set(bpy.data.objects)
    bpy.ops.wm.obj_import(filepath=f"{NP}/{t}_sculpt_graft.obj", up_axis='Z', forward_axis='Y')
    o=[x for x in bpy.data.objects if x not in pre and x.type=='MESH'][0]
    bpy.ops.object.select_all(action='DESELECT'); o.select_set(True); bpy.context.view_layer.objects.active=o
    bpy.ops.object.transform_apply(location=True,rotation=True,scale=True)
    zs=[v.co.z for v in o.data.vertices]; Hnew[t]=max(zs)-min(zs)
    xs=[v.co.x for v in o.data.vertices]; ys=[v.co.y for v in o.data.vertices]
    Wnew[t]=max(max(xs)-min(xs), max(ys)-min(ys))            # real footprint (mm)
    if not o.data.materials: o.data.materials.append(bpy.data.materials.new(t+"_slot"))
    newmesh[t]=o.data
    o.hide_render=True; o.hide_viewport=True; o.location=(9000,9000,0)

# ---- 3. group old bodies by design, map design->type by height order ----
sc.frame_set(400)
def wbb(o):
    cs=[o.matrix_world@V(c) for c in o.bound_box]
    return (max(c.z for c in cs)-min(c.z for c in cs))
bodies=defaultdict(list)
for o in bpy.data.objects:
    if o.type=='MESH' and 'quad sphere' in o.name.lower(): bodies[len(o.data.vertices)].append(o)
dh={vc:wbb(bodies[vc][0]) for vc in bodies}
byh=sorted(bodies.keys(), key=lambda vc:dh[vc])
MAP={byh[0]:"GROW", byh[1]:"EAT", byh[2]:"MOVE"}
print("MAP(vc->type,worldH):", {vc:(MAP[vc],round(dh[vc],2)) for vc in bodies})

# pre-sample each body's FULL-POSE scale (max over the animation) by stepping frames (avoids the changed fcurve API)
_allb=[o for objs in bodies.values() for o in objs]
fpmap={o:[abs(o.scale[i]) for i in range(3)] for o in _allb}
for _f in [1,80,160,240,320,400,480,560]:
    sc.frame_set(_f)
    for o in _allb:
        for i in range(3): fpmap[o][i]=max(fpmap[o][i], abs(o.scale[i]))
sc.frame_set(400)

# ---- faithful real-proportion scale: ONE factor for ALL pieces so MOVE:EAT:GROW come through at their
# TRUE ratios (55:44:34.8), not the old per-creature heights. Overall size anchored so the set's mean
# footprint matches the old creatures' (~3.70), i.e. they keep sitting on their cells the same way. ----
def wfp(o):
    cs=[o.matrix_world@V(c) for c in o.bound_box]
    return max(max(c.x for c in cs)-min(c.x for c in cs), max(c.y for c in cs)-min(c.y for c in cs))
OLD_FP=sorted(wfp(bodies[vc][0]) for vc in bodies)[len(bodies)//2]     # median old-body world footprint
S=OLD_FP/(sum(Wnew.values())/len(Wnew))
print("FAITHFUL S=%.4f old_fp=%.2f -> heights %s footprints %s"%(
    S,OLD_FP,{t:round(Hnew[t]*S,2) for t in Hnew},{t:round(Wnew[t]*S,2) for t in Wnew}))

# ---- 4. swap: parent new sculpt to each old body, align base+height, object colour, hide old ----
made=0; Nmap={}
for vc,objs in bodies.items():
    T=MAP[vc]
    for O in objs:
        bb=O.bound_box
        lcx=(max(c[0] for c in bb)+min(c[0] for c in bb))/2
        lcy=(max(c[1] for c in bb)+min(c[1] for c in bb))/2
        lz =min(c[2] for c in bb)
        lh =max(c[2] for c in bb)-min(c[2] for c in bb)      # old mesh LOCAL height
        # FULL-POSE scale of O (max over its keyframes) -> the non-uniform "shaping" scale, separate from the 0->full appear anim
        fp=list(fpmap[O])
        if max(fp)<1e-4: continue                            # piece never visible -> skip
        fp=[max(v,1e-3) for v in fp]
        uk=S                                                 # ONE faithful scale for every piece -> true real proportions (not old per-creature heights)
        N=bpy.data.objects.new(O.name+"_"+T, newmesh[T]); sc.collection.objects.link(N)
        N.parent=O; N.matrix_parent_inverse=mathutils.Matrix.Identity(4)
        N.location=(lcx,lcy,lz); N.rotation_euler=(0,0,0)
        N.scale=(uk/fp[0], uk/fp[1], uk/fp[2])               # divide out O's non-uniform full-pose scale (appear/disappear still inherited via parenting)
        mat=(O.material_slots[0].material if O.material_slots and O.material_slots[0].material else (O.data.materials[0] if O.data.materials else None))
        if mat and N.material_slots:
            N.material_slots[0].link='OBJECT'; N.material_slots[0].material=mat
        O.hide_render=True
        Nmap[O]=N                                            # old body -> new piece (food parents to this)
        made+=1
print("swapped_pieces",made)

# ---- 5. FOOD: real meniscus on each piece that carried a cup. Parent to the PIECE, NOT the cup: a cup's
# scale animates to 0 on appear/disappear and the old matrix trick divided by it -> food shot to infinity
# (giant grey) and inherited the cup's 180deg flip (upside-down). Parented to the upright piece the food
# sits rigidly on the connector, centered + upright + tracking movement; ONLY its appear/disappear is
# driven from the cup's own scale animation. ----
FOODOBJ="/home/youdonotexist/code/organism/pieces/renders/food/FOOD_nosnap.obj"
pre=set(bpy.data.objects)
bpy.ops.wm.obj_import(filepath=FOODOBJ, up_axis='Y', forward_axis='NEGATIVE_Z')   # meniscus -> Z-up
fo=[x for x in bpy.data.objects if x not in pre and x.type=='MESH'][0]
bpy.ops.object.select_all(action='DESELECT'); fo.select_set(True); bpy.context.view_layer.objects.active=fo
bpy.ops.object.transform_apply(location=False,rotation=True,scale=True)   # keep NATIVE origin (base=socket seat), Z-up
if not fo.data.materials: fo.data.materials.append(bpy.data.materials.new("food_slot"))
foodmesh=fo.data; fo.hide_render=True; fo.hide_viewport=True; fo.location=(9000,9100,0)
foodmat=bpy.data.materials.new("FOODcream"); foodmat.diffuse_color=(242/255,230/255,158/255,1)
DOME=4.3; FCLEAR=0.94/0.90    # build_play_real food/piece clearance so the socket swallows the peg with slack
allbodies=[B for objs in bodies.values() for B in objs]
_tor=[o for o in bpy.data.objects if o.type=='MESH' and 'torus' in o.name.lower()]
# presample each cup's scale across the WHOLE timeline (drives food appear/disappear), step 2 frames
STEP=2; FRS=list(range(1,581,STEP))
cupS={T:[] for T in _tor}; cupmax={T:1e-6 for T in _tor}
for f in FRS:
    sc.frame_set(f)
    for T in _tor:
        s=max(abs(T.scale[i]) for i in range(3)); cupS[T].append(s)
        if s>cupmax[T]: cupmax[T]=s
sc.frame_set(400)
foods=0
for T in _tor:
    if cupmax[T]<1e-3: continue
    tw=T.matrix_world.translation
    best=None; bd=1e9
    for B in allbodies:
        bw=B.matrix_world.translation; d=((tw.x-bw.x)**2+(tw.y-bw.y)**2)**0.5
        if d<bd: bd=d; best=B
    if best is None or bd>3.0: continue                  # stray center ring -> skip
    N=Nmap.get(best)
    if N is None: continue
    typ=MAP[len(best.data.vertices)]
    F=bpy.data.objects.new(T.name+"_FOOD", foodmesh); sc.collection.objects.link(F)
    F.parent=N; F.matrix_parent_inverse=mathutils.Matrix.Identity(4)
    F.location=(0.0,0.0, Hnew[typ]-DOME)                 # piece-local central axis, at the plateau: socket swallows the peg
    F.rotation_euler=(0,0,0)                             # upright (piece is upright)
    if F.material_slots: F.material_slots[0].link='OBJECT'; F.material_slots[0].material=foodmat
    # appear/disappear: local scale = FCLEAR*(cup_scale/cup_max); x piece world-scale S -> world FS*ratio
    for i,f in enumerate(FRS):
        sv=FCLEAR*(cupS[T][i]/cupmax[T])
        F.scale=(sv,sv,sv); F.keyframe_insert("scale",frame=f)
    foods+=1
print("seated_food",foods)

# hide the old cup meshes (their transforms still drive the parented FOOD)
hid=0
for o in _tor: o.hide_render=True; hid+=1
print("hid_toruses",hid)

sc.render.resolution_percentage=int(os.environ.get("RES_PCT","50"))
sc.render.image_settings.file_format='PNG'
if os.environ.get("ANIM","0")=="1":
    sc.frame_start=int(os.environ.get("FSTART","1")); sc.frame_end=int(os.environ.get("FEND","580"))
    sc.render.filepath=os.environ.get("OUTDIR","/mnt/data/archive/organism-renders/oldvideo/frames/")+"f"
    sc.render.use_overwrite=False      # RESUME: skip frames whose PNG already exists (Blender writes each only when complete)
    print("ANIM range %d-%d (resume)"%(sc.frame_start,sc.frame_end))
    bpy.ops.render.render(animation=True)
    print("RENDERED_ANIM")
elif os.environ.get("FRAMES"):
    for _fr in [int(x) for x in os.environ["FRAMES"].split(",")]:
        sc.frame_set(_fr)
        sc.render.filepath=os.environ.get("OUTDIR","/mnt/data/archive/organism-renders/oldvideo/")+"chk_%04d"%_fr
        bpy.ops.render.render(write_still=True); print("CHK",_fr)
    print("RENDERED_CHECKS")
else:
    _fr=int(os.environ.get("FRAME","400")); sc.frame_set(_fr)
    sc.render.filepath=os.environ.get("OUTPNG","/tmp/claude-1000/-home-youdonotexist-code-organism/bbf7149e-6408-42a3-bcf9-0890d4f37841/scratchpad/swap_test_%d"%_fr)
    bpy.ops.render.render(write_still=True)
    print("RENDERED_SWAP")
