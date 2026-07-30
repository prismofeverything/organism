"""ORGANISM unboxing (FULL) — every component with its real texture, animated FROM the packed box TO
the "what's in the box" HERO (build_layout.py football-arc layout): round pentaboard centre, five sets
on two curved arcs, food back-left, printed goods in a back row behind the board, mutations fanned, the
box standing (cover + wrap) at the back. Real scale (1u=1mm), Cycles.
  ~/Downloads/.../blender -b --threads 4 --python build_anim.py
Env: RESX/RESY, SAMPLES, LAST(frames), FRAME(single still), FR(out dir)
"""
import bpy, os, math, glob, colorsys
from mathutils import Vector
P=os.path.dirname(os.path.abspath(__file__)); ASSETS=f"{P}/clip_assets"; LA=f"{P}/layout_assets"
FR=os.environ.get("FR","/mnt/data/archive/organism-renders/unbox4"); os.makedirs(FR,exist_ok=True)
RESX=int(os.environ.get("RESX","640")); RESY=int(os.environ.get("RESY","400")); SAMPLES=int(os.environ.get("SAMPLES","10"))
LAST=int(os.environ.get("LAST","338"))
def _l(c): return c/12.92 if c<=0.04045 else ((c+0.055)/1.055)**2.4
def lin3(c): return (_l(c[0]),_l(c[1]),_l(c[2]),1.0)
def hsl(h,s,l): return colorsys.hls_to_rgb(h/360.0,l/100.0,s/100.0)
PLAYERS=["red","yellow","blue","purple","green"]
PCOL={"red":hsl(353,62,58),"yellow":hsl(45,78,60),"blue":hsl(196,45,56),"purple":hsl(266,55,60),"green":hsl(118,45,66)}
FOOD_RGB=(242/255,230/255,158/255)
def rnd(s): x=math.sin(s*12.9898)*43758.5; return x-math.floor(x)
bpy.ops.wm.read_factory_settings(use_empty=True); sc=bpy.context.scene
sc.render.engine="CYCLES"
try: sc.cycles.device='CPU'; sc.cycles.samples=SAMPLES; sc.cycles.use_denoising=True
except Exception: pass
sc.render.resolution_x=RESX; sc.render.resolution_y=RESY; sc.render.fps=24
try: sc.view_settings.view_transform='Standard'
except Exception: pass
w=bpy.data.worlds.new("W"); sc.world=w; w.use_nodes=True
w.node_tree.nodes["Background"].inputs[0].default_value=(0.05,0.055,0.065,1.0)
def C(): return bpy.context.collection
def sun(nm,en,rx,rz):
    d=bpy.data.lights.new(nm,'SUN'); d.energy=en; d.angle=math.radians(5)
    o=bpy.data.objects.new(nm,d); C().objects.link(o); o.rotation_euler=(math.radians(rx),0,math.radians(rz))
sun("K",4.4,50,35); sun("F",1.9,62,-120); sun("R",2.1,116,175)
def colmat(name,rgb,rough=0.45):
    m=bpy.data.materials.new(name); m.use_nodes=True
    b=m.node_tree.nodes["Principled BSDF"]; b.inputs["Base Color"].default_value=lin3(rgb); b.inputs["Roughness"].default_value=rough
    return m
_IMG={}
def imgmat(name,path,shadeless=False):
    key=(path,shadeless)
    if key in _IMG: return _IMG[key]
    m=bpy.data.materials.new(name); m.use_nodes=True; nt=m.node_tree; out=nt.nodes["Material Output"]
    img=bpy.data.images.load(path,check_existing=True)
    if shadeless:
        for n in list(nt.nodes):
            if n.type!="OUTPUT_MATERIAL": nt.nodes.remove(n)
        t=nt.nodes.new("ShaderNodeTexImage"); t.image=img
        e=nt.nodes.new("ShaderNodeEmission"); nt.links.new(t.outputs["Color"],e.inputs["Color"]); nt.links.new(e.outputs[0],out.inputs["Surface"])
    else:
        t=nt.nodes.new("ShaderNodeTexImage"); t.image=img
        b=nt.nodes["Principled BSDF"]; nt.links.new(t.outputs["Color"],b.inputs["Base Color"]); b.inputs["Roughness"].default_value=0.6
    _IMG[key]=m; return m
me=bpy.data.meshes.new("T"); o=bpy.data.objects.new("T",me); C().objects.link(o)
s=4000; me.from_pydata([(-s,-s,0),(s,-s,0),(s,s,0),(-s,s,0)],[],[(0,1,2,3)]); me.update()
me.materials.append(colmat("tab",(0.11,0.12,0.14),0.85)); o.location=(0,0,-0.1)
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
MIND={t:load(f"{P}/out/{t}_mindisk.obj") for t in ["EAT","MOVE","GROW"]}
FOODT=load(f"{P}/renders/food/FOOD_slip.obj",zup=False)
GOLD=colmat("gold",FOOD_RGB,0.5)
def dup(tpl,mat,loc,rot=(0,0,0),scale=1.0):
    o=bpy.data.objects.new("d",tpl.data); C().objects.link(o)
    o.location=loc; o.rotation_euler=rot; o.scale=(scale,scale,scale)
    o.material_slots[0].link='OBJECT'; o.material_slots[0].material=mat; return o
def disc(name,mat,loc,r,h=2.0,rotz=0.0):
    bpy.ops.mesh.primitive_cylinder_add(vertices=48,radius=r,depth=h,location=loc)
    o=bpy.context.active_object; o.name=name; o.rotation_euler=(0,0,rotz)
    try: bpy.ops.object.shade_smooth()
    except Exception: pass
    o.data.materials.clear(); o.data.materials.append(mat); return o
def disc_art(name,path,loc,r,rotz=0.0,h=2.0,shadeless=True,crop=1.0):
    o=disc(name,colmat(name+"_e",(0.05,0.06,0.08),0.7),loc,r,h=h,rotz=0)
    o.data.materials.append(imgmat(name+"_f",path,shadeless=shadeless))
    for poly in o.data.polygons: poly.material_index=1 if poly.normal.z>0.9 else 0
    uv=o.data.uv_layers[0] if o.data.uv_layers else o.data.uv_layers.new()
    for poly in o.data.polygons:
        for li in poly.loop_indices:
            co=o.data.vertices[o.data.loops[li].vertex_index].co
            uv.data[li].uv=(0.5+crop*co.x/(2*r),0.5+crop*co.y/(2*r))
    o.rotation_euler=(0,0,rotz); return o

# ================= box: tray + lid, BOTH with cover(top) + wrap(sides) art =================
BX,BY=0.0,-30.0; IW,ID,BH=330.0,312.0,92.0; hw,hd=IW/2,ID/2
COVER=imgmat("cover",f"{ASSETS}/box_top.png"); WRAP=imgmat("wrap",f"{ASSETS}/box_wrap.png")
WRAPS=float(os.environ.get("WRAPS","0.56"))/330.0               # box_wrap image-fraction per mm (cover panel ~0.56 wide); tune via WRAPS
def wrap_walls(mesh, z_off):                                     # continuous DRAPE of box_wrap: sides continue the top's art outward at UNIFORM scale (no stretch)
    uv=mesh.uv_layers.active or mesh.uv_layers.new()
    S=WRAPS; TOPZ=116.0                                          # box top / fold line at world z=116
    for poly in mesh.polygons:
        n=poly.normal
        if abs(n.z)>0.5: continue                               # walls only
        for li in poly.loop_indices:
            c=mesh.vertices[mesh.loops[li].vertex_index].co; d=TOPZ-(c.z+z_off)   # depth below the top fold
            if abs(n.y)>=abs(n.x):
                u=0.5+S*c.x; v=(0.5-S*hd)-S*d if n.y<0 else (0.5+S*hd)+S*d        # front / back
            else:
                v=0.5+S*c.y; u=(0.5-S*hw)-S*d if n.x<0 else (0.5+S*hw)+S*d        # left / right
            uv.data[li].uv=(u,v)
# tray (walls wrapped, open top)
tv=[(-hw,-hd,0),(hw,-hd,0),(hw,hd,0),(-hw,hd,0),(-hw,-hd,BH),(hw,-hd,BH),(hw,hd,BH),(-hw,hd,BH)]
tm=bpy.data.meshes.new("tray"); tray=bpy.data.objects.new("tray",tm); C().objects.link(tray)
tm.from_pydata(tv,[],[(0,1,2,3),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); tm.update()
tm.materials.append(colmat("trayin",(0.10,0.14,0.16),0.6)); tm.materials.append(WRAP)
for i,p in enumerate(tm.polygons): p.material_index=1 if abs(p.normal.z)<0.5 else 0
wrap_walls(tm,0)                                                 # tray walls sit at world z 0..92
for p in tm.polygons: p.use_smooth=False
tray.location=(BX,BY,0)
# lid: cover on top + wrap on the skirt
lh=30; lv=[(-hw-4,-hd-4,0),(hw+4,-hd-4,0),(hw+4,hd+4,0),(-hw-4,hd+4,0),(-hw-4,-hd-4,lh),(hw+4,-hd-4,lh),(hw+4,hd+4,lh),(-hw-4,hd+4,lh)]
lm=bpy.data.meshes.new("lid"); lid=bpy.data.objects.new("lid",lm); C().objects.link(lid)
lm.from_pydata(lv,[],[(4,5,6,7),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); lm.update()
lm.materials.append(COVER); lm.materials.append(WRAP)
for i,p in enumerate(lm.polygons): p.material_index=0 if p.normal.z>0.5 else 1
uv=lm.uv_layers.new()
for li in lm.polygons[0].loop_indices:
    vi=lm.loops[li].vertex_index; uv.data[li].uv={4:(0,0),5:(1,0),6:(1,1),7:(0,1)}[vi]
wrap_walls(lm,BH-6)                                              # lid skirt sits at world z=86..116 -> top of each flap, continuing the tray
for p in lm.polygons: p.use_smooth=False

# ================= hero geometry (build_layout football arc, real scale) =================
PLACED=[]
def reg(obj,pk,prot,hero,hrot,wave): PLACED.append(dict(obj=obj,pk=pk,prot=prot,hero=hero,hrot=hrot,wave=wave)); return obj
R=340.0; DCOL=math.radians(7.2); ROUT=560.0; RDIAG=540.0
def ppos(deg,dcol,drad):
    a=math.radians(deg)+dcol*DCOL; rr=R+drad
    return Vector((rr*math.cos(a),rr*math.sin(a),0)), a-math.radians(90)
def ring(deg): th=math.radians(deg); return Vector((ROUT*math.cos(th),ROUT*math.sin(th),0)), th-math.radians(90)
# packed floor-slots inside the box (things hide in the box, then rise out)
SLOTS=[(BX-118+ix*59, BY-108+iy*54) for iy in range(5) for ix in range(5)]
def slot(i): return SLOTS[i%len(SLOTS)]
ARC=[(130,"food"),(180,"red"),(230,"yellow"),(50,"blue"),(0,"purple"),(-50,"green")]
order=["EAT","MOVE","GROW"]; PSCALE=1.0
pidx=0; si=0
for deg,who in ARC:
    if who=="food":
        n=0; col=0
        while n<60:
            hc=3+int(rnd(col)*6); base,_=ppos(deg,col%4-1.5,(col//4-1)*40)
            sx,sy=slot(si); si+=1
            for k in range(hc):
                if n>=60: break
                pk=Vector((sx,sy,3+k*6.92))
                reg(dup(FOODT,GOLD,pk,(0,0,rnd(n)*6.28),1.0),pk,(0,0,rnd(n)*6.28),base+Vector((0,0,1+k*6.92)),rnd(n)*6.28,"food"); n+=1
            col+=1
        continue
    pl=who; pi=PLAYERS.index(pl); pmat=colmat("pm_"+pl,PCOL[pl]); dmat=colmat("dm_"+pl,PCOL[pl])
    for r,t in enumerate(order):
        for cc in range(4):
            gx,gy=pidx%10,pidx//10
            pk=Vector((BX-148+gx*32, BY+112-gy*38, 44+(gy//3)*38)); prot=(math.radians(90),0,0)
            hero,hrot=ppos(deg,cc-1.5,(r-1)*40); hero.z=0
            reg(dup(TPL[t],pmat,pk,prot,PSCALE),pk,prot,hero,hrot,"p%d"%pi); pidx+=1
    for di,t in enumerate(order):                              # 3 minimal-disk stacks BEHIND the set
        base,hr=ppos(deg,(di-1)*1.4,92); sx,sy=slot(si); si+=1
        for k in range(4):
            pk=Vector((sx,sy,4+k*10.5))
            reg(dup(MIND[t],dmat,pk,(0,0,0),PSCALE),pk,(0,0,0),base+Vector((0,0,4+k*10.5)),hr,"disk")
    base,hr=ppos(deg,-2.9,6); sx,sy=slot(si); si+=1            # platforms beside the set
    for k in range(9):
        pk=Vector((sx,sy,1.2+k*2.3))
        reg(disc_art("plat_%d_%d"%(pi,k),f"{LA}/plats/{pl}.png",pk,18.5,rotz=0,h=2.2,crop=0.8,shadeless=False),pk,(0,0,0),base+Vector((0,0,1.2+k*2.3)),hr,"disk")

# ---- back row behind the board: rulebook | player aids | power board (+ tokens) ----  (packed FLAT in box, centred)
def flat_pk(cx,cy,z): return Vector((cx,cy,z))
# rulebook
rbh,rir=ring(140)
bm=bpy.data.meshes.new("book"); book=bpy.data.objects.new("book",bm); C().objects.link(book)
RW,RTk=210.0,7.0
bv=[(-RW/2,-RW/2,0),(RW/2,-RW/2,0),(RW/2,RW/2,0),(-RW/2,RW/2,0),(-RW/2,-RW/2,RTk),(RW/2,-RW/2,RTk),(RW/2,RW/2,RTk),(-RW/2,RW/2,RTk)]
bm.from_pydata(bv,[],[(4,5,6,7),(0,3,2,1),(0,1,5,4),(1,2,6,5),(2,3,7,6),(3,0,4,7)]); bm.update()
bm.materials.append(COVER); bm.materials.append(colmat("book_s",(0.9,0.9,0.88),0.7))
for i,p in enumerate(bm.polygons): p.material_index=0 if i==0 else 1
uvb=bm.uv_layers.new()
for li in bm.polygons[0].loop_indices:
    vi=bm.loops[li].vertex_index; uvb.data[li].uv={4:(0,0),5:(1,0),6:(1,1),7:(0,1)}[vi]
reg(book,flat_pk(BX-30,BY,16),(0,0,0),Vector((rbh.x,rbh.y,0)),rir,"back")
# player aids (5, fanned) — packed flat centre-stacked
for k in range(5):
    th=math.radians(116)+(k-2)*math.radians(6.0); rad=RDIAG
    hero=Vector((rad*math.cos(th),rad*math.sin(th),1.0+k*0.3))
    pk=flat_pk(BX-70,BY+50,25+k*1.6)
    reg(disc_art("aid_%d"%k,f"{ASSETS}/player_aid.png",pk,52,rotz=0,h=1.4,shadeless=False),pk,(0,0,0),hero,th-math.radians(90),"back")
# power board
pbh,pir=ring(64)
reg(disc_art("powerboard",f"{ASSETS}/power_board.png",flat_pk(BX+40,BY,20),105,rotz=0,h=3.0,shadeless=False),flat_pk(BX+40,BY,20),(0,0,0),Vector((pbh.x,pbh.y,0.5)),pir,"back")
# power tokens: 3 per player, stacks straddling the power board
sdx,sdy=-math.cos(math.radians(64)),-math.sin(math.radians(64)); tdx,tdy=-sdy,sdx
for pi,pl in enumerate(PLAYERS):
    cx=pbh.x+70*sdx+(pi-2)*22*tdx; cy=pbh.y+70*sdy+(pi-2)*22*tdy
    tmat=colmat("pw_"+pl,PCOL[pl],0.5); sx,sy=slot(20+pi)
    for k in range(3):
        pk=Vector((sx,sy,2.4+k*3.0))
        reg(disc("tok_%d_%d"%(pi,k),tmat,pk,11,h=2.7),pk,(0,0,0),Vector((cx,cy,2.4+k*3.0)),0,"back")
# mutation cards fanned upper-right (three short arcs straddling RDIAG)
mcards=(sorted(glob.glob(f"{LA}/cards/card_*.png")) or sorted(glob.glob(f"{LA}/MutationCard*.png")))[:26]
for k,cp in enumerate(mcards):
    rr=RDIAG-40+(k%3)*40; th=math.radians(42)+((k//3)-(len(mcards)//6))*math.radians(2.8)
    hero=Vector((rr*math.cos(th),rr*math.sin(th),1.2+ (k%3)*0.3))
    sx,sy=slot(k%25); pk=Vector((sx,sy,3+(k%6)*1.0))
    reg(disc_art("mut_%d"%k,cp,pk,18.5,rotz=0,h=1.0,crop=0.9,shadeless=True),pk,(0,0,0),hero,th-math.radians(90),"mut")

# ================= board: a CIRCLE that quad-folds — 4 quarter-disc planes on real hinges, DOUBLE-SIDED (Pent top / Hex bottom), real 3mm depth so faces never coincide (no z-fight) =================
hub=bpy.data.objects.new("hub",None); C().objects.link(hub); hub.rotation_mode='XYZ'
RB=235.0; TBd=3.0; SEG=26
PENTART=imgmat("pentart",f"{LA}/27_Pent_54cm_01.png",shadeless=False)
HEXART =imgmat("hexart", f"{LA}/27_HEX_54cm_01.png", shadeless=False)
BEDGE=colmat("bedge",(0.10,0.14,0.16),0.6)
def hinge(name,parent):                                         # empty pivot on a fold line; children rotate ONLY about it
    e=bpy.data.objects.new(name,None); C().objects.link(e); e.parent=parent; e.location=(0,0,0); e.rotation_mode='XYZ'; return e
def qpanel(name,a0,parent):                                     # quarter-disc pie theta in [a0,a0+90], solid slab; top=Pent, bottom=Hex(x-mirrored so the flip reads), rim=edge
    m=bpy.data.meshes.new(name); o=bpy.data.objects.new(name,m); C().objects.link(o)
    V=[(0,0,0),(0,0,TBd)]; ab=[]; at=[]                         # 0=bottom centre, 1=top centre
    for s in range(SEG+1):
        th=math.radians(a0+90.0*s/SEG); x,y=RB*math.cos(th),RB*math.sin(th)
        ab.append(len(V)); V.append((x,y,0.0)); at.append(len(V)); V.append((x,y,TBd))
    F=[]; MI=[]
    for s in range(SEG):
        F.append((1,at[s],at[s+1])); MI.append(0)               # top fan  -> Pent (+z), consistent CCW winding
        F.append((0,ab[s+1],ab[s])); MI.append(1)               # bottom fan-> Hex  (-z)
        F.append((ab[s],ab[s+1],at[s+1],at[s])); MI.append(2)   # arc rim
    F.append((0,1,at[0],ab[0])); MI.append(2); F.append((0,ab[SEG],at[SEG],1)); MI.append(2)   # the two straight radius (fold) edges
    m.from_pydata(V,[],F); m.update()
    for mat in (PENTART,HEXART,BEDGE): m.materials.append(mat)
    uv=m.uv_layers.new()
    for i,p in enumerate(m.polygons):
        p.material_index=MI[i]
        for li in p.loop_indices:
            c=m.vertices[m.loops[li].vertex_index].co
            if   MI[i]==0: uv.data[li].uv=(0.5+0.5*c.x/RB, 0.5+0.5*c.y/RB)   # Pent (top)
            elif MI[i]==1: uv.data[li].uv=(0.5-0.5*c.x/RB, 0.5+0.5*c.y/RB)   # Hex (bottom), x-mirrored for the double-sided flip
    for p in m.polygons: p.use_smooth=False
    o.parent=parent; return o
# hinge tree: P0 base(0-90deg). H1 folds P1(90-180) about the +y edge (x=0). H2 folds the bottom half about the +x edge (y=0). H3 folds P3(180-270) about x=0 (rides on H2). Unfolds to a full CIRCLE.
H1=hinge("H1",hub); H2=hinge("H2",hub); H3=hinge("H3",H2)
P0=qpanel("P0",0,hub); P1=qpanel("P1",90,H1); P2=qpanel("P2",270,H2); P3=qpanel("P3",180,H3)

def key(o,f,loc=None,rot=None):
    if loc is not None: o.location=loc; o.keyframe_insert("location",frame=f)
    if rot is not None: o.rotation_euler=rot; o.keyframe_insert("rotation_euler",frame=f)

# ================= animation =================
WAVE={"p0":40,"p1":50,"p2":60,"p3":70,"p4":80,"disk":108,"food":136,"mut":160,"back":172}   # all components out before the board lifts (216)
DUR={"food":26,"mut":22,"back":32}
for d in PLACED:
    o=d["obj"]; pk=d["pk"]; prot=d["prot"]; hero=d["hero"]; hrot=d["hrot"]; wv=d["wave"]
    t0=WAVE.get(wv,120)+(rnd(id(o)%997))*8; dur=DUR.get(wv,30); t1=t0+dur
    apex=(pk+hero)*0.5+Vector((0,0,150))
    o.rotation_mode='XYZ'
    key(o,0,pk,prot); key(o,t0,pk,prot)
    key(o,int((t0+t1)/2),apex,(prot[0]*0.5,0,hrot))
    key(o,t1,Vector((hero.x,hero.y,hero.z)),(0,0,hrot))
# lid -> stands at the BACK as the cover+wrap display box
lid.location=(BX,BY,BH-6); lid.rotation_euler=(0,0,0)
for a in ("location","rotation_euler"): lid.keyframe_insert(a,frame=0)
lid.location=(BX,BY,BH+120); lid.keyframe_insert("location",frame=16)
lid.location=(0,600,168); lid.rotation_euler=(math.radians(90),0,0)   # cover+wrap display, behind the back row
for a in ("location","rotation_euler"): lid.keyframe_insert(a,frame=48)
# board is LAST out: rise straight UP -> slow PULSING TUMBLE (reveals the hex underside) held high -> GLORIOUS slow lower + unfold
LIFT=210; UPCLR=228; CTR=252; TRAYGO=232; LOWER=300; UF1=318; END=336
hub.location=(-120,-150,8)                                      # folded quarter-disc stack, packed in the box
hub.keyframe_insert("location",frame=0); hub.keyframe_insert("location",frame=LIFT)
hub.location=(-120,-150,265); hub.keyframe_insert("location",frame=UPCLR)       # (1) straight UP (NOT to the side), clear of the box
hub.location=(0,0,300); hub.keyframe_insert("location",frame=CTR)               # drift to a high, centred hold
hub.keyframe_insert("location",frame=LOWER)                                     # hold high while the box arcs away
hub.location=(0,0,4); hub.keyframe_insert("location",frame=END-2)               # (3) glorious SLOW lower to centre
def pulse_tumble(obj,f0,f1,total,npulse,axis=0):               # slow rotate, velocity pulsing ~0.5x..2x; ends at `total` (flat)
    N=48; us=[i/N for i in range(N+1)]; vr=[1.25+0.75*math.sin(2*math.pi*npulse*u-math.pi/2) for u in us]; cum=[0.0]
    for i in range(N): cum.append(cum[-1]+(vr[i]+vr[i+1])*0.5/N)
    for i,u in enumerate(us):
        e=list(obj.rotation_euler); e[axis]=cum[i]/cum[-1]*total; obj.rotation_euler=e
        obj.keyframe_insert("rotation_euler",frame=int(round(f0+(f1-f0)*u)))
pulse_tumble(hub,LIFT,LOWER,2*math.pi,3,0)                     # one slow pulsing barrel-roll: Pent -> HEX -> Pent
hub.rotation_euler=(2*math.pi,0,0); hub.keyframe_insert("rotation_euler",frame=END)   # hold flat through the lower
def hk(h,rot,fr): h.rotation_euler=rot; h.keyframe_insert("rotation_euler",frame=fr)   # SLOW 2-stage unfold DURING the lower
for fr in (0,LIFT,LOWER): hk(H1,(0,math.pi,0),fr); hk(H2,(math.pi,0,0),fr); hk(H3,(0,math.pi,0),fr)
hk(H1,(0,math.pi,0),UF1); hk(H3,(0,math.pi,0),UF1); hk(H2,(0,0,0),UF1); hk(H2,(0,0,0),END)
hk(H1,(0,0,0),END); hk(H3,(0,0,0),END)
STK=5.0                                                         # z-stack folded layers so no faces coincide (no flicker)
def pz(o,z,fr): o.location=(0,0,z); o.keyframe_insert("location",frame=fr)
for fr in (0,LIFT,LOWER): pz(P1,-STK,fr); pz(P2,-2*STK,fr); pz(P3,3*STK,fr)
pz(P1,-STK,UF1); pz(P3,3*STK,UF1); pz(P2,0,UF1); pz(P1,0,END); pz(P3,0,END)
# tray: rise SLIGHTLY to clear the pieces, then arc LOW along the table around the right and tuck BEHIND the standing box (board is high above -> no collision)
key(tray,0,(BX,BY,0)); key(tray,TRAYGO,(BX,BY,0))
key(tray,TRAYGO+10,(BX,BY,78))                                 # up slightly, over the pieces
key(tray,TRAYGO+22,(480,150,78)); key(tray,TRAYGO+34,(450,560,78)); key(tray,TRAYGO+46,(160,800,78))   # arc around the right, clearing the standing box
key(tray,TRAYGO+56,(0,840,78)); key(tray,TRAYGO+64,(0,840,0))  # settle behind the standing box
# ===== GLORIOUS LOWERING: the board emanates warm-yellow plasma light as it settles =====
pbsdf=PENTART.node_tree.nodes["Principled BSDF"]; pimg=next(n for n in PENTART.node_tree.nodes if n.type=='TEX_IMAGE')
PENTART.node_tree.links.new(pimg.outputs["Color"],pbsdf.inputs["Emission Color"])          # board self-illuminates its own art
for fr,v in [(LOWER-8,0.0),(UF1,0.04),(END,0.06)]:                                          # barely-there self-glow — board art stays fully readable
    pbsdf.inputs["Emission Strength"].default_value=v; pbsdf.inputs["Emission Strength"].keyframe_insert("default_value",frame=fr)
gl=bpy.data.lights.new("glow",'POINT'); gl.color=(1.0,0.82,0.34); gl.shadow_soft_size=260     # gentle warm WASH (raised high + big+soft so it never hotspots the centre)
glo=bpy.data.objects.new("glow",gl); C().objects.link(glo); glo.location=(0,0,150)
for fr,e in [(LOWER-8,0.0),(UF1,0.09e6),(END,0.13e6)]:
    gl.energy=e; gl.keyframe_insert("energy",frame=fr)
bpy.ops.mesh.primitive_cylinder_add(vertices=44,radius=252,depth=46,location=(0,0,25))        # LOW faint plasma haze hugging the board (not a light pillar)
plz=bpy.context.active_object; plz.name="plasma"; plz.display_type='WIRE'
pmat=bpy.data.materials.new("plasma"); pmat.use_nodes=True; pnt=pmat.node_tree; pnt.nodes.clear()
po=pnt.nodes.new("ShaderNodeOutputMaterial"); pv=pnt.nodes.new("ShaderNodeVolumePrincipled")
ptc=pnt.nodes.new("ShaderNodeTexCoord"); pmap=pnt.nodes.new("ShaderNodeMapping"); pnz=pnt.nodes.new("ShaderNodeTexNoise")
pnz.inputs['Scale'].default_value=2.4; pnz.inputs['Detail'].default_value=3.0
pmr=pnt.nodes.new("ShaderNodeMapRange"); pmr.inputs['From Min'].default_value=0.45; pmr.inputs['From Max'].default_value=0.72
pav=pnt.nodes.new("ShaderNodeValue"); pmul=pnt.nodes.new("ShaderNodeMath"); pmul.operation='MULTIPLY'
pnt.links.new(ptc.outputs['Object'],pmap.inputs['Vector']); pnt.links.new(pmap.outputs['Vector'],pnz.inputs['Vector'])
pnt.links.new(pnz.outputs['Fac'],pmr.inputs['Value']); pnt.links.new(pmr.outputs['Result'],pmul.inputs[0]); pnt.links.new(pav.outputs[0],pmul.inputs[1])
pnt.links.new(pmul.outputs['Value'],pv.inputs['Density'])
pv.inputs['Color'].default_value=(1.0,0.8,0.32,1.0); pv.inputs['Emission Color'].default_value=(1.0,0.76,0.26,1.0)
pes=pnt.nodes.new("ShaderNodeMath"); pes.operation='MULTIPLY'; pes.inputs[1].default_value=2.5
pnt.links.new(pmul.outputs['Value'],pes.inputs[0]); pnt.links.new(pes.outputs['Value'],pv.inputs['Emission Strength'])
pnt.links.new(pv.outputs[0],po.inputs['Volume']); plz.data.materials.append(pmat)
pmap.inputs['Location'].default_value=(0,0,0); pmap.inputs['Location'].keyframe_insert("default_value",frame=LOWER-8)
pmap.inputs['Location'].default_value=(0.3,0.15,1.2); pmap.inputs['Location'].keyframe_insert("default_value",frame=END)   # slow drift -> subtle shimmer
for fr,v in [(LOWER-8,0.0),(UF1,0.0025),(END,0.0035)]:                                       # WHISPER-thin haze -> board fully shows through
    pav.outputs[0].default_value=v; pav.outputs[0].keyframe_insert("default_value",frame=fr)

# ================= camera: closed box -> crane out to the hero framing =================
cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=40; cam_d.clip_end=16000
cam=bpy.data.objects.new("Cam",cam_d); C().objects.link(cam); sc.camera=cam
aim=bpy.data.objects.new("aim",None); C().objects.link(aim)
tc=cam.constraints.new('TRACK_TO'); tc.target=aim; tc.track_axis='TRACK_NEGATIVE_Z'; tc.up_axis='UP_Y'
for f,cl,al in [(0,(0,-640,320),(BX,BY,30)),(95,(0,-1010,665),(0,-10,62)),(200,(0,-1370,800),(0,74,52)),(LAST,(0,-1370,800),(0,74,52))]:   # pulled back + raised so the standing box fits at 16:9
    cam.location=cl; cam.keyframe_insert("location",frame=f); aim.location=al; aim.keyframe_insert("location",frame=f)

sc.frame_start=int(os.environ.get("FIRST","0")); sc.frame_end=LAST; sc.render.image_settings.file_format='PNG'
single=os.environ.get("FRAME")
if single:
    sc.frame_set(int(single)); sc.render.filepath=f"{FR}/single_{int(single):04d}"; bpy.ops.render.render(write_still=True); print("frame ->",sc.render.filepath)
else:
    sc.render.filepath=f"{FR}/f"; bpy.ops.render.render(animation=True); print("anim ->",FR)
print("components:",len(PLACED)+1,"objects")
