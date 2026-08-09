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
LAST=int(os.environ.get("LAST","810"))                          # dive ends at SWOOPEND(521); the ONE continuous track keeps zooming into the crimson; words surface as it fills
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
# table with a BOWL carved at centre so the board can keep spinning as it lowers (its edge dips into the well)
RBOWL=float(os.environ.get("RBOWL","244")); DBOWL=float(os.environ.get("DBOWL","268"))   # slightly wider than the board (235) -> catches the 2x-spin edge dip; leaves a thin glowing ring at rest
def bowlz(r): return 0.0 if r>=RBOWL else -DBOWL*math.sqrt(max(0.0,1.0-(r/RBOWL)**2))   # ellipsoid bowl (rim -> 0)
me=bpy.data.meshes.new("T"); o=bpy.data.objects.new("T",me); C().objects.link(o)
_NS=72; _rs=[RBOWL*k/28 for k in range(1,29)]+[RBOWL*1.4,RBOWL*2.2,900,4000]
_V=[(0,0,bowlz(0))]; _F=[]; _ring=[]
for r in _rs:
    _ring.append(len(_V))
    for si in range(_NS):
        th=2*math.pi*si/_NS; _V.append((r*math.cos(th),r*math.sin(th),bowlz(r)))
for si in range(_NS): _F.append((0,1+si,1+(si+1)%_NS))         # centre cap
for ri in range(len(_rs)-1):                                   # rings
    a=_ring[ri]; b=_ring[ri+1]
    for si in range(_NS):
        s2=(si+1)%_NS; _F.append((a+si,b+si,b+s2,a+s2))
me.from_pydata(_V,[],_F); me.update()
try: [setattr(p,"use_smooth",True) for p in me.polygons]
except Exception: pass
me.materials.append(colmat("tab",(0.11,0.12,0.14),0.85)); o.location=(0,0,-0.15)
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
TWOD=int(os.environ.get("TWOD","0"))                            # 2D minimal-disk piece tokens: REMOVED from the product+animation (TWOD=1 restores)
TPL={t:load(f"{P}/out/{t}_sculpt_graft.obj") for t in ["EAT","MOVE","GROW"]}
MIND={t:load(f"{P}/out/{t}_mindisk.obj") for t in ["EAT","MOVE","GROW"]} if TWOD else {}
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
# ---- NO fake red disc: the ending's backdrop is the cover's OWN crimson cell (world ~(-5,570,170), 78x49mm).
# The camera simply travels far enough into it that it fills the frame. `_fademat` = white emission gated by a
# keyframable fade (transparent -> white; x image alpha for symbols) — used by the surfacing words. ----
def _fademat(name,image=None):
    m=bpy.data.materials.new(name); m.use_nodes=True; nt=m.node_tree
    for n in list(nt.nodes): nt.nodes.remove(n)
    em=nt.nodes.new("ShaderNodeEmission"); em.inputs["Color"].default_value=(1,1,1,1); em.inputs["Strength"].default_value=1.0
    tr=nt.nodes.new("ShaderNodeBsdfTransparent"); fade=nt.nodes.new("ShaderNodeValue"); fade.outputs[0].default_value=0.0
    mix=nt.nodes.new("ShaderNodeMixShader"); nt.links.new(tr.outputs[0],mix.inputs[1]); nt.links.new(em.outputs[0],mix.inputs[2])
    out=nt.nodes.new("ShaderNodeOutputMaterial"); nt.links.new(mix.outputs[0],out.inputs["Surface"])
    if image is not None:
        tex=nt.nodes.new("ShaderNodeTexImage"); tex.image=image; tex.interpolation='Cubic'
        mul=nt.nodes.new("ShaderNodeMath"); mul.operation='MULTIPLY'
        nt.links.new(tex.outputs["Alpha"],mul.inputs[0]); nt.links.new(fade.outputs[0],mul.inputs[1]); nt.links.new(mul.outputs[0],mix.inputs[0])
    else:
        nt.links.new(fade.outputs[0],mix.inputs[0])
    return m,fade

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
# ================= PACKED layout: real per-player COMPARTMENTS (3x2 grid) — how we actually pack the box =================
# floor tier at ZT (printed goods sit below); each player's set is a colour-blocked compartment.
ZT=13.0
CELL={"red":(-110,BY+78),"yellow":(0,BY+78),"blue":(110,BY+78),
      "purple":(-110,BY-78),"green":(0,BY-78),"food":(110,BY-78)}
for deg,who in ARC:
    if who=="food":                                            # food + accessory (platform/token) stacks share the 6th compartment
        cx,cy=CELL["food"]; n=0; col=0
        while n<60:
            hc=6+int(rnd(col)*4); sx=cx-38+(col%3)*38; sy=cy+46-(col//3)*40
            base,_=ppos(deg,col%4-1.5,(col//4-1)*40)
            for k in range(hc):
                if n>=60: break
                pk=Vector((sx,sy,ZT+k*6.0))
                reg(dup(FOODT,GOLD,pk,(0,0,rnd(n)*6.28),1.0),pk,(0,0,rnd(n)*6.28),base+Vector((0,0,1+k*6.92)),rnd(n)*6.28,"food"); n+=1
            col+=1
        continue
    pl=who; pi=PLAYERS.index(pl); pmat=colmat("pm_"+pl,PCOL[pl]); dmat=colmat("dm_"+pl,PCOL[pl])
    cx,cy=CELL[pl]
    for r,t in enumerate(order):                               # 12 pieces STANDING: 3 type-columns x 4 rows (back of the compartment)
        for cc in range(4):
            pk=Vector((cx+(r-1)*34, cy+56-cc*28, ZT)); prot=(0,0,0)
            hero,hrot=ppos(deg,cc-1.5,(r-1)*40); hero.z=0
            reg(dup(TPL[t],pmat,pk,prot,PSCALE),pk,prot,hero,hrot,"p%d"%pi)
    if TWOD:                                                   # 2D minimal-disk stacks (front strip) — REMOVED from the product (no more 2D pieces)
        for di,t in enumerate(order):                          # 3 per player, same colour = part of the set
            sx=cx-36+di*36; sy=cy-44; base,hr=ppos(deg,(di-1)*1.4,92)
            for k in range(4):
                pk=Vector((sx,sy,ZT+k*9))
                reg(dup(MIND[t],dmat,pk,(0,0,0),PSCALE),pk,(0,0,0),base+Vector((0,0,4+k*10.5)),hr,"disk")
    fx,fy=CELL["food"]; sx=fx-40+pi*20; sy=fy-44; base,hr=ppos(deg,-2.9,6)   # platform stacks grouped in the accessory cell
    for k in range(9):
        pk=Vector((sx,sy,ZT+k*2.0))
        reg(disc_art("plat_%d_%d"%(pi,k),f"{LA}/plats/{pl}.png",pk,18.5,rotz=0,h=2.2,crop=0.8,shadeless=False),pk,(0,0,0),base+Vector((0,0,1.2+k*2.3)),hr,"disk")

# ---- printed goods = the FLOOR bundle (packed flat BELOW the compartments, z<ZT); they rise LAST ----
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
reg(book,flat_pk(BX,BY,2),(0,0,0),Vector((rbh.x,rbh.y,0)),rir,"back")           # rulebook = bottom of the floor bundle
# player aids (5, fanned) — packed flat centre-stacked
for k in range(5):
    th=math.radians(116)+(k-2)*math.radians(6.0); rad=RDIAG
    hero=Vector((rad*math.cos(th),rad*math.sin(th),1.0+k*0.3))
    pk=flat_pk(BX-96,BY+58,3+k*1.6)                                             # aids stacked in a floor corner
    reg(disc_art("aid_%d"%k,f"{ASSETS}/player_aid.png",pk,52,rotz=0,h=1.4,shadeless=False),pk,(0,0,0),hero,th-math.radians(90),"back")
# power board
pbh,pir=ring(64)
reg(disc_art("powerboard",f"{ASSETS}/power_board.png",flat_pk(BX,BY,7),105,rotz=0,h=3.0,shadeless=False),flat_pk(BX,BY,7),(0,0,0),Vector((pbh.x,pbh.y,0.5)),pir,"back")   # power board on the rulebook
# power tokens: 3 per player, small colour stacks in the accessory (food) cell
sdx,sdy=-math.cos(math.radians(64)),-math.sin(math.radians(64)); tdx,tdy=-sdy,sdx
fx,fy=CELL["food"]
for pi,pl in enumerate(PLAYERS):
    cx=pbh.x+70*sdx+(pi-2)*22*tdx; cy=pbh.y+70*sdy+(pi-2)*22*tdy
    tmat=colmat("pw_"+pl,PCOL[pl],0.5); sx,sy=fx-44+pi*22, fy-32
    for k in range(3):
        pk=Vector((sx,sy,ZT+k*3.0))
        reg(disc("tok_%d_%d"%(pi,k),tmat,pk,11,h=2.7),pk,(0,0,0),Vector((cx,cy,2.4+k*3.0)),0,"back")
# mutation cards fanned upper-right (three short arcs straddling RDIAG)
mcards=(sorted(glob.glob(f"{LA}/cards/card_*.png")) or sorted(glob.glob(f"{LA}/MutationCard*.png")))[:26]
for k,cp in enumerate(mcards):
    rr=RDIAG-40+(k%3)*40; th=math.radians(42)+((k//3)-(len(mcards)//6))*math.radians(2.8)
    hero=Vector((rr*math.cos(th),rr*math.sin(th),1.2+ (k%3)*0.3))
    pk=Vector((BX+96,BY+58,3+k*0.7))                                            # mutations = a deck stacked in a floor corner
    reg(disc_art("mut_%d"%k,cp,pk,18.5,rotz=0,h=1.0,crop=0.9,shadeless=True),pk,(0,0,0),hero,th-math.radians(90),"mut")

# ================= board: a CIRCLE that quad-folds — 4 quarter-disc planes on real hinges, DOUBLE-SIDED (Pent top / Hex bottom), real 3mm depth so faces never coincide (no z-fight) =================
# board_root = the TUMBLE + descent pivot (sits at the board's CENTROID so it spins about its own centre,
# not the disc-centre which is the corner of the folded quarter). hub (child) is the disc-centre / hinge origin.
board_root=bpy.data.objects.new("board_root",None); C().objects.link(board_root); board_root.rotation_mode='QUATERNION'
hub=bpy.data.objects.new("hub",None); C().objects.link(hub); hub.parent=board_root; hub.rotation_mode='XYZ'
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
lid.location=(BX,BY,BH+148); lid.keyframe_insert("location",frame=14)          # (a) straight UP, high — well clear of the tray top
lid.location=(0,330,255); lid.keyframe_insert("location",frame=32)             # (b) glide BACK while still FLAT & high (tray back edge is y=126)
lid.location=(0,600,170); lid.rotation_euler=(math.radians(90),0,0)            # (c) settle to a standing poster — tilt ONLY now, behind everything
for a in ("location","rotation_euler"): lid.keyframe_insert(a,frame=52)
# board is LAST out: rise straight UP -> slow PULSING TUMBLE (reveals the hex underside) held high -> GLORIOUS slow lower + unfold
LIFT=210; UPCLR=228; CTR=252; TRAYGO=232; PEAK=300; LOWER=300; UF1=306; END=390   # SYMMETRIC arc: gradual RISE+spin-up (210->300) mirrors the gradual DESCENT+spin-down (300->390)
import mathutils as _mu
def _smooth(t): t=max(0.0,min(1.0,t)); return t*t*t*(t*(6*t-15)+10)      # smootherstep: 0 slope both ends
# board_root = CENTROID of the board -> spins about its own centre. Symmetric ballistic ARC: rises (ease-out
# so it clears the box early yet slows toward the apex) and descends (smootherstep, soft landing) — mirrored.
PACKZ=8.0; PEAKZ=float(os.environ.get("PEAKZ","380.0")); RESTZ=2.0; PACKXY=(-20.0,-50.0)
RISEEND=int(os.environ.get("RISEEND","244"))                   # board reaches its high HOVER by here — clear BEFORE the tray slides out underneath
# ONE continuous velocity curve (no jumps): smooth ease-in-out UP (0 speed off the box), a flat HOVER at the
# top (0 speed), smooth ease-in-out DOWN. Up is faster than down; velocity is 0 at every junction (210/244/300).
def _bz(fr):                                                   # board centroid height along the arc
    if fr<=LIFT: return PACKZ
    if fr<=RISEEND: p=(fr-LIFT)/(RISEEND-LIFT); return PACKZ+(PEAKZ-PACKZ)*_smooth(p)          # RISE: smooth ease-in-out (no pop off the box)
    if fr<=PEAK: return PEAKZ                                                                  # HOVER at the top while the tray exits beneath the raised board
    p=(fr-PEAK)/(END-PEAK); return PEAKZ-(PEAKZ-RESTZ)*_smooth(p)                              # DESCENT: smootherstep -> soft landing
def _bxy(fr):                                                  # drift from the packed spot to centre over the early rise
    q=_smooth(max(0.0,min(1.0,(fr-LIFT)/44.0))); return (PACKXY[0]*(1.0-q),PACKXY[1]*(1.0-q))
board_root.location=(PACKXY[0],PACKXY[1],PACKZ)
board_root.keyframe_insert("location",frame=0); board_root.keyframe_insert("location",frame=LIFT)
for _f in range(LIFT+1,END+1):
    _x,_y=_bxy(_f); board_root.location=(_x,_y,_bz(_f)); board_root.keyframe_insert("location",frame=_f)
# hub = disc centre / hinge origin, offset from the centroid so board_root sits at the folded quarter's
# centre. The offset relaxes to 0 as the board opens (centroid -> disc centre).
UO0=254; UEND0=330                                             # (mirror of the unfold window UO..UEND below)
hub.location=(-100,-100,0); hub.keyframe_insert("location",frame=0); hub.keyframe_insert("location",frame=UO0)
hub.location=(0,0,0); hub.keyframe_insert("location",frame=UEND0)
# board ORIENTATION on board_root: a fast MULTI-AXIS tumble that ramps up, CRANKS AT MAX for ~1.5s, then
# decelerates GRADUALLY (over the whole lowering) resolving to FLAT (pent up) as it lands.
WMAX=float(os.environ.get("WMAX","1.44"))                      # max angular speed (rad/frame ~82deg, 2x; still <pi so no strobe)
RY=float(os.environ.get("SPINY","0.63")); RZ=float(os.environ.get("SPINZ","0.34"))
TAIL=float(os.environ.get("TAIL","2.4"))                       # peak spin is at the APEX; spin-UP and spin-DOWN are mirror images (same gradual character)
def _settle(fr):                                               # spin-DOWN over the descent: 0 at apex -> 1 at rest, ease-OUT tail
    x=max(0.0,min(1.0,(fr-PEAK)/max(1,END-PEAK))); return 1.0-(1.0-x)**TAIL
def _riseup(fr):                                               # spin-UP over the rise: 0 at LIFT -> 1 at apex, the MIRROR of the tail
    y=max(0.0,min(1.0,(fr-LIFT)/max(1,PEAK-LIFT))); return y**TAIL
def _omega(fr):
    if fr<=PEAK: return WMAX*_riseup(fr)                        # INCREASE spinning as it RISES (gradual, mirror)
    return WMAX*(1.0-_settle(fr))                              # DECREASE spinning as it LOWERS (same gradual way)
M=END-LIFT
_ang=[0.0]                                                     # cumulative X-rotation angle (rad), integrating omega per frame
for i in range(1,M+1): _ang.append(_ang[-1]+_omega(LIFT+i))
# LAND EXACTLY FLAT with NO slerp (slerp of a still-spinning quat wiggles/pops at the end): snap the main
# X spin to a whole number of TURNS, and FADE the Y/Z axes to zero across the decel -> ends at identity.
_rawA=_ang[M]; _sx=(2*math.pi*max(1,round(_rawA/(2*math.pi))))/_rawA if _rawA>1e-6 else 1.0
_pe=bpy.context.preferences.edit; _ki=_pe.keyframe_new_interpolation_type; _pe.keyframe_new_interpolation_type='LINEAR'
for i in range(M+1):                                          # ONE key per frame + LINEAR -> exact path, no Bezier stutter
    fr=LIFT+i; ax=_ang[i]*_sx                                  # X angle, rescaled so it ends on a full-turn multiple (flat)
    fade=1.0-_settle(fr)                                       # Y/Z present during the fast spin, unwind to 0 with the same long tail
    q=(_mu.Quaternion((1,0,0),ax)                             # fast barrel roll (X) -> lands on a whole turn
       @ _mu.Quaternion((0,1,0),ax*RY*fade)                   # + Y (fades out)
       @ _mu.Quaternion((0,0,1),ax*RZ*fade))                  # + Z yaw (fades out) -> multi-axis early, single-axis flat landing
    board_root.rotation_quaternion=q; board_root.keyframe_insert("rotation_quaternion",frame=fr)
_pe.keyframe_new_interpolation_type=_ki
def hk(h,rot,fr): h.rotation_euler=rot; h.keyframe_insert("rotation_euler",frame=fr)
# CONTINUOUS cascade unfold: overlapping hinge windows -> one smooth blossom (no staged pause/jolt)
UO=254; UEND=328
W2=(int(os.environ.get("W2S","254")),int(os.environ.get("W2E","304")))   # bottom row leads
W1=(int(os.environ.get("W1S","272")),int(os.environ.get("W1E","318")))   # left-top follows (overlaps)
W3=(int(os.environ.get("W3S","284")),int(os.environ.get("W3E","328")))   # diagonal last (overlaps)
for fr in (0,LIFT): hk(H1,(0,math.pi,0),fr); hk(H2,(math.pi,0,0),fr); hk(H3,(0,math.pi,0),fr)
hk(H2,(math.pi,0,0),W2[0]); hk(H2,(0,0,0),W2[1]); hk(H2,(0,0,0),END)
hk(H1,(0,math.pi,0),W1[0]); hk(H1,(0,0,0),W1[1]); hk(H1,(0,0,0),END)
hk(H3,(0,math.pi,0),W3[0]); hk(H3,(0,0,0),W3[1]); hk(H3,(0,0,0),END)
STK=float(os.environ.get("STK","9.0")); S1=float(os.environ.get("S1","-1")); S2=float(os.environ.get("S2","-4")); S3=float(os.environ.get("S3","-3"))
def pz(o,z,fr): o.location=(0,0,z); o.keyframe_insert("location",frame=fr)   # per-layer z-separation collapses AS each panel swings (aligned -> no pop)
for fr in (0,LIFT): pz(P1,S1*STK,fr); pz(P2,S2*STK,fr); pz(P3,S3*STK,fr)
pz(P2,S2*STK,W2[0]+12); pz(P2,0,W2[1])                       # bottom row holds its lift, collapses late (clear of the top row)
pz(P1,S1*STK,W1[0]); pz(P1,0,W1[1])
pz(P3,S3*STK,W3[0]); pz(P3,0,W3[1])
# tray: rise SLIGHTLY to clear the pieces, then arc LOW along the table around the right and tuck BEHIND the standing box (board is high above -> no collision)
key(tray,0,(BX,BY,0)); key(tray,TRAYGO,(BX,BY,0))
key(tray,TRAYGO+10,(BX,BY,78))                                 # up slightly, over the pieces
key(tray,TRAYGO+22,(560,300,78)); key(tray,TRAYGO+34,(560,660,78)); key(tray,TRAYGO+46,(300,900,78))   # WIDE arc around the right — stays clear of the standing poster (x±169 @ y600)
key(tray,TRAYGO+56,(0,940,78)); key(tray,TRAYGO+64,(0,940,0))  # settle well BEHIND the standing box
# ===== GLORIOUS LOWERING: a PLASMA COLUMN emanates FROM the board (the shader from the actions/rules clips) =====
# faint board self-glow (art stays readable) + a small warm spill light
pbsdf=PENTART.node_tree.nodes["Principled BSDF"]; pimg=next(n for n in PENTART.node_tree.nodes if n.type=='TEX_IMAGE')
PENTART.node_tree.links.new(pimg.outputs["Color"],pbsdf.inputs["Emission Color"])
for fr,v in [(UPCLR,0.0),(CTR,0.03),(LOWER,0.07),(END,0.15),(LAST,0.15)]:        # board self-glow INCREASES the whole time it rotates
    pbsdf.inputs["Emission Strength"].default_value=v; pbsdf.inputs["Emission Strength"].keyframe_insert("default_value",frame=fr)
gl=bpy.data.lights.new("glow",'POINT'); gl.color=(1.0,0.83,0.4); gl.shadow_soft_size=220
glo=bpy.data.objects.new("glow",gl); C().objects.link(glo)                       # warm spill light, PARENTED to the board so it follows (point light = rotation doesn't matter)
glo.parent=board_root; glo.location=(0,0,50); glo.matrix_parent_inverse=_mu.Matrix.Identity(4)
for fr,e in [(UPCLR,0.0),(CTR,0.05e6),(LOWER,0.12e6),(END,0.30e6),(LAST,0.30e6)]:
    gl.energy=e; gl.keyframe_insert("energy",frame=fr)
# the plasma: a tall volume column, EXPONENTIALLY pooled at the base (heavy gravity) evaporating up,
# warm gold, wisps scrolling upward. Base rides the board down so it comes OUT of the board; starts at
# zero and grows tall (but still base-pooled) as the board unfolds.
EXPB=float(os.environ.get("EXPB","0.03"))                       # vertical density = EXPB^height -> strongly exponential pool
PLH=float(os.environ.get("PLH","360")); PLR=float(os.environ.get("PLR","232"))
DSCALE=float(os.environ.get("PLDENS","0.0028")); ESCALE=float(os.environ.get("PLEMIT","0.0315"))  # 0.7x opacity (dialed down); mm units -> long ray paths -> TINY coeffs
bpy.ops.mesh.primitive_cylinder_add(vertices=56,radius=PLR,depth=PLH,location=(0,0,0))
plz=bpy.context.active_object; plz.name="plasma"
for v in plz.data.vertices: v.co.z+=PLH/2.0                    # base at origin -> scale.z grows it UP from the board
plz.data.update(); plz.display_type='WIRE'
pmat=bpy.data.materials.new("plasma"); pmat.use_nodes=True; pnt=pmat.node_tree; pnt.nodes.clear()
pout=pnt.nodes.new("ShaderNodeOutputMaterial"); pv=pnt.nodes.new("ShaderNodeVolumePrincipled")
tc=pnt.nodes.new("ShaderNodeTexCoord"); sep=pnt.nodes.new("ShaderNodeSeparateXYZ")
pnt.links.new(tc.outputs["Generated"],sep.inputs[0])           # height 0(base)..1(top)
expb=pnt.nodes.new("ShaderNodeValue"); expb.outputs[0].default_value=EXPB
prof=pnt.nodes.new("ShaderNodeMath"); prof.operation='POWER'   # EXPB^height: 1 at base -> EXPB at top (gravity pool)
pnt.links.new(expb.outputs[0],prof.inputs[0]); pnt.links.new(sep.outputs["Z"],prof.inputs[1])
mp=pnt.nodes.new("ShaderNodeMapping"); mp.inputs["Scale"].default_value=(0.007,0.007,0.004)
pnt.links.new(tc.outputs["Object"],mp.inputs["Vector"])
nz=pnt.nodes.new("ShaderNodeTexNoise"); nz.inputs["Scale"].default_value=2.2; nz.inputs["Detail"].default_value=2.0
pnt.links.new(mp.outputs["Vector"],nz.inputs["Vector"])
wisp=pnt.nodes.new("ShaderNodeMapRange"); wisp.inputs["From Min"].default_value=0.42; wisp.inputs["From Max"].default_value=0.72
pnt.links.new(nz.outputs["Fac"],wisp.inputs["Value"])
m1=pnt.nodes.new("ShaderNodeMath"); m1.operation='MULTIPLY'; pnt.links.new(prof.outputs[0],m1.inputs[0]); pnt.links.new(wisp.outputs["Result"],m1.inputs[1])
# RADIAL falloff: dense at the axis, clearing to the rim so it's a rounded pool, not a hard cylinder
gc=pnt.nodes.new("ShaderNodeVectorMath"); gc.operation='SUBTRACT'; gc.inputs[1].default_value=(0.5,0.5,0.5); pnt.links.new(tc.outputs["Generated"],gc.inputs[0])
gf=pnt.nodes.new("ShaderNodeVectorMath"); gf.operation='MULTIPLY'; gf.inputs[1].default_value=(1.0,1.0,0.0); pnt.links.new(gc.outputs["Vector"],gf.inputs[0])
gln=pnt.nodes.new("ShaderNodeVectorMath"); gln.operation='LENGTH'; pnt.links.new(gf.outputs["Vector"],gln.inputs[0])
rad=pnt.nodes.new("ShaderNodeMapRange"); rad.inputs["From Min"].default_value=0.10; rad.inputs["From Max"].default_value=0.5; rad.inputs["To Min"].default_value=1.0; rad.inputs["To Max"].default_value=0.0
pnt.links.new(gln.outputs["Value"],rad.inputs["Value"])
rm=pnt.nodes.new("ShaderNodeMath"); rm.operation='MULTIPLY'; pnt.links.new(m1.outputs[0],rm.inputs[0]); pnt.links.new(rad.outputs["Result"],rm.inputs[1])
anim=pnt.nodes.new("ShaderNodeValue")                          # global fade-in
m2=pnt.nodes.new("ShaderNodeMath"); m2.operation='MULTIPLY'; pnt.links.new(rm.outputs[0],m2.inputs[0]); pnt.links.new(anim.outputs[0],m2.inputs[1])
dens=pnt.nodes.new("ShaderNodeMath"); dens.operation='MULTIPLY'; dens.inputs[1].default_value=DSCALE; pnt.links.new(m2.outputs[0],dens.inputs[0])
pnt.links.new(dens.outputs[0],pv.inputs["Density"])
pv.inputs["Color"].default_value=(1.0,0.8,0.36,1.0); pv.inputs["Emission Color"].default_value=(1.0,0.72,0.28,1.0)
emi=pnt.nodes.new("ShaderNodeMath"); emi.operation='MULTIPLY'; emi.inputs[1].default_value=ESCALE; pnt.links.new(m2.outputs[0],emi.inputs[0])
pnt.links.new(emi.outputs[0],pv.inputs["Emission Strength"])
pnt.links.new(pv.outputs[0],pout.inputs["Volume"]); plz.data.materials.append(pmat)
_pe.keyframe_new_interpolation_type='LINEAR'                    # steady scroll -> the plasma KEEPS emanating after landing (doesn't freeze)
mp.inputs["Location"].default_value=(0,0,0); mp.inputs["Location"].keyframe_insert("default_value",index=2,frame=UPCLR)
mp.inputs["Location"].default_value=(0,0,-4.6); mp.inputs["Location"].keyframe_insert("default_value",index=2,frame=LAST)   # scroll wisps up through the very end
_pe.keyframe_new_interpolation_type=_ki
for fr,v in [(UPCLR,0.0),(CTR,0.18),(LOWER,0.45),(END,1.0),(LAST,1.0)]:          # glow INCREASES the whole time it rotates
    anim.outputs[0].default_value=v; anim.outputs[0].keyframe_insert("default_value",frame=fr)
for fr,s in [(UPCLR,0.1),(CTR,0.3),(LOWER,0.55),(END,1.0),(LAST,1.0)]:           # grows taller throughout (still base-pooled via EXPB)
    plz.scale=(1,1,s); plz.keyframe_insert("scale",index=2,frame=fr)
# base FOLLOWS the board along its arc (stays VERTICAL = gravity) so the glow rises off the board the whole time
for fr in range(UPCLR,END+1,2):
    _x,_y=_bxy(fr); plz.location=(_x,_y,_bz(fr)+5.0); plz.keyframe_insert("location",frame=fr)
plz.location=(0,0,RESTZ+5.0); plz.keyframe_insert("location",frame=LAST)

# ================= camera: closed box -> crane out to the hero framing =================
cam_d=bpy.data.cameras.new("Cam"); cam_d.lens=40; cam_d.clip_end=16000
cam=bpy.data.objects.new("Cam",cam_d); C().objects.link(cam); sc.camera=cam
aim=bpy.data.objects.new("aim",None); C().objects.link(aim)
tc=cam.constraints.new('TRACK_TO'); tc.target=aim; tc.track_axis='TRACK_NEGATIVE_Z'; tc.up_axis='UP_Y'
ZS=int(os.environ.get("ZS","334"))                              # start the swoop EARLIER, while the board is still landing
for f,cl,al in [(0,(0,-640,320),(BX,BY,30)),(95,(0,-1010,665),(0,-10,62)),(200,(0,-1370,800),(0,74,52)),(ZS,(0,-1370,800),(0,74,52))]:
    cam.location=cl; cam.keyframe_insert("location",frame=f); aim.location=al; aim.keyframe_insert("location",frame=f)
# (no camera board-tracking — it felt clumsy; the camera holds the hero framing while the board rises/falls)
# SWOOP: the camera STARTS where it has been (hero look), goes DOWN THE SLOPE, then RIGHTS ITSELF for the
# final tail straight into the red ball. It always looks along its direction of travel (tangent). Cubic
# Bezier: P1 = along the hero look dir (start tangent == hero look, no jump); P2,P3 LEVEL at the ball height
# (end tangent level -> rights itself, straight in). Progress eases by a SIGMOID in time (slow start/end).
SWOOPEND=int(os.environ.get("SWOOPEND","521"))                  # end of the APPROVED dive; the into-red push is appended after
_N=SWOOPEND-ZS
# two-segment path: hero look -> DIVE (steep, down at the board) -> right itself, LEVEL into the red ball
_H=_mu.Vector((0,-1370,800)); _D=_mu.Vector((0,60,168)); _R=_mu.Vector((0,470,186))
_TH=_mu.Vector((0,0.888,-0.460))   # hero look dir (27.4deg down) = start tangent, continuous with the held shot
_TL=_mu.Vector((0,1,0))            # level tangent at the dive bottom AND the red-ball end
_vA=(_D-_H).length; _A0=_H; _A1=_H+_TH*(_vA/3.0); _A2=_D-_TL*(_vA/3.0); _A3=_D   # H->D
_vB=(_R-_D).length; _C0=_D; _C1=_D+_TL*(_vB/3.0); _C2=_R-_TL*(_vB/3.0); _C3=_R   # D->R
def _cub(p0,p1,p2,p3,u):
    u=max(0.0,min(1.0,u)); v=1.0-u
    return v*v*v*p0+3*v*v*u*p1+3*v*u*u*p2+u*u*u*p3
_AMID=float(os.environ.get("AMID","0.78"))   # fraction of the path at the dive bottom
def _bez(a):
    a=max(0.0,min(1.0,a))
    if a<_AMID: return _cub(_A0,_A1,_A2,_A3,a/_AMID)
    return _cub(_C0,_C1,_C2,_C3,(a-_AMID)/(1.0-_AMID))
_SIGK=float(os.environ.get("SIGK","4.5"))
def _sig(t):
    a0=1.0/(1.0+math.exp(_SIGK*0.5)); a1=1.0/(1.0+math.exp(-_SIGK*0.5))
    return (1.0/(1.0+math.exp(-_SIGK*(t-0.5)))-a0)/(a1-a0)
# EASING: keep the APPROVED accel+dive (logistic over NACC frames) byte-identical, then graft a LONG
# velocity-matched decel tail -> gentle stop into the red. Seam at a=ASEAM (past the dive bottom 0.78).
_NACC=int(os.environ.get("NACC","162"))            # frames of the unchanged accel+dive (=old ZS..496 span)
_ASEAM=float(os.environ.get("ASEAM","0.86"))       # graft point (well past the dive, on the descent)
_ks=1
while _ks<_NACC and _sig(_ks/_NACC)<_ASEAM: _ks+=1
_FS=ZS+_ks; _as=_sig(_ks/_NACC); _vs=_as-_sig((_ks-1)/_NACC)   # seam frame, a, and its velocity (Δa/frame)
_TT=max(1,SWOOPEND-_FS); _m0=_vs*_TT                          # tail length; Hermite start tangent = seam velocity
# seg C: continue the SAME Bezier track PAST the red ball INTO the cover's crimson cell — C1-continuous with
# the dive's end tangent, so the camera just keeps zooming in on one curve (tangent-following aim, no re-aim).
_segR=_bez(1.0); _segRt=(_bez(1.0)-_bez(0.99)).normalized()
_CE=_mu.Vector((-5.0,536.0,169.0)); _CEt=_mu.Vector((0.0,1.0,0.0))    # camera end (~34mm off the crimson), looking +y into it
_clen=(_CE-_segR).length; _Cc0=_segR; _Cc1=_segR+_segRt*(_clen/3.0); _Cc2=_CE-_CEt*(_clen/3.0); _Cc3=_CE
_AEXT=1.0+float(os.environ.get("EXT","0.35"))
def _path(a):
    if a<=1.0: return _bez(a)
    return _cub(_Cc0,_Cc1,_Cc2,_Cc3,(a-1.0)/(_AEXT-1.0))
# ONE continuous ease: accel+dive UNCHANGED -> decel toward the red ball but HAND OFF a small velocity (_V521,
# no stop) -> keep decelerating into the crimson to a full stop at LAST. No pause, no re-aim, no stitch.
_V521=float(os.environ.get("V521","0.0025"))
def _ease(fr):
    if fr<=_FS: return _sig((fr-ZS)/_NACC)
    if fr<=SWOOPEND:
        s=(fr-_FS)/float(SWOOPEND-_FS); m0=_vs*(SWOOPEND-_FS); m1=_V521*(SWOOPEND-_FS)
        return (2*s**3-3*s**2+1)*_as+(s**3-2*s**2+s)*m0+(-2*s**3+3*s**2)*1.0+(s**3-s**2)*m1
    s=(fr-SWOOPEND)/float(LAST-SWOOPEND); m0=_V521*(LAST-SWOOPEND)
    return (2*s**3-3*s**2+1)*1.0+(s**3-2*s**2+s)*m0+(-2*s**3+3*s**2)*_AEXT
_pe0=bpy.context.preferences.edit; _ki0=_pe0.keyframe_new_interpolation_type; _pe0.keyframe_new_interpolation_type='LINEAR'
for fr in range(ZS+1,LAST+1):
    a=_ease(fr); pos=_path(a); tp=_path(a+0.01)-_path(a-0.01); look=pos+tp*25.0   # tangent = direction of travel, all the way in
    cam.location=(pos.x,pos.y,pos.z); cam.keyframe_insert("location",frame=fr)
    aim.location=(look.x,look.y,look.z); aim.keyframe_insert("location",frame=fr)
_pe0.keyframe_new_interpolation_type=_ki0
# ================= ENDING — IN-SCENE, SAME CAMERA (no stitch): the words SURFACE from the red on the cover =====
# The camera keeps advancing PAST the dive with a SUBTLE push toward the red field (velocity continuous from the
# dive's near-stop, ease-in -> no jump), aim straight ahead on the field (+y, on-axis so the word plane is square
# to the camera -> no keystone). EAT/MOVE/GROW word+symbol pairs surface from the red (clarify red->white,
# gentle float that LOCKS IN). This is the reveal ending built INTO this scene — one camera, one render.
_font=bpy.data.fonts.load(os.path.join(P,"inputs/JTEnergyVF.ttf")); _SYMDIR=os.path.join(P,"inputs/sym_png"); _WY=569.0
def _wordobj(txt,size):
    cu=bpy.data.curves.new(txt,'FONT'); cu.body=txt; cu.font=_font; cu.align_x='CENTER'; cu.align_y='CENTER'; cu.size=size
    o=bpy.data.objects.new(txt,cu); C().objects.link(o); o.rotation_euler=(math.pi/2,0,0); o.visible_shadow=False
    m,fade=_fademat(txt+"_m"); cu.materials.append(m); return o,fade
def _symobj(name,diam):
    img=bpy.data.images.load(os.path.join(_SYMDIR,name+".png")); iw,ih=img.size
    w,h=(diam,diam*ih/iw) if iw>=ih else (diam*iw/ih,diam)
    bpy.ops.mesh.primitive_plane_add(size=1); o=bpy.context.active_object; o.name=name+"_sym"
    o.rotation_euler=(math.pi/2,0,0); o.scale=(w,h,1); o.visible_shadow=False
    m,fade=_fademat(name+"_sm",image=img); o.data.materials.append(m); return o,fade
_WSZ=float(os.environ.get("WSZ","3.3")); _SYSZ=float(os.environ.get("SYSZ","4.2"))   # tiny — pushed deep into the small crimson so it's PURE red (no yellow ring); words still fill the frame
def _wwidth(txt):
    ob,_=_wordobj(txt,_WSZ); bpy.context.view_layer.update()
    dg=bpy.context.evaluated_depsgraph_get(); w=ob.evaluated_get(dg).dimensions.x
    bpy.data.objects.remove(ob,do_unlink=True); return w
_maxw=max(_wwidth(t) for t in ("EAT","MOVE","GROW")); _CXC=-5.0    # centred on the crimson cell (camera is on-axis there)
_COLSP=_maxw+float(os.environ.get("COLGAP","3.5")); _COLX=[_CXC-_COLSP,_CXC,_CXC+_COLSP]   # spacing = widest word + gap -> no overlap
_WZ=172.0; _SZ=166.0                                            # tight vertical pair (the zoomed frame is short)
_plan=[("EAT",0,_WZ,"w"),("eat",0,_SZ,"s"),("MOVE",1,_WZ,"w"),("move",1,_SZ,"s"),("GROW",2,_WZ,"w"),("grow",2,_SZ,"s")]
_ELEAD=int(os.environ.get("ELEAD","115")); _ESTAG=int(os.environ.get("ESTAG","14")); _EFADE=int(os.environ.get("EFADE","36"))
_ESETTLE=int(os.environ.get("ESETTLE","64")); _LOCKMIN=0.13; _FLX=0.3; _FLZ=0.45; _FTZ=3.6; _FTX=4.7
def _smoother(t): t=max(0.0,min(1.0,t)); return t*t*t*(t*(t*6-15)+10)
_pe=bpy.context.preferences.edit; _ki2=_pe.keyframe_new_interpolation_type; _pe.keyframe_new_interpolation_type='LINEAR'
for _i,(label,col,zc,kind) in enumerate(_plan):
    x=_COLX[col]; ob,fade=(_wordobj(label,_WSZ) if kind=="w" else _symobj(label,_SYSZ))
    t0=SWOOPEND+_ELEAD+_i*_ESTAG; ph=_i*1.7
    for fr in range(SWOOPEND+1,LAST+1):
        f=_smoother((fr-t0)/_EFADE) if fr>=t0 else 0.0
        fade.outputs[0].default_value=f; fade.outputs[0].keyframe_insert("default_value",frame=fr)
        lock=_smoother((fr-t0)/_ESETTLE) if fr>=t0 else 0.0; amp=_LOCKMIN+(1.0-_LOCKMIN)*(1.0-lock)
        fx=amp*_FLX*math.sin(2*math.pi*(fr/24.0)/_FTX+ph); fz=amp*_FLZ*math.sin(2*math.pi*(fr/24.0)/_FTZ+ph*1.3)
        ob.location=(x+fx,_WY,zc+fz); ob.keyframe_insert("location",frame=fr)
# (the camera into-red motion is now part of the ONE continuous track above — no separate/appended move)
_pe.keyframe_new_interpolation_type=_ki2

sc.frame_start=int(os.environ.get("FIRST","0")); sc.frame_end=LAST; sc.render.image_settings.file_format='PNG'

# ================= collision VALIDATION (VALIDATE=1) — does anything fly through anything while moving? =================
# World AABBs for box parts/pieces; a rotation-invariant BOUNDING SPHERE for the board (centre=hub pivot,
# radius=furthest panel vertex) so its tumble is checked without caring about orientation.
def _run_validate():
    import mathutils
    dg=bpy.context.evaluated_depsgraph_get()
    panels=[P0,P1,P2,P3]; pieces=[d["obj"] for d in PLACED]
    def wc(o): m=o.matrix_world; return [m@mathutils.Vector(c) for c in o.bound_box]
    def aabb(o):
        cs=wc(o); return (min(c.x for c in cs),min(c.y for c in cs),min(c.z for c in cs),
                          max(c.x for c in cs),max(c.y for c in cs),max(c.z for c in cs))
    def box_pen(a,b):                                            # mm of interpenetration on the tightest axis (>0 = overlapping)
        return min(min(a[3],b[3])-max(a[0],b[0]), min(a[4],b[4])-max(a[1],b[1]), min(a[5],b[5])-max(a[2],b[2]))
    def board_sphere():
        c=board_root.matrix_world.translation.copy(); r=0.0      # tumble pivot = board centroid (rotation-invariant)
        for o in panels:
            oe=o.evaluated_get(dg); m=oe.matrix_world
            for v in oe.data.vertices: r=max(r,(m@v.co-c).length)
        return c,r
    def sph_pen(c,r,b):                                          # mm the sphere pokes into an AABB (>0 = overlapping)
        dx=max(b[0]-c.x,0,c.x-b[3]); dy=max(b[1]-c.y,0,c.y-b[4]); dz=max(b[2]-c.z,0,c.z-b[5])
        return r-math.sqrt(dx*dx+dy*dy+dz*dz)
    def panel_hit(A,B,mx=14.0,mz=0.4):                          # any vertex of A poking INSIDE B's thin slab (away from shared hinge edges)?
        bb=B.bound_box; bmn=[min(c[i] for c in bb) for i in range(3)]; bmx=[max(c[i] for c in bb) for i in range(3)]
        Ae=A.evaluated_get(dg); Am=Ae.matrix_world; Bi=B.evaluated_get(dg).matrix_world.inverted()
        for v in Ae.data.vertices:
            p=Bi@(Am@v.co)
            if bmn[0]+mx<p.x<bmx[0]-mx and bmn[1]+mx<p.y<bmx[1]-mx and bmn[2]+mz<p.z<bmx[2]-mz: return True
        return False
    PEN=4.0                                                      # ignore <4mm grazing (numeric slop / resting contact)
    hits={}                                                      # label -> set(frames)
    _worst=[1e9,-1]                                              # worst board-below-table clearance (mm), frame
    def note(lbl,f): hits.setdefault(lbl,set()).add(f)
    for f in range(0,LAST+1,2):
        sc.frame_set(f); dg=bpy.context.evaluated_depsgraph_get()
        La,Ta=aabb(lid),aabb(tray)
        if box_pen(La,Ta)>PEN: note("lid×tray",f)
        c,r=board_sphere(); Ba=None
        if sph_pen(c,r,Ta)>PEN: note("board×tray",f)
        if sph_pen(c,r,La)>PEN: note("board×lid",f)
        for pc in pieces:
            if sph_pen(c,r,aabb(pc))>PEN: note("board×piece",f); break
        for a in range(4):                                      # do the 4 board panels pass THROUGH each other during the unfold?
            for b in range(4):
                if a!=b and panel_hit(panels[a],panels[b]): note("panel P%d→P%d"%(a,b),f)
        if f>=240:                                              # does the board dip BELOW the table/bowl surface? (needs the carved well)
            for o2 in panels:
                oe=o2.evaluated_get(dg); m=oe.matrix_world
                for v in oe.data.vertices:
                    w=m@v.co; clr=w.z-(bowlz(math.hypot(w.x,w.y))-0.15)
                    if clr<_worst[0]: _worst[0]=clr; _worst[1]=f
                    if clr<-PEN: note("board↓table",f)
    def ranges(fs):                                             # compress frame set -> "a-b, c-d"
        fs=sorted(fs); out=[]
        for x in fs:
            if out and x-out[-1][1]<=2: out[-1][1]=x
            else: out.append([x,x])
        return ", ".join(f"{a}" if a==b else f"{a}-{b}" for a,b in out)
    print("\n===== COLLISION REPORT (interpenetration > %gmm) ====="%PEN)
    print("(expected: things packed IN the box overlap until they lift — board is packed until f~%d)"%LIFT)
    if not hits: print("  clean — no interpenetration anywhere")
    for lbl in ("lid×tray","board×tray","board×lid","board×piece"):
        if lbl in hits: print(f"  {lbl:12s}: frames {ranges(hits[lbl])}")
    for lbl in sorted(l for l in hits if l.startswith("panel")):
        print(f"  {lbl:12s}: frames {ranges(hits[lbl])}   <-- panels passing through each other")
    if "board↓table" in hits: print(f"  board↓table : frames {ranges(hits['board↓table'])}   <-- board pokes through the table (deepen/widen the bowl)")
    print(f"  bowl clearance: worst {_worst[0]:.0f}mm @ f{_worst[1]} (RBOWL={RBOWL:.0f} DBOWL={DBOWL:.0f}; negative = board through table)")
    print("=====================================================\n")

def _measure():                                                 # diagnose board-sphere vs tray-path clearance
    import mathutils
    for f in [210,220,232,242,248,254,266,280,300]:
        sc.frame_set(f); dg=bpy.context.evaluated_depsgraph_get()
        c=board_root.matrix_world.translation.copy(); r=0.0
        for o in (P0,P1,P2,P3):
            oe=o.evaluated_get(dg); m=oe.matrix_world
            for v in oe.data.vertices: r=max(r,(m@v.co-c).length)
        tm=tray.matrix_world; cs=[tm@mathutils.Vector(cc) for cc in tray.bound_box]
        ta=(min(p.x for p in cs),min(p.y for p in cs),min(p.z for p in cs),max(p.x for p in cs),max(p.y for p in cs),max(p.z for p in cs))
        dx=max(ta[0]-c.x,0,c.x-ta[3]); dy=max(ta[1]-c.y,0,c.y-ta[4]); dz=max(ta[2]-c.z,0,c.z-ta[5])
        gap=math.sqrt(dx*dx+dy*dy+dz*dz)-r
        print("f%d root=(%.0f,%.0f,%.0f) R=%.0f | tray z[%.0f..%.0f] x[%.0f..%.0f] y[%.0f..%.0f] | gap=%.0fmm %s"%(
            f,c.x,c.y,c.z,r,ta[2],ta[5],ta[0],ta[3],ta[1],ta[4],gap,"OVERLAP" if gap<0 else "clear"))
if os.environ.get("MEASURE"):
    _measure()
elif os.environ.get("VALIDATE"):
    _run_validate()
elif not os.environ.get("NORENDER"):
    single=os.environ.get("FRAME")
    if single:
        sc.frame_set(int(single)); sc.render.filepath=f"{FR}/single_{int(single):04d}"; bpy.ops.render.render(write_still=True); print("frame ->",sc.render.filepath)
    else:
        sc.render.filepath=f"{FR}/f"; bpy.ops.render.render(animation=True); print("anim ->",FR)
print("components:",len(PLACED)+1,"objects")
