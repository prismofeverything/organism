"""DESIGN the nested-silhouette insert. For each piece type, find the tightest interlocking
lattice (alternate rows rotated by the fold half-angle, clearance added for real walls). Lay out
piece zones + disk zones + round stacks (food/token/platform) in ONE layer above the folded board,
and lock the box. Draws the actual pockets.
  ../.venv/bin/python pieces/build_insert_nested.py
"""
import os, math, numpy as np, trimesh
from shapely.geometry import Polygon
from shapely import affinity
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Circle
HERE=os.path.dirname(os.path.abspath(__file__))
CLR=1.5                       # wall/draft clearance (mm) added around each pocket
FOLD={"EAT":5,"MOVE":3,"GROW":4}
PCOL={"EAT":"#cf5b52","MOVE":"#5aa0b4","GROW":"#8a6bb0"}

def silhouette(name,z=6.5):
    m=trimesh.load(f"{HERE}/out/{name}_mindisk.obj",force="mesh")
    sec=m.section(plane_origin=[0,0,z],plane_normal=[0,0,1]); V=sec.vertices[:,:2]; best=None
    for ent in sec.entities:
        pts=V[ent.points]
        if len(pts)>=4:
            q=Polygon(pts).buffer(0)
            if q.area>(best.area if best else 0): best=q
    c=best.centroid; return affinity.translate(best,-c.x,-c.y)
SIL={n:silhouette(n) for n in FOLD}

def touch(a,b,axis,lo=8,hi=45):
    for _ in range(28):
        m=(lo+hi)/2; t=(m,0) if axis==0 else (0,m)
        (lo:=m) if a.intersects(affinity.translate(b,*t)) else (hi:=m)
    return hi
def lattice(name):
    half=180.0/FOLD[name]
    P=SIL[name].buffer(CLR); P2=affinity.rotate(P,half)
    return touch(P,P,0), touch(P,P2,1), half            # px (same row), py (interdigitated), half-angle
LAT={n:lattice(n) for n in FOLD}

def zone(name,n,cols):
    px,py,half=LAT[name]; rows=math.ceil(n/cols); place=[]
    for i in range(n):
        r,c=divmod(i,cols); rot=half if r%2 else 0
        x=c*px+(px/2 if r%2 else 0); y=-r*py; place.append((x,y,rot))
    ys=SIL[name].bounds; ext=ys[3]-ys[1]
    return place, cols*px+px/2, rows*py+ext-py

# ---- zones: 20 pieces + short disk-stacks (15, one per player-type) per type ----
def draw_zone(ax,name,place,ox,oy,alpha=1):
    for x,y,rot in place:
        p=affinity.rotate(SIL[name],rot); p=affinity.translate(p,ox+x,oy+y)
        xs,ys=p.exterior.xy; ax.fill(xs,ys,fc=PCOL[name],ec="#222",lw=.5,alpha=alpha)

fig,ax=plt.subplots(figsize=(11,11))
cur_x,cur_y,rowh=0,0,0; placed=[]
def put(name,n,cols,label):
    global cur_x,cur_y,rowh
    pl,w,h=zone(name,n,cols)
    if cur_x+w>330 and cur_x>0: cur_y-=rowh+16; cur_x=0; rowh=0
    draw_zone(ax,name,pl,cur_x,cur_y)
    ax.text(cur_x+w/2,cur_y+14,label,ha="center",fontsize=8,weight="bold")
    placed.append((cur_x,cur_y,w,h)); cur_x+=w+14; rowh=max(rowh,h); return w,h
# piece zones
for t in ["EAT","MOVE","GROW"]: put(t,20,5,f"{t} ×20")
# disk zones (60 disks -> 15 stacks of 4, silhouette footprint, short)
for t in ["EAT","MOVE","GROW"]: put(t,5,5,f"{t} disk-stacks ×5")

# round stacks: food(8) token(5) platform(5) — hex wells in the remaining strip
def round_wells(ax,n,dia,ox,oy,cols,col):
    px=dia+3; rows=math.ceil(n/cols)
    for i in range(n):
        r,c=divmod(i,cols); ax.add_patch(Circle((ox+c*px+(px/2 if r%2 else 0),oy-r*px*.87),dia/2,fc=col,ec="#222",lw=.5))
    return cols*px, rows*px*.87
cur_y-=rowh+20; cur_x=0
w,h=round_wells(ax,8,28,cur_x,cur_y,4,"#d99a3a"); ax.text(cur_x+w/2,cur_y+12,"food ×8 stacks",ha="center",fontsize=8,weight="bold"); cur_x+=w+16
w2,_=round_wells(ax,5,30,cur_x,cur_y,5,"#b9b9c2"); ax.text(cur_x+w2/2,cur_y+12,"tokens ×5",ha="center",fontsize=8,weight="bold"); cur_x+=w2+16
w3,_=round_wells(ax,5,37,cur_x,cur_y,5,"#9a8fb5"); ax.text(cur_x+w3/2,cur_y+12,"platforms ×5",ha="center",fontsize=8,weight="bold")

# bounding box of the layout
xs=[p[0] for p in placed]+[0]; xe=[p[0]+p[2] for p in placed]+[cur_x+w3]
ys=[p[1]+p[3] for p in placed]+[cur_y-40]; ye=[p[1] for p in placed]+[14]
LW=max(xe)-min(xs); LH=max(ye)-min(ys)
FW=max(LW,270); FH=max(LH,270)                          # board is the floor
BOARD_H,LAYER_H,LID=14,60,8
EW,ED,EH=FW+6,FH+6,BOARD_H+LAYER_H+LID
ax.set_title(f"NESTED insert (one layer over the folded board)\n"
             f"box  {EW:.0f} × {ED:.0f} × {EH:.0f} mm   ({EW/25.4:.1f} × {ED/25.4:.1f} × {EH/25.4:.1f} in)   "
             f"clearance {CLR}mm/pocket",weight="bold",fontsize=12)
ax.set_aspect("equal"); ax.axis("off"); ax.autoscale()
fig.tight_layout()
p="/tmp/claude-1000/-home-youdonotexist-code-organism/bbf7149e-6408-42a3-bcf9-0890d4f37841/scratchpad/insert_nested.png"
fig.savefig(p,dpi=120); print("wrote",p)
for t in FOLD: print(f"{t}: pitch {LAT[t][0]:.1f}×{LAT[t][1]:.1f} mm (fold {FOLD[t]})")
print(f"layout {LW:.0f}×{LH:.0f} mm -> BOX {EW:.0f}×{ED:.0f}×{EH:.0f} mm ({EW/25.4:.1f}×{ED/25.4:.1f}×{EH/25.4:.1f} in)")
