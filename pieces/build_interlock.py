"""How much does interlocking tipped-piece columns buy, using REAL MESH COLLISION (not bounding
boxes)? A column = EAT+GROW+MOVE tipped end-to-end. Compare column pitch when packed:
  (a) side-by-side straight, (b) alternate columns REVERSED (M-G-E) so bulges meet gaps,
  (c) alternate columns brick-SHIFTED along their length. Collision via mesh point-containment.
  ../.venv/bin/python pieces/build_interlock.py
"""
import numpy as np, trimesh, math
from trimesh.transformations import rotation_matrix as R
HERE_pieces={t:trimesh.load(f"out/{t}_sculpt_graft.obj",force='mesh') for t in ["EAT","MOVE","GROW"]}
Rx=R(-math.pi/2,[1,0,0])
P={t:m.copy() for t,m in HERE_pieces.items()}
for m in P.values(): m.apply_transform(Rx)                     # tip on side (lie down)
DIMy={t:P[t].extents[1] for t in P}                            # length along Y (former height)
GAP=2.0

def column(order, flip, xoff, yoff):
    parts=[]; y=yoff
    for t in order:
        c=P[t].copy()
        if flip: c.apply_transform(R(math.pi,[0,0,1]))
        c.apply_translation([xoff - c.bounds[:,0].mean(), y - c.bounds[0,1], 0])  # seat at y (top), x centered
        parts.append(c); y-=DIMy[t]+GAP
    return trimesh.util.concatenate(parts)

from scipy.spatial import cKDTree
def voxpts(m,v=1.6):
    try:
        p=m.voxelized(v).points
        if len(p)>10: return p
    except Exception: pass
    return m.sample(4000)
def overlap(Apts,Bpts,tol=1.8):
    if len(Apts)==0 or len(Bpts)==0: return False
    d,_=cKDTree(Apts).query(Bpts, distance_upper_bound=tol)
    return bool(np.isfinite(d).any())

def min_pitch(orderA,orderB,flipB,yshiftB,lo=8,hi=46):
    Apts=voxpts(column(orderA,False,0,0)); Bpts0=voxpts(column(orderB,flipB,0,yshiftB))
    for _ in range(24):
        mid=(lo+hi)/2
        if overlap(Apts, Bpts0+[mid,0,0]): lo=mid
        else: hi=mid
    return hi
# sanity: columns nearly on top of each other MUST collide; far apart must NOT
_Ap=voxpts(column(fwd,False,0,0) if False else column(["EAT","GROW","MOVE"],False,0,0))
print("sanity: overlap@5mm =", overlap(_Ap,_Ap+[5,0,0]), " overlap@42mm =", overlap(_Ap,_Ap+[42,0,0]))

fwd=["EAT","GROW","MOVE"]; rev=["MOVE","GROW","EAT"]
base = max(P[t].extents[0] for t in P)+GAP
print(f"single column width (bbox) = {base:.1f} mm  -> straight 4-col compartment = {4*base:.0f} mm wide")
p_rev  = min_pitch(fwd, rev, False, 0)
p_flip = min_pitch(fwd, fwd, True, 0)
p_brick= min_pitch(fwd, fwd, False, DIMy["EAT"]/1.5)
for name,p in [("reversed-order alt cols",p_rev),("flipped alt cols",p_flip),("brick-shifted alt cols",p_brick)]:
    print(f"  {name:26s} pitch {p:5.1f} mm  ({4*p:.0f} mm/compartment, {100*(1-p/base):.0f}% tighter)")
best=min(p_rev,p_flip,p_brick)
print(f"\nBEST interlock pitch {best:.1f} vs {base:.1f} bbox -> compartment {4*best:.0f} mm wide "
      f"(saves {4*(base-best):.0f} mm/compartment, {100*(1-best/base):.0f}%)")
