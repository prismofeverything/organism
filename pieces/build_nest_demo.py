"""Any-shape walls: nest the REAL piece silhouettes (star/spiral/clover) instead of their
bounding circles. Measures each silhouette's area and finds how tightly EAT stars interdigitate
(points into a neighbour's gaps), then compares footprint vs hex-of-circles. Draws it.
  ../.venv/bin/python pieces/build_nest_demo.py
"""
import os, math, numpy as np, trimesh
from shapely.geometry import Polygon
from shapely import affinity
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Circle
HERE = os.path.dirname(os.path.abspath(__file__))

def silhouette(name, z=6.5):
    m = trimesh.load(f"{HERE}/out/{name}_mindisk.obj", force="mesh")
    sec = m.section(plane_origin=[0,0,z], plane_normal=[0,0,1])
    V = sec.vertices[:, :2]
    # largest loop
    best = None
    for ent in sec.entities:
        pts = V[ent.points]
        if len(pts) >= 4:
            poly = Polygon(pts).buffer(0)
            if poly.area > (best.area if best else 0): best = poly
    c = best.centroid; return affinity.translate(best, -c.x, -c.y)

sil = {n: silhouette(n) for n in ["EAT","MOVE","GROW"]}
CIRC = math.pi*18.5**2
print("silhouette areas vs Ø37 bounding circle (%.0f mm²):" % CIRC)
for n,p in sil.items(): print(f"  {n:5s} {p.area:5.0f} mm²  = {100*p.area/CIRC:.0f}% of the circle")

# tightest interdigitated pitch for EAT stars (alternate rows rotated 36°) via shapely
E = sil["EAT"]; E2 = affinity.rotate(E, 36)
def touch_dx(a, b, lo=15, hi=40):
    for _ in range(30):
        mid=(lo+hi)/2
        (lo:=mid) if a.intersects(affinity.translate(b, mid, 0)) else (hi:=mid)
    return hi
dx = touch_dx(E, E)                                   # same-orientation row pitch
rowdy = touch_dx(E, E2, 12, 40) * 0  # placeholder
# vertical pitch between a row and a 36°-rotated row nested into its gaps
def touch_dy(a,b,lo=10,hi=40):
    for _ in range(30):
        mid=(lo+hi)/2
        (lo:=mid) if a.intersects(affinity.translate(b,0,mid)) else (hi:=mid)
    return hi
dy = touch_dy(E, E2)
print(f"\nEAT star: same-row pitch {dx:.1f} mm, interdigitated row pitch {dy:.1f} mm (vs Ø37 circle pitch 39)")

# footprint for 60 EAT-like at nested pitch (8 cols) vs hex circles
cols=8; nrows=math.ceil(60/cols)
nest_w, nest_d = cols*dx, nrows*dy
hexpitch=39; hex_w, hex_d = cols*hexpitch, math.ceil(60/cols)*hexpitch*math.sqrt(3)/2 + hexpitch
print(f"nested silhouettes: {nest_w:.0f} x {nest_d:.0f} = {nest_w*nest_d/100:.0f} cm²")
print(f"hex circles:        {hex_w:.0f} x {hex_d:.0f} = {hex_w*hex_d/100:.0f} cm²   -> nesting saves {100*(1-nest_w*nest_d/(hex_w*hex_d)):.0f}%")

# ---- draw: hex circles vs nested EAT stars ----
fig, axs = plt.subplots(1,2, figsize=(14,6.5))
ax=axs[0]
i=0;row=0
while i<60:
    nc=8 if row%2==0 else 7; off=0 if row%2==0 else 19.5
    for c in range(nc):
        if i>=60:break
        ax.add_patch(Circle((c*39+off,-row*39*math.sqrt(3)/2),18.5,fc="#5aa0b4",ec="#222",lw=.5)); i+=1
    row+=1
ax.set_title(f"hex circles (bounding)\n{hex_w:.0f}×{hex_d:.0f} mm", weight="bold")
ax=axs[1]
for r in range(nrows):
    for c in range(cols):
        p = affinity.rotate(E, 36 if r%2 else 0)
        p = affinity.translate(p, c*dx + (dx/2 if r%2 else 0), -r*dy)
        xs,ys=p.exterior.xy; ax.fill(xs,ys,fc="#cf5b52",ec="#222",lw=.5)
ax.set_title(f"nested silhouettes (any-shape wells)\n{cols*dx:.0f}×{nrows*dy:.0f} mm  "
             f"−{100*(1-nest_w*nest_d/(hex_w*hex_d)):.0f}%", weight="bold")
for ax in axs: ax.set_aspect("equal"); ax.axis("off"); ax.autoscale()
fig.suptitle("Any-shape walls: nesting the real silhouettes beats packing their bounding circles", weight="bold", fontsize=13)
fig.tight_layout()
p="/tmp/claude-1000/-home-youdonotexist-code-organism/bbf7149e-6408-42a3-bcf9-0890d4f37841/scratchpad/nest_demo.png"
fig.savefig(p,dpi=120); print("wrote",p)
