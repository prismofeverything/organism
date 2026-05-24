"""Measure how deeply FOOD pieces nest, with correct TELESCOPING physics.

Two coaxial solids of revolution collide iff their meridian cross-sections (r>=0, z)
overlap. So we build the meridian polygon (thin-shell body + peg, minus socket), then
binary-search the smallest vertical offset D at which the upper copy stops overlapping
the lower one. That handles a piece sliding down INTO the bowl below (not just stacking).

    nested_fraction = 1 - D / H          height of N stacked = H + (N-1)*D

    .venv/bin/python pieces/stack_food.py [path/to.profile.json]
"""
import sys, json
from pathlib import Path
import numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, box
from shapely.affinity import translate

HERE = Path(__file__).resolve().parent
SC = Path(sys.argv[1]) if len(sys.argv) > 1 else HERE / "renders" / "food" / "FOOD_new.profile.json"
d = json.loads(SC.read_text())
P = [(float(r), float(z)) for r, z in d["prof"]]
floor_z, peg_tip = d["floor_z"], d["peg_tip"]
ridge_or, socket_or, socket_ceiling = d["ridge_or"], d["socket_or"], d["socket_ceiling"]

# meridian solid: thin-shell body  +  peg (on the floor)  -  socket (notch from z=0)
body = Polygon(P)
if not body.is_valid:
    body = body.buffer(0)
peg    = box(0.0, floor_z - 1.0, ridge_or, peg_tip)
socket = box(0.0, 0.0,            socket_or, socket_ceiling)
solid  = body.union(peg).difference(socket)
H = solid.bounds[3] - solid.bounds[1]

def overlaps(D, eps=2e-2):
    return solid.intersection(translate(solid, yoff=D)).area > eps

lo, hi = 0.0, H                       # overlaps(small)=True, overlaps(big)=False
while hi - lo > 0.01:
    mid = (lo + hi) / 2
    (lo := mid) if overlaps(mid) else (hi := mid)
D = hi
nested = 1 - D / H

print(f"single height H = {H:.2f} mm   (rim {solid.bounds[3]:.2f})")
print(f"rest offset  D = {D:.2f} mm")
print(f"NESTED FRACTION = {nested*100:.0f}%   (target >= 50%)")
for n in (2, 3, 4):
    print(f"  {n} stacked = {H + (n-1)*D:5.1f} mm  (vs {n}xH = {n*H:.1f})")

# --- render the stack (4 pieces) ------------------------------------------
xs, ys = solid.exterior.xy
xs, ys = np.array(xs), np.array(ys)
holes = [np.array(r.xy) for r in solid.interiors]
fig, ax = plt.subplots(figsize=(6, 8.5))
cols = ["#7a3f0e", "#a85a12", "#cf8430", "#ecaf57"]
for n in range(4):
    yo = n * D
    ax.fill(np.r_[xs, -xs[::-1]], np.r_[ys, ys[::-1]] + yo, color=cols[n], alpha=.55, lw=0)
    for hx, hy in holes:
        ax.fill(np.r_[hx, -hx[::-1]], np.r_[hy, hy[::-1]] + yo, color="white", lw=0)
ax.axhline(H, color="0.6", ls=":", lw=.8)
ax.annotate(f"1 = {H:.1f} mm", xy=(13.5, H), fontsize=8, va="center")
ax.annotate(f"4 stacked = {H+3*D:.1f}\n(vs 4x = {4*H:.1f})", xy=(13.5, H + 3 * D),
            fontsize=9, va="center", weight="bold")
ax.set_aspect("equal"); ax.set_xlabel("radius (mm)"); ax.set_ylabel("height (mm)")
ax.set_title(f"Stacked FOOD — nested {nested*100:.0f}%/piece  (D={D:.1f}, H={H:.1f})", weight="bold")
fig.tight_layout()
p = SC.with_name("FOOD_stack_xsec.png"); fig.savefig(p, dpi=130); print("wrote", p)
