"""Verify + top-down preview of the food plate STL (trimesh + matplotlib, no Blender).
Run:  .venv/bin/python food_plate_preview.py"""
import os, trimesh, numpy as np, matplotlib
matplotlib.use("Agg"); import matplotlib.pyplot as plt
HERE = os.path.dirname(os.path.abspath(__file__))
STL  = os.path.join(HERE, "renders/food/food_plate_ss.stl")
OUT  = os.path.join(HERE, "renders/food/food_plate_ss_top.png")

m = trimesh.load(STL)
parts = m.split(only_watertight=False)
b = np.round(m.bounds, 1)
print("bodies: %d  bounds(mm) x[%g,%g] y[%g,%g] z[%g,%g]" %
      (len(parts), b[0,0], b[1,0], b[0,1], b[1,1], b[0,2], b[1,2]))
print("watertight: %d/%d   euler==2: %d/%d" %
      (sum(p.is_watertight for p in parts), len(parts),
       sum(p.euler_number == 2 for p in parts), len(parts)))

labels = {(-40,-20):"snap", (0,-20):"snap", (40,-20):"snap",
          (-40, 20):"slip", (0, 20):"slip", (40, 20):"slip"}
fig, ax = plt.subplots(figsize=(7, 6))
for bed, c in [(256, '0.8'), (220, '0.6')]:
    ax.add_patch(plt.Rectangle((-bed/2, -bed/2), bed, bed, fill=False, ec=c, ls='--'))
    ax.text(-bed/2+2, bed/2-9, f"{bed}mm bed", color=c, fontsize=8)
rng = np.random.default_rng(0)
for p in parts:
    v = p.vertices; idx = rng.choice(len(v), min(1500, len(v)), replace=False)
    ax.scatter(v[idx,0], v[idx,1], s=1, alpha=0.25)
for (x, y), lab in labels.items():
    ax.annotate(lab, (x, y), ha='center', va='center', fontsize=9,
                bbox=dict(boxstyle="round", fc="white", ec="0.5", alpha=0.9))
ax.set_aspect('equal'); ax.grid(alpha=0.2)
ax.set_title("food plate (top-down): 3 snap (front) + 3 slip (back), self-supporting sockets")
ax.set_xlabel("x (mm)"); ax.set_ylabel("y (mm)")
ax.set_xlim(-120, 120); ax.set_ylim(-100, 100)
plt.tight_layout(); plt.savefig(OUT, dpi=120); print("wrote", OUT)
