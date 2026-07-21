"""Verify + top-down preview of the print plate STL (light: trimesh + matplotlib, no Blender).
Honours PIECES_ONLY / PLATE_STL / PLATE_CELL so it can preview the pieces-only plate too."""
import os, trimesh, numpy as np, matplotlib
matplotlib.use("Agg"); import matplotlib.pyplot as plt
HERE = os.path.dirname(os.path.abspath(__file__))
PIECES_ONLY = os.environ.get("PIECES_ONLY", "false").lower() in ("true", "1", "yes")
CELL = float(os.environ.get("PLATE_CELL", "48.0" if PIECES_ONLY else "42.0"))
STL  = os.environ.get("PLATE_STL", os.environ.get("PLATE_OUT", os.path.join(
    HERE, "renders/food", "pieces_plate.stl" if PIECES_ONLY else "print_plate.stl")))
OUT  = STL[:-4] + "_top.png" if STL.endswith(".stl") else STL + "_top.png"

m = trimesh.load(STL)
parts = m.split(only_watertight=False)
b = np.round(m.bounds, 1)
print("bodies: %d  bounds(mm) x[%g,%g] y[%g,%g] z[%g,%g]" %
      (len(parts), b[0,0], b[1,0], b[0,1], b[1,1], b[0,2], b[1,2]))
print("watertight: %d/%d   euler==2: %d/%d" %
      (sum(p.is_watertight for p in parts), len(parts),
       sum(p.euler_number == 2 for p in parts), len(parts)))

if PIECES_ONLY:
    labels = {(-CELL,0):"EAT", (0,0):"MOVE", (CELL,0):"GROW"}
    title  = "pieces print plate (top-down): EAT / MOVE / GROW  (no food — separate colour)"
else:
    labels = {(-CELL,CELL):"EAT",(0,CELL):"MOVE",(CELL,CELL):"GROW",
              (-CELL,0):"slip",(0,0):"slip",(CELL,0):"slip",
              (-CELL,-CELL):"snap",(0,-CELL):"snap",(CELL,-CELL):"snap"}
    title  = "print plate (top-down): 3 slip + 3 snap food, 1 each piece"
fig, ax = plt.subplots(figsize=(7,7))
for bed, c in [(256,'0.8'), (220,'0.6')]:
    ax.add_patch(plt.Rectangle((-bed/2,-bed/2), bed, bed, fill=False, ec=c, ls='--'))
    ax.text(-bed/2+2, bed/2-9, f"{bed}mm bed", color=c, fontsize=8)
rng = np.random.default_rng(0)
for p in parts:
    v = p.vertices; idx = rng.choice(len(v), min(1500, len(v)), replace=False)
    ax.scatter(v[idx,0], v[idx,1], s=1, alpha=0.25)
for (x,y), lab in labels.items():
    ax.annotate(lab, (x,y), ha='center', va='center', fontsize=8,
                bbox=dict(boxstyle="round", fc="white", ec="0.5", alpha=0.85))
ax.set_aspect('equal'); ax.grid(alpha=0.2)
ax.set_title(title)
ax.set_xlabel("x (mm)"); ax.set_ylabel("y (mm)")
ax.set_xlim(-140,140); ax.set_ylim(-140,140)
plt.tight_layout(); plt.savefig(OUT, dpi=110); print("wrote", OUT)
