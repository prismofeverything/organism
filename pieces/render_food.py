"""Quick CPU render of the food piece (no Blender): a 3D surface view and a
cross-section, read straight from the mesh (robust as the profile changes).

    .venv/bin/python pieces/render_food.py [path/to.obj]
"""
import sys
from pathlib import Path
import numpy as np, trimesh, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = Path(__file__).resolve().parent
OBJ  = Path(sys.argv[1]) if len(sys.argv) > 1 else HERE / "renders" / "food" / "FOOD_new.obj"

m = trimesh.load(OBJ, process=False)
V, F = np.asarray(m.vertices), np.asarray(m.faces)
h = int(np.argmin(np.ptp(V, axis=0)))                 # height = shortest extent
o = [i for i in range(3) if i != h]
X, Y = V[:, o[0]] - V[:, o[0]].mean(), V[:, o[1]] - V[:, o[1]].mean()
Z = V[:, h] - V[:, h].min()
r = np.hypot(X, Y)

# --- 3D surface ------------------------------------------------------------
fig = plt.figure(figsize=(7, 6))
ax = fig.add_subplot(111, projection="3d")
ax.plot_trisurf(X, Y, Z, triangles=F, cmap="YlOrBr", linewidth=0, antialiased=True)
Rb = max(np.ptp(X), np.ptp(Y)) / 2
ax.set_box_aspect((2 * Rb, 2 * Rb, np.ptp(Z)))
ax.set_title(f"FOOD — Ø{2*r.max():.1f} mm, {np.ptp(Z):.1f} mm tall", weight="bold")
ax.set_axis_off(); ax.view_init(elev=18, azim=-55)
fig.tight_layout()
p3 = OBJ.with_name("FOOD_fixed_3d.png"); fig.savefig(p3, dpi=130); print("wrote", p3)

# --- cross-section (mesh only) --------------------------------------------
# outer silhouette = max radius per height bin; rim apex = highest outer point
nb = 60
bins = np.linspace(0, Z.max(), nb + 1)
idx = np.clip(np.digitize(Z, bins) - 1, 0, nb - 1)
outerR = np.array([r[idx == k].max() if np.any(idx == k) else 0 for k in range(nb)])
bc = (bins[:-1] + bins[1:]) / 2
rim_apex_z = bc[np.argmax(outerR)]
# connector tip = highest point near the axis
tip_z = Z[r < 3].max()

fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter(r, Z, s=.6, color="#8a5a1c", alpha=.35)
ax.scatter(-r, Z, s=.6, color="#8a5a1c", alpha=.35)
ax.axhline(rim_apex_z, color="#1f6f1f", ls="--", lw=1, label=f"rim apex z={rim_apex_z:.1f}")
ax.axhline(tip_z, color="#b52b27", ls="--", lw=1, label=f"connector tip z={tip_z:.1f}")
ax.set_aspect("equal"); ax.set_xlabel("radius (mm)"); ax.set_ylabel("height (mm)")
ax.set_title(f"Cross-section — Ø{2*r.max():.1f}, {np.ptp(Z):.1f} mm tall  "
             f"(connector {'recessed' if tip_z < rim_apex_z else 'proud'} "
             f"{abs(tip_z-rim_apex_z):.1f} mm)", weight="bold")
ax.legend(loc="upper right", fontsize=8)
fig.tight_layout()
px = OBJ.with_name("FOOD_fixed_xsec.png"); fig.savefig(px, dpi=130); print("wrote", px)
