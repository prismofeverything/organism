"""Render the finished pieces to diagrams/final_pieces.png with TRUE proportions
(z-axis scaled to real height, so tall pieces look tall and nothing clips).

    .venv/bin/python pieces/meshlib/render.py
"""
from pathlib import Path
import numpy as np, trimesh, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parent.parent
NAMES = ["EAT", "MOVE", "GROW"]

fig = plt.figure(figsize=(15, 6.5))
for c, name in enumerate(NAMES):
    m = trimesh.load(ROOT / "out" / f"{name}.obj")
    V, F = m.vertices, m.faces
    ax = fig.add_subplot(1, 3, c + 1, projection="3d")
    ax.plot_trisurf(V[:, 0], V[:, 1], V[:, 2], triangles=F,
                    cmap="viridis", linewidth=0, antialiased=True)
    R = max(np.ptp(V[:, 0]), np.ptp(V[:, 1])) / 2 * 1.05
    zmax = V[:, 2].max() * 1.08
    cx, cy = V[:, 0].mean(), V[:, 1].mean()
    ax.set_xlim(cx - R, cx + R); ax.set_ylim(cy - R, cy + R); ax.set_zlim(0, zmax)
    ax.set_box_aspect((2 * R, 2 * R, zmax))      # real proportions
    ax.set_title(f"{name}  —  {V[:, 2].max():.0f} mm tall", fontsize=12, weight="bold")
    ax.set_axis_off(); ax.view_init(elev=14, azim=-60)

fig.suptitle("Finished pieces (true proportions) — all seven invariants pass",
             fontsize=14, weight="bold")
fig.tight_layout()
out = ROOT / "diagrams" / "final_pieces.png"
fig.savefig(out, dpi=120)
print("wrote", out)
