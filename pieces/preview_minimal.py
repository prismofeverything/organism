"""Quick CPU preview of the minimal disks: top-down silhouette + vertical cross-section
(shows the 9mm height, the bevelled edges, and the socket carved into both faces).
  ../.venv/bin/python pieces/preview_minimal.py
"""
import os, numpy as np, trimesh, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection

HERE = os.path.dirname(os.path.abspath(__file__))
names = ["EAT", "MOVE", "GROW"]; cols = ["#cf5b52", "#5aa0b4", "#8a6bb0"]
fig, axes = plt.subplots(2, 3, figsize=(12, 7.2))
for i, (n, c) in enumerate(zip(names, cols)):
    m = trimesh.load(f"{HERE}/out/{n}_mindisk.obj", force="mesh")
    # --- top-down silhouette: fill all triangles projected to XY ---
    ax = axes[0, i]
    tris = m.triangles[:, :, :2]
    ax.add_collection(PolyCollection(tris, facecolors=c, edgecolors="none", alpha=0.04))
    ax.set_xlim(-22, 22); ax.set_ylim(-22, 22); ax.set_aspect("equal")
    ax.set_title(f"{n} — top silhouette", weight="bold"); ax.axis("off")
    # --- vertical cross-section at y=0: shows height, bevel, sockets top+bottom ---
    ax = axes[1, i]
    try:
        sec = m.section(plane_origin=[0, 0, 0], plane_normal=[0, 1, 0])
        if sec is not None:
            V = sec.vertices
            for ent in sec.entities:
                p = V[ent.points]
                ax.plot(p[:, 0], p[:, 2], c=c, lw=1.3)      # x (radius) vs z (height)
    except Exception as e:
        ax.text(0.5, 0.5, f"section failed\n{e}", ha="center", transform=ax.transAxes, fontsize=7)
    ax.axhline(0, color="0.8", lw=.6, ls=":"); ax.axhline(9, color="0.8", lw=.6, ls=":")
    ax.set_aspect("equal"); ax.set_ylim(-2, 15)
    ax.set_title(f"{n} — section: peg↑ socket↓", weight="bold"); ax.set_xlabel("mm")
fig.suptitle("MINIMAL disks — 37mm silhouette · 9mm body · bevelled edges · food-style peg(top)+socket(bottom)",
             weight="bold")
fig.tight_layout()
p = "/tmp/claude-1000/-home-youdonotexist-code-organism/bbf7149e-6408-42a3-bcf9-0890d4f37841/scratchpad/minimal_disks.png"
fig.savefig(p, dpi=120); print("wrote", p)
