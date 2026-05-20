"""
Dev preview: run domain -> mesh2d -> field -> height for each piece and render
the 2D quality mesh + the lifted 3D surface. This is a visual smoke test of
stages 1-4 (not the final exporter).

    .venv/bin/python pieces/meshlib/preview.py
"""
from __future__ import annotations
from pathlib import Path
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection

from domain import load_region
from mesh2d import triangulate_region, triangle_quality
from field import membrane_field
from profile import height

ROOT = Path(__file__).resolve().parent.parent
SPECS = {                                  # name -> (svg, z_max, shape, wall_frac)
    "EAT":  ("eat.svg",  48.0, "hemisphere", 7 / 8),
    "MOVE": ("move.svg", 60.0, "parabola",   0.0),
    "GROW": ("grow.svg", 36.0, "hemisphere", 0.5),
}


def main():
    fig = plt.figure(figsize=(13, 12))
    for r, (name, (svg, zmax, shape, wall)) in enumerate(SPECS.items()):
        reg = load_region(ROOT / "inputs" / svg, name)
        V, T, bnd = triangulate_region(reg, edge=1.0, min_angle=30.0)
        u = membrane_field(V, T, bnd)
        H = height(u, zmax, shape, wall)
        amin, amean, ntri = triangle_quality(V, T)
        print(f"{name}: {ntri} tris, min-angle={amin:.1f}deg (mean {amean:.1f}), "
              f"u in [0,1], H in [{H.min():.1f},{H.max():.1f}]mm")

        # 2D mesh
        ax = fig.add_subplot(3, 2, r * 2 + 1)
        segs = []
        for a, b, c in T:
            for i, j in ((a, b), (b, c), (c, a)):
                segs.append([V[i], V[j]])
        ax.add_collection(LineCollection(segs, colors="0.6", linewidths=0.3))
        ax.plot(V[bnd, 0], V[bnd, 1], ".", ms=2, color="navy")
        ax.set_aspect("equal"); ax.autoscale()
        ax.set_title(f"{name} — 2D mesh: {ntri} tris, min angle {amin:.0f}deg", fontsize=10)
        ax.set_xticks([]); ax.set_yticks([])

        # 3D surface
        ax = fig.add_subplot(3, 2, r * 2 + 2, projection="3d")
        ax.plot_trisurf(V[:, 0], V[:, 1], H, triangles=T, cmap="viridis",
                        linewidth=0.1, antialiased=True, edgecolor="none")
        ax.set_title(f"{name} — lifted surface ({shape}, wall={wall:.2f})", fontsize=10)
        ax.set_box_aspect((1, 1, 0.55))
        ax.set_xticks([]); ax.set_yticks([]); ax.set_zticks([])
        ax.view_init(elev=38, azim=-60)

    fig.suptitle("Stages 1-4: domain -> quality mesh -> membrane field -> lifted height field",
                 fontsize=14, weight="bold")
    fig.tight_layout(rect=[0, 0, 1, 0.96])
    p = ROOT / "diagrams" / "stage234_preview.png"
    fig.savefig(p, dpi=125)
    print("wrote", p)


if __name__ == "__main__":
    main()
