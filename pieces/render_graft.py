"""
Verify + visualize the EAT generative graft (out/EAT_graft.obj):
  - cross-sections sliced from the ACTUAL mesh (throat + point rays), overlaid on
    the analytic target meridian -> confirms the build matches the design + the
    connector<->body seam is G1/shelf-free;
  - 3D oblique view + a crown close-up (the connector + calyx collar).

    uv run python pieces/render_graft.py
"""
from __future__ import annotations
import sys
from pathlib import Path
import numpy as np
import trimesh
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE / "meshlib"))
from domain import load_region, round_corners                  # noqa: E402
from symmetry import symmetrize_region, _radial_outline, _r_at  # noqa: E402
import connector_field as cf                                    # noqa: E402

NAME, FOLD, ZMAX = "EAT", 5, 48.0
KNEE_FRAC, FULLNESS = 0.70, 0.6


def xsec(mesh, theta):
    """Meridian points (r_signed, z) from a vertical plane through the axis at angle theta."""
    n = np.array([-np.sin(theta), np.cos(theta), 0.0])
    sec = mesh.section(plane_origin=[0, 0, 0], plane_normal=n)
    pts = []
    if sec is not None:
        for poly in sec.discrete:
            P = np.asarray(poly)
            r = P[:, 0] * np.cos(theta) + P[:, 1] * np.sin(theta)
            pts.append(np.column_stack([r, P[:, 2]]))
    return pts


def main():
    mesh = trimesh.load(HERE / "out" / f"{NAME}_graft.obj", process=False)
    reg = round_corners(symmetrize_region(
        load_region(HERE / "inputs" / "eat.svg", NAME), FOLD), radius=1.5)
    th_s, r_s = _radial_outline(reg)
    th_point = float(th_s[np.argmax(r_s)])
    th_throat = float(th_s[np.argmin(r_s)])
    fz = cf.seat_z(ZMAX)

    fig = plt.figure(figsize=(14, 7))
    # --- cross-sections ---
    for c, (lab, theta) in enumerate([("THROAT", th_throat), ("POINT", th_point)]):
        ax = fig.add_subplot(2, 2, 1 + c)
        for seg in xsec(mesh, theta):
            ax.plot(seg[:, 0], seg[:, 1], ".", ms=1.5, color="C3")
        R0 = float(_r_at(th_s, r_s, theta))
        mer = cf.full_meridian(R0, ZMAX, KNEE_FRAC, FULLNESS)
        ax.plot(mer[:, 0], mer[:, 1], "-", color="0.5", lw=1, label="analytic target")
        ax.plot(-mer[:, 0], mer[:, 1], "-", color="0.5", lw=1)
        ax.axvline(cf.R_SEAT, color="orange", lw=0.8, ls=":")
        ax.axvline(-cf.R_SEAT, color="orange", lw=0.8, ls=":")
        ax.axhline(fz, color="orange", lw=0.6, ls=":")
        ax.set_title(f"{NAME} {lab} cross-section (mesh dots vs analytic)", fontsize=10)
        ax.set_aspect("equal"); ax.grid(alpha=0.3); ax.legend(fontsize=7)
        ax.set_xlabel("r (mm)"); ax.set_ylabel("z (mm)")

    V, F = mesh.vertices, mesh.faces
    # --- 3D oblique (full) ---
    ax = fig.add_subplot(2, 2, 3, projection="3d")
    ax.plot_trisurf(V[:, 0], V[:, 1], V[:, 2], triangles=F, cmap="viridis",
                    linewidth=0, antialiased=True)
    R = np.ptp(V[:, 0]) / 2 * 1.05
    ax.set_xlim(-R, R); ax.set_ylim(-R, R); ax.set_zlim(0, ZMAX * 1.05)
    ax.set_box_aspect((2 * R, 2 * R, ZMAX))
    ax.set_axis_off(); ax.view_init(elev=18, azim=-60)
    ax.set_title(f"{NAME} graft — {V[:,2].max():.0f}mm tall", fontsize=10)

    # --- 3D crown close-up ---
    ax = fig.add_subplot(2, 2, 4, projection="3d")
    ax.plot_trisurf(V[:, 0], V[:, 1], V[:, 2], triangles=F, cmap="viridis",
                    linewidth=0.05, edgecolor="0.3", antialiased=True)
    ax.set_xlim(-12, 12); ax.set_ylim(-12, 12); ax.set_zlim(fz - 9, ZMAX + 0.5)
    ax.set_box_aspect((24, 24, 9 + 4.8))
    ax.set_axis_off(); ax.view_init(elev=22, azim=-60)
    ax.set_title("crown close-up: connector boss + calyx collar", fontsize=10)

    fig.suptitle("EAT generative connector graft — built mesh verification",
                 fontsize=13, weight="bold")
    fig.tight_layout()
    p = HERE / "renders" / "EAT_graft_3d.png"
    fig.savefig(p, dpi=120); print("wrote", p)


if __name__ == "__main__":
    main()
