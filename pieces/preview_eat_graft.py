"""
Cross-section preview of the generative connector graft on EAT (GRAFTING.md
approach F). Plots the merged meridian (connector + collar + wall) along the
THROAT ray and the POINT ray, against the CURRENT membrane body (which domes to a
point apex). Judged here BEFORE meshing: is the connector<->body junction G1 and
shelf-free, and does the star->circle collar look reasonable?

    uv run python pieces/preview_eat_graft.py
"""
from __future__ import annotations
import sys
from pathlib import Path
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE / "meshlib"))

from domain import load_region, round_corners                       # noqa: E402
from symmetry import symmetrize_region, _radial_outline, _r_at      # noqa: E402
from mesh2d import triangulate_region                               # noqa: E402
from field import membrane_field, smooth_scalar                     # noqa: E402
from profile import height                                          # noqa: E402
import connector_field as cf                                        # noqa: E402

NAME, FOLD, ZMAX, SHAPE, WALL, SM = "EAT", 5, 48.0, "hemisphere", 7 / 8, 40


def current_body_Hfn():
    """Interpolator H(x,y) for the current EAT membrane body (domes to a point)."""
    from scipy.interpolate import LinearNDInterpolator, NearestNDInterpolator
    reg = round_corners(symmetrize_region(
        load_region(HERE / "inputs" / "eat.svg", NAME), FOLD), radius=1.5)
    V, T, bnd = triangulate_region(reg, edge=0.5)
    u = membrane_field(V, T, bnd)
    H = height(u, ZMAX, SHAPE, WALL)
    H = smooth_scalar(V, T, H, bnd, iters=SM); H *= ZMAX / H.max()
    li = LinearNDInterpolator(V, H); ne = NearestNDInterpolator(V, H)
    def f(P):
        P = np.atleast_2d(P); z = li(P); nan = np.isnan(z)
        if nan.any(): z[nan] = ne(P[nan])
        return z
    return f, reg


def main():
    Hfn, reg = current_body_Hfn()
    th_s, r_s = _radial_outline(reg)
    th_point = float(th_s[np.argmax(r_s)])
    th_throat = float(th_s[np.argmin(r_s)])
    fz = cf.seat_z(ZMAX)

    fig, axes = plt.subplots(1, 2, figsize=(12, 6), sharey=True)
    for ax, (label, theta) in zip(axes, [("THROAT (valley)", th_throat),
                                         ("POINT (lobe tip)", th_point)]):
        R0 = float(_r_at(th_s, r_s, theta))
        # current body radial profile H(d) along this ray
        d = np.linspace(0, R0, 240)
        P = np.column_stack([d * np.cos(theta), d * np.sin(theta)])
        Hb = Hfn(P).ravel()
        ax.plot(d, Hb, color="0.55", lw=1.4, ls="--", label="current body (point apex)")

        # merged meridians at a few collar knee fractions
        for kf, col in [(0.62, "C0"), (0.78, "C2")]:
            mer = cf.full_meridian(R0, ZMAX, knee_frac=kf)
            ax.plot(mer[:, 0], mer[:, 1], color=col, lw=1.8,
                    label=f"graft (knee_frac={kf})")

        # connector zone markers
        ax.axvspan(0, cf.RIDGE_OR, color="orange", alpha=0.07)
        ax.axvline(cf.R_SEAT, color="orange", lw=1, ls=":");
        ax.axhline(fz, color="orange", lw=0.8, ls=":")
        ax.text(cf.R_SEAT, fz - 1.0, f" seat r={cf.R_SEAT:.1f}\n z={fz:.1f}",
                fontsize=8, color="darkorange", va="top")
        for r, lab in [(cf.DOME_R, "dome"), (cf.RIDGE_IR, "ridge"), (cf.RIDGE_OR, "")]:
            ax.axvline(r, color="orange", lw=0.5, ls="-", alpha=0.4)
        ax.set_title(f"{NAME} {label}  (rim r={R0:.1f}mm)")
        ax.set_xlabel("planar radius d (mm)"); ax.grid(alpha=0.3)
        ax.legend(fontsize=8, loc="upper right")
    axes[0].set_ylabel("height z (mm)")
    fig.suptitle("EAT generative graft — merged cross-section (connector boss + star→circle collar)",
                 fontsize=12, weight="bold")
    fig.tight_layout()
    p = HERE / "renders" / "EAT_graft_xsec.png"
    fig.savefig(p, dpi=120); print("wrote", p)


if __name__ == "__main__":
    main()
