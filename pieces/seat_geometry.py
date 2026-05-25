"""
Seat-geometry probe for the generative connector graft (GRAFTING.md, approach F).

The universal connector seats on a disc of radius RIDGE_OD/2 (=6.4mm), +SEAT_MARGIN
(=7.0mm) for a flush ring. Its dome apex is the piece's z_max, so the ridge base
(the seat) sits at floor_z = z_max - DOME_HEIGHT. QUESTION: at floor_z, is the
generative body already >= the seat radius in EVERY direction? If yes, the peg is a
clean height-field boss with a tiny skirt. If the body is narrower there (it tapers
toward a point apex), a Ø14 seat would overhang -> a mushroom lip; we'd have to lower
the seat to where the body is naturally wide enough (and the piece grows a little
taller), or flare the top out to meet it.

This measures the min/max cross-section radius vs height near the top of each built
body (out/*.obj) and reports the highest seat we can place with no lip.

    uv run python pieces/seat_geometry.py            # all three
    uv run python pieces/seat_geometry.py EAT
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
OUT = HERE / "out"
RENDERS = HERE / "renders"; RENDERS.mkdir(exist_ok=True)

# connector spec (keep in sync with graft_connector.py)
RIDGE_OD = 12.8
SEAT_MARGIN = 0.6
DOME_HEIGHT = 4.3
R_HARD = RIDGE_OD / 2            # 6.40  ridge flush with body edge (no margin)
R_SEAT = RIDGE_OD / 2 + SEAT_MARGIN   # 7.00  flush seat with margin

ZMAX = {"EAT": 48.0, "MOVE": 60.0, "GROW": 36.0}   # = peg_top_z (dome apex)


def height_axis(mesh, zmax):
    """Height axis = the one whose extent matches z_max (robust when a wide, short
    piece like GROW has a footprint wider than its height, which fools 'max extent')."""
    ext = mesh.bounds[1] - mesh.bounds[0]
    return int(np.argmin(np.abs(ext - zmax))), ext


def ring_radii(mesh, axis, center, zt, normal):
    """Min/max radius of the cross-section at height zt (exact, via mesh.section)."""
    origin = center.copy(); origin[axis] = zt        # absolute cut height
    sec = mesh.section(plane_origin=origin, plane_normal=normal)
    if sec is None or len(sec.vertices) == 0:
        return None
    P = np.asarray(sec.vertices)
    planar = np.delete(P, axis, axis=1) - np.delete(center, axis)
    r = np.hypot(planar[:, 0], planar[:, 1])
    return float(r.min()), float(r.max()), float(np.median(r))


def analyze(name):
    path = OUT / f"{name}.obj"
    if not path.exists():
        print(f"  !! {path} missing — build it first (uv run python pieces/meshlib/build.py {name})")
        return None
    mesh = trimesh.load(path, process=False)
    zmax = ZMAX[name]
    ax, ext = height_axis(mesh, zmax)
    normal = np.zeros(3); normal[ax] = 1.0
    lo, hi = mesh.bounds[0][ax], mesh.bounds[1][ax]
    z_top = hi
    # planar center = median of the verts in the top 5mm (robust to a flat-ish base)
    V = mesh.vertices
    top = V[V[:, ax] > z_top - 5.0]
    center = np.array(mesh.bounds.mean(axis=0))
    center[np.delete([0, 1, 2], ax)] = np.median(np.delete(top, ax, axis=1), axis=0)

    floor_z = zmax - DOME_HEIGHT
    # measured height may differ slightly from spec; report both
    print(f"\n=== {name} ===  height axis={'xyz'[ax]}  extent={ext[ax]:.2f}mm "
          f"(spec z_max {zmax})  mesh top={z_top:.2f}")
    print(f"    seat needs r >= {R_HARD:.2f} (hard) / {R_SEAT:.2f} (w/ margin); "
          f"floor_z = z_max - {DOME_HEIGHT} = {floor_z:.2f}")

    zs = np.arange(max(lo + 0.5, 0.30 * zmax), z_top - 0.2, 0.4)
    rows = []
    for zt in zs:
        res = ring_radii(mesh, ax, center, zt, normal)
        if res:
            rmin, rmax, rmed = res
            rows.append((zt, rmin, rmax, rmed))
    rows = np.array(rows)
    zt, rmin, rmax, rmed = rows.T

    def highest_seat(thresh):
        ok = rmin >= thresh
        return float(zt[ok].max()) if ok.any() else None

    h_hard = highest_seat(R_HARD)
    h_seat = highest_seat(R_SEAT)

    # radius right at the spec floor_z
    i = int(np.argmin(np.abs(zt - floor_z)))
    print(f"    at floor_z={floor_z:.1f}:  r_min={rmin[i]:.2f}  r_max={rmax[i]:.2f}  "
          f"-> {'FITS' if rmin[i] >= R_HARD else 'TOO NARROW (would lip)'}")
    print(f"    highest seat with NO lip:  r>=6.4 at z<= {h_hard if h_hard else float('nan'):.2f}"
          f"   r>=7.0 at z<= {h_seat if h_seat else float('nan'):.2f}")
    if h_seat is not None:
        drop = floor_z - h_seat
        if drop > 0.05:
            print(f"    => seat must DROP {drop:.2f}mm below floor_z (piece grows "
                  f"~{drop:.1f}mm taller, or flare the top out to meet a Ø14 seat at floor_z)")
        else:
            print(f"    => body is wide enough AT floor_z — clean boss, minimal skirt")

    # plot
    fig, a = plt.subplots(figsize=(6, 4))
    a.plot(rmin, zt, label="r_min (binding)", color="C3", lw=2)
    a.plot(rmax, zt, label="r_max", color="C0", lw=1, ls="--")
    a.plot(rmed, zt, label="r_med", color="C2", lw=1, ls=":")
    a.axvline(R_HARD, color="gray", lw=1); a.text(R_HARD, zt.min(), " 6.4", fontsize=8)
    a.axvline(R_SEAT, color="k", lw=1);    a.text(R_SEAT, zt.min(), " 7.0", fontsize=8)
    a.axhline(floor_z, color="C1", lw=1.2); a.text(rmax.max(), floor_z, f" floor_z={floor_z:.0f}", fontsize=8, va="bottom", ha="right")
    if h_seat is not None:
        a.axhline(h_seat, color="C5", lw=1, ls="--"); a.text(rmax.max(), h_seat, f" seat<= {h_seat:.0f}", fontsize=8, va="top", ha="right")
    a.set_xlabel("cross-section radius (mm)"); a.set_ylabel("height z (mm)")
    a.set_title(f"{name}: top taper vs Ø{2*R_SEAT:.0f} seat")
    a.legend(fontsize=8, loc="lower right"); a.grid(alpha=0.3)
    fig.tight_layout()
    p = RENDERS / f"{name}_seat_profile.png"
    fig.savefig(p, dpi=110); plt.close(fig)
    print(f"    plot -> {p}")
    return name, floor_z, h_hard, h_seat


if __name__ == "__main__":
    names = sys.argv[1:] or ["EAT", "MOVE", "GROW"]
    summary = [analyze(n) for n in names]
    print("\n================ SUMMARY ================")
    print(f"{'piece':6} {'floor_z':>8} {'seat<=6.4':>10} {'seat<=7.0':>10}  {'verdict':<10}")
    for s in summary:
        if not s:
            continue
        name, fz, hh, hs = s
        drop = (fz - hs) if hs else None
        verdict = "clean" if (hs and fz - hs <= 0.05) else (f"drop {drop:.1f}mm" if hs else "no fit")
        print(f"{name:6} {fz:8.2f} {hh if hh else float('nan'):10.2f} "
              f"{hs if hs else float('nan'):10.2f}  {verdict:<10}")
