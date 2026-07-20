"""Cross-section comparison of the FOOD socket, OLD (flat-apex dome + flat-topped ridge groove ->
buried supports) vs NEW self-supporting (pointed dome cap + tented ridge roof). Printed socket-DOWN
(bed at z=0). Ceiling faces are coloured by overhang: red = steeper than 45deg from vertical (slicer
adds support), green = self-supporting. The mating peg (from the piece below) is overlaid to show the
grip is unchanged.  Run:  .venv/bin/python render_socket_fix.py   (pure python + matplotlib)"""
import math, os, sor
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon

dome_r, dome_h, ridge_ir, ridge_or, ridge_h, peak_w = 1.9, 4.3, 4.15, 6.4, 2.75, 2.0
GAP = -0.10   # snap

def old_socket(gap):
    s_dr, s_dh = dome_r + gap, dome_h + gap
    s_ir, s_or, s_rh = ridge_ir - gap, ridge_or + gap, ridge_h + gap
    P = sor._dome(s_dr, s_dh, 0.0)[::-1]; P.append((s_ir, 0.0)); P += sor._ridge(s_ir, s_or, s_rh, peak_w, 0.0)[1:]
    return P

def peg_outline():
    ridge = sor._ridge(ridge_ir, ridge_or, ridge_h, peak_w, 0.0)      # (ir,0)->peak->(or,0)
    dome = sor._dome(dome_r, dome_h, 0.0)                             # (dome_r,0)->(0,dome_h)
    return dome, ridge

def seg_color(a, b):
    dr = abs(b[0] - a[0]); dz = abs(b[1] - a[1])
    ang = 90.0 if dz < 1e-9 else math.degrees(math.atan2(dr, dz))
    if max(a[1], b[1]) < 0.06: return "#9aa0a6"                        # on the bed (seat) - supported
    return ("#d93025" if ang > 45 else "#188038")                     # red overhang / green self-support

def draw(ax, P, title, support_note):
    # mirror the meridian for a full cross-section
    full = [(-r, z) for (r, z) in reversed(P)] + list(P)
    # solid body hint: fill a big box then knock out the cavity by drawing white cavity polygon
    ax.add_patch(Polygon(full + [(P[-1][0], -0.6), (-P[-1][0], -0.6)], closed=True,
                         facecolor="#eef1f5", edgecolor="none", zorder=0))
    # ceiling coloured by overhang
    for side in (P, [(-r, z) for (r, z) in P]):
        for a, b in zip(side, side[1:]):
            ax.plot([a[0], b[0]], [a[1], b[1]], color=seg_color(a, b), lw=2.6, solid_capstyle="round", zorder=4)
    # mating peg (from the piece below), overlaid semi-transparent
    dome, ridge = peg_outline()
    for sgn in (1, -1):
        ax.plot([sgn * r for r, z in dome], [z for r, z in dome], color="#1a73e8", lw=1.4, alpha=.55, zorder=3)
        ax.plot([sgn * r for r, z in ridge], [z for r, z in ridge], color="#1a73e8", lw=1.4, alpha=.55, zorder=3)
    # support hatch for OLD flat roofs (from the flat ceiling down to the bed)
    if support_note:
        for (r0, z0, r1, z1) in support_note:
            ax.add_patch(Polygon([(r0, 0), (r1, 0), (r1, z1), (r0, z0)], closed=True, facecolor="none",
                                 hatch="////", edgecolor="#d93025", lw=0, alpha=.7, zorder=2))
            ax.add_patch(Polygon([(-r0, 0), (-r1, 0), (-r1, z1), (-r0, z0)], closed=True, facecolor="none",
                                 hatch="\\\\\\\\", edgecolor="#d93025", lw=0, alpha=.7, zorder=2))
    ax.axhline(0, color="#5f6368", lw=1.2, ls="--", zorder=1)
    ax.text(0, -0.5, "build plate", ha="center", va="top", fontsize=8, color="#5f6368")
    ax.set_title(title, fontsize=11, fontweight="bold")
    ax.set_aspect("equal"); ax.set_xlim(-8, 8); ax.set_ylim(-1.2, 5.6); ax.axis("off")

fig, (a1, a2) = plt.subplots(1, 2, figsize=(11, 4.4))
oldP = old_socket(GAP)
# flat-roof support zones for the OLD panel (dome apex + ridge groove flat)
oldsup = [(0, 4.2, 0.9, 4.2), (4.8, 2.65, 6.0, 2.65)]
draw(a1, oldP, "OLD  —  flat apex + flat groove roof", oldsup)
newP, apex = sor._socket_ss(dome_r, dome_h, ridge_ir, ridge_or, ridge_h, peak_w, GAP, 0.0, 30.0, 0.3)
draw(a2, newP, "NEW  —  pointed cap + tented roof", None)
# legend
from matplotlib.lines import Line2D
fig.legend([Line2D([0],[0],color="#d93025",lw=3), Line2D([0],[0],color="#188038",lw=3),
            Line2D([0],[0],color="#1a73e8",lw=1.6,alpha=.6)],
           ["overhang >45° (needs support)", "self-supporting ≤30°", "mating peg (grip unchanged)"],
           loc="lower center", ncol=3, frameon=False, fontsize=9, bbox_to_anchor=(0.5, -0.02))
fig.suptitle("FOOD socket, printed socket-DOWN  (snap fit, gap −0.10)", fontsize=12)
fig.tight_layout(rect=(0, 0.05, 1, 0.96))
out = os.path.join(os.path.dirname(os.path.abspath(__file__)), "renders", "food", "socket_fix.png")
fig.savefig(out, dpi=140); print("wrote", out)
