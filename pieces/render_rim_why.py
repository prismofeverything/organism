"""Why the rim points out, and what makes it point up. Two cross-sections:
  LEFT  - current: the underside is still FLARING OUT (~69deg) at the rim, so the
          tangent rim inherits that outward direction and bulges sideways.
  RIGHT - proposed: the wall turns VERTICAL at the rim (its widest shoulder), so the
          rim rolls UP and over (curling slightly inward) -> points up, no side bulge.

    .venv/bin/python pieces/render_rim_why.py
"""
from pathlib import Path
import numpy as np, math, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = Path(__file__).resolve().parent
R, R_BEAR, T_C, FLARE_B, T_R = 13.0, 7.0, 8.0, 8.0, 2.5
FLARE_T = (FLARE_B + T_R) - T_C
NB, NR = 48, 18
rad = T_R / 2.0


def unit(v): n = math.hypot(*v); return (v[0] / n, v[1] / n)
def slope(f, x, h=1e-4): return (f(x + h) - f(x - h)) / (2 * h)


# ---- current shape: quadratic underside + tangent-Bezier rim + dished top ----
def cur_bottom(r): return 0.0 if r <= R_BEAR else FLARE_B * ((r - R_BEAR) / (R - R_BEAR)) ** 2
def cur_top(r):    return T_C + FLARE_T * (r / R) ** 2

def current():
    P = [(R * i / NB, cur_bottom(R * i / NB)) for i in range(NB + 1)]
    P0, P3 = (R, cur_bottom(R)), (R, cur_top(R))
    ub, ut = unit((1.0, slope(cur_bottom, R))), unit((1.0, slope(cur_top, R)))
    L = T_R * 0.75
    P1 = (P0[0] + L * ub[0], P0[1] + L * ub[1]); P2 = (P3[0] + L * ut[0], P3[1] + L * ut[1])
    for i in range(1, NR):
        t = i / NR; mt = 1 - t
        P.append((mt**3*P0[0]+3*mt*mt*t*P1[0]+3*mt*t*t*P2[0]+t**3*P3[0],
                  mt**3*P0[1]+3*mt*mt*t*P1[1]+3*mt*t*t*P2[1]+t**3*P3[1]))
    P += [(R * (1 - i / NB), cur_top(R * (1 - i / NB))) for i in range(NB + 1)]
    return np.array(P), P0, ub


# ---- proposed shape: quarter-ellipse wall (vertical at rim) + roll-over + dish ----
def proposed():
    P = [(0.0, 0.0), (R_BEAR, 0.0)]
    for i in range(1, NB + 1):                              # wall: flat base -> vertical rim
        ph = (math.pi / 2) * i / NB
        P.append((R_BEAR + (R - R_BEAR) * math.sin(ph), FLARE_B * (1 - math.cos(ph))))
    for i in range(1, NR + 1):                             # roll-over rim: vertical -> apex up
        al = (math.pi / 2) * i / NR
        P.append((R - rad + rad * math.cos(al), FLARE_B + rad * math.sin(al)))
    r_ap, z_ap = R - rad, FLARE_B + rad                   # dished top: horizontal at rim -> center
    c = (z_ap - T_C) / (r_ap ** 2)
    for i in range(1, NB + 1):
        r = r_ap * (1 - i / NB); P.append((r, z_ap - c * (r - r_ap) ** 2))
    return np.array(P), (R, FLARE_B), (0.0, 1.0)


def panel(ax, prof, joint, walldir, title, rim_note, rim_xy, rim_dir):
    P = prof
    ax.fill_betweenx(P[:, 1], -P[:, 0], P[:, 0], color="#f0a030", alpha=.18)
    ax.plot(P[:, 0], P[:, 1], "-", color="#a85a12", lw=2)
    ax.plot(-P[:, 0], P[:, 1], "-", color="#a85a12", lw=2)
    # wall tangent at the rim
    ax.annotate("", xy=(joint[0] + 3.2*walldir[0], joint[1] + 3.2*walldir[1]), xytext=joint,
                arrowprops=dict(arrowstyle="->", color="#1f6f1f", lw=2.2))
    ax.plot([joint[0]], [joint[1]], "o", ms=5, color="#1f6f1f")
    ang = math.degrees(math.atan2(walldir[1], walldir[0]))
    ax.text(joint[0] + 0.3, joint[1] - 1.6, f"wall tangent {ang:.0f}°", color="#1f6f1f", fontsize=9)
    # rim direction
    ax.annotate("", xy=(rim_xy[0] + 2.6*rim_dir[0], rim_xy[1] + 2.6*rim_dir[1]), xytext=rim_xy,
                arrowprops=dict(arrowstyle="->", color="#b52b27", lw=2.2))
    ax.text(rim_xy[0]-1, rim_xy[1] + 1.4, rim_note, color="#b52b27", fontsize=9, ha="center")
    wi = np.argmax(P[:, 0])
    ax.plot([P[wi, 0]], [P[wi, 1]], "k.", ms=8)
    ax.text(P[wi, 0] + 0.3, P[wi, 1], f"widest Ø{2*P[wi,0]:.1f}", fontsize=8)
    ax.axvline(R, color="0.7", ls=":", lw=.8); ax.axvline(-R, color="0.7", ls=":", lw=.8)
    ax.set_aspect("equal"); ax.set_xlabel("radius (mm)"); ax.set_ylabel("height (mm)")
    ax.set_title(title, weight="bold", fontsize=11); ax.set_xlim(-16, 16)


fig, (a0, a1) = plt.subplots(1, 2, figsize=(13, 6))
Pc, jc, wc = current()
Pp, jp, wp = proposed()
panel(a0, Pc, jc, wc, "CURRENT — wall still flaring OUT at rim",
      "rim follows →\npoints OUT", (Pc[np.argmax(Pc[:,0])][0], Pc[np.argmax(Pc[:,0])][1]), (0.95, 0.3))
panel(a1, Pp, jp, wp, "PROPOSED — wall turns UP (vertical) at rim",
      "rim rolls UP\n& over", (R - rad, FLARE_B + rad), (-0.3, 0.95))
fig.suptitle("Why the rim points out — and how to make it point up "
             "(the rim just follows the wall's direction)", weight="bold")
fig.tight_layout()
p = HERE / "renders" / "food" / "FOOD_rim_why.png"; fig.savefig(p, dpi=130); print("wrote", p)
