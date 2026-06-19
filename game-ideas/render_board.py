#!/usr/bin/env python3
"""Chroma board v2: seeded geography + 6 rotating wedges + neutral hub.
Hex-of-hexes, edge=N cells (N=6 -> 91). Pointy-top cells, flat-top board."""
import math
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import RegularPolygon

N = 6
R = N - 1
SIZE = 1.0
SQRT3 = math.sqrt(3)

# RYB artist wheel, angular order
WHEEL = ["#e23b3b", "#ef8a2b", "#f4d030", "#3fae54", "#2f6fd0", "#7a3fb0"]
SECTOR_TINT = ["#fce8e8", "#fdeede", "#fcf7df", "#e6f4e9", "#e4ecf8", "#efe7f6"]

def ax_to_px(q, r):
    return SIZE * (SQRT3 * q + SQRT3 / 2 * r), SIZE * 1.5 * r

def px_to_cell(x, y):
    r = (y / (1.5 * SIZE))
    q = (x / SIZE - SQRT3 / 2 * r) / SQRT3
    # cube round
    cx, cy, cz = q, -q - r, r
    rx, ry, rz = round(cx), round(cy), round(cz)
    dx, dy, dz = abs(rx - cx), abs(ry - cy), abs(rz - cz)
    if dx > dy and dx > dz:
        rx = -ry - rz
    elif dy > dz:
        ry = -rx - rz
    else:
        rz = -rx - ry
    return (rx, rz)  # (q, r)

cells = []
for q in range(-R, R + 1):
    for r in range(-R, R + 1):
        if max(abs(q), abs(r), abs(-q - r)) <= R:
            cells.append((q, r))
cellset = set(cells)

# corners, by angle -> wheel color index
corners = []
for (q, r) in cells:
    if sorted(abs(v) for v in (q, r, -q - r)) == [0, R, R]:
        px, py = ax_to_px(q, r)
        corners.append((math.degrees(math.atan2(py, px)) % 360, q, r))
corners.sort()
corner_idx = {(q, r): i for i, (_, q, r) in enumerate(corners)}
# secondaries = triangle vertices = idx 1,3,5 (Orange,Green,Purple)
vert_cells = {i: (q, r) for i, (_, q, r) in enumerate(corners)}

seed = {}  # cell -> color hex
# center + 6 wheel neighbors
seed[(0, 0)] = "black"
neighbors = [(1, 0), (1, -1), (0, -1), (-1, 0), (-1, 1), (0, 1)]
nb = []
for (q, r) in neighbors:
    px, py = ax_to_px(q, r)
    nb.append((math.degrees(math.atan2(py, px)) % 360, q, r))
nb.sort()
for i, (_, q, r) in enumerate(nb):
    seed[(q, r)] = WHEEL[i]

# triangle edges: vertices idx 1,3,5; edge primary = shared component
# (1,3)->Yellow(2) ; (3,5)->Blue(4) ; (5,1)->Red(0)
edges = [((1, 3), 2), ((3, 5), 4), ((5, 1), 0)]
for (a, b), prim in edges:
    (qa, ra), (qb, rb) = vert_cells[a], vert_cells[b]
    xa, ya = ax_to_px(qa, ra); xb, yb = ax_to_px(qb, rb)
    for t in [k / 600 for k in range(601)]:
        c = px_to_cell(xa + (xb - xa) * t, ya + (yb - ya) * t)
        if c in cellset and c != (0, 0):
            seed[c] = WHEEL[prim]
# vertices get their secondary color (override)
for i in (1, 3, 5):
    seed[vert_cells[i]] = WHEEL[i]

# only the single black center is the neutral pivot; the 6 wheel cells
# each belong to (are the tip of) one wedge and stay playable
hub = {(0, 0)}

# sectors: 6 angular wedges for non-hub cells; boundaries through corners
def sector(q, r):
    px, py = ax_to_px(q, r)
    a = (math.degrees(math.atan2(py, px)) + 1e-6) % 360
    return int(((a + 30) % 360) // 60)

fig, ax = plt.subplots(figsize=(11, 11))
ax.set_aspect("equal"); ax.axis("off")

for (q, r) in cells:
    px, py = ax_to_px(q, r)
    if (q, r) in hub:
        bg = "#cfcfcf"
    else:
        bg = SECTOR_TINT[sector(q, r)]
    ax.add_patch(RegularPolygon((px, py), 6, radius=SIZE * 0.95, orientation=0,
                                facecolor=bg, edgecolor="#999", linewidth=1.0))
    if (q, r) in seed:
        ax.add_patch(RegularPolygon((px, py), 6, radius=SIZE * 0.62, orientation=0,
                                    facecolor=seed[(q, r)], edgecolor="white",
                                    linewidth=1.2))

# label each wedge at its outermost cell
for w in range(6):
    best = max((c for c in cells if c not in hub and sector(*c) == w),
               key=lambda c: max(abs(c[0]), abs(c[1]), abs(-c[0] - c[1])))
    px, py = ax_to_px(*best)
    ax.text(px, py, str(w + 1), ha="center", va="center",
            fontsize=13, fontweight="bold", color="#333", zorder=9)

n_seed = len(seed)
ax.set_title(f"Chroma v2 — edge 6 (91), seeded={n_seed}, white={91-n_seed} | "
             f"6 wedges (pale) + neutral hub (grey)", fontsize=13, pad=12)
xs = [ax_to_px(q, r)[0] for (q, r) in cells]
ys = [ax_to_px(q, r)[1] for (q, r) in cells]
ax.set_xlim(min(xs) - 2, max(xs) + 2); ax.set_ylim(min(ys) - 2, max(ys) + 2)
plt.tight_layout()
plt.savefig("/home/m/organism/game-ideas/chroma-board-mockup.png", dpi=110,
            bbox_inches="tight", facecolor="#f4f4f2")

# per-sector white-cell counts (excl hub & seeded)
from collections import Counter
cnt = Counter()
for (q, r) in cells:
    if (q, r) in hub or (q, r) in seed:
        continue
    cnt[sector(q, r)] += 1
seededw = Counter()
for (q, r) in cells:
    if (q, r) in hub or (q, r) not in seed:
        continue
    seededw[sector(q, r)] += 1
print("seeded:", n_seed, "white:", 91 - n_seed)
print("white per wedge:", dict(sorted(cnt.items())))
print("seeded per wedge:", dict(sorted(seededw.items())))
print("total playable per wedge:",
      {w: cnt[w] + seededw[w] for w in range(6)})
