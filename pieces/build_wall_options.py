"""Non-straight insert walls: square grid vs hexagonal honeycomb vs radial wedges, for the
60 round Ø37 pieces. Shows the footprint each needs. Pure matplotlib.
  ../.venv/bin/python pieces/build_wall_options.py
"""
import math, colorsys
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Circle, RegularPolygon, Wedge
PC = [colorsys.hls_to_rgb(h/360,.60,.55) for h in (353,45,196,266,118)]
R = 18.5; PITCH = 39.0                     # Ø37 + 2mm clearance
N = 60

fig, axs = plt.subplots(1, 3, figsize=(16, 6))

# --- square grid: 8 cols x 8 rows ---
ax = axs[0]; cols = 8
for i in range(N):
    gx, gy = i % cols, i // cols
    ax.add_patch(RegularPolygon((gx*PITCH, -gy*PITCH), 4, radius=PITCH/2*1.414, orientation=math.pi/4,
                                fc="none", ec="#bbb", lw=1))
    ax.add_patch(Circle((gx*PITCH, -gy*PITCH), R, fc=PC[i//12], ec="#222", lw=.6))
sw, sd = cols*PITCH, math.ceil(N/cols)*PITCH
ax.set_title(f"STRAIGHT square grid\n{sw:.0f} × {sd:.0f} mm  ({sw*sd/100:.0f} cm²)", weight="bold")

# --- hex honeycomb: offset rows, hex cells ---
ax = axs[1]; RP = PITCH*math.sqrt(3)/2       # row pitch
i = 0; row = 0; ys = []
while i < N:
    ncol = 8 if row % 2 == 0 else 7
    off = 0 if row % 2 == 0 else PITCH/2
    y = -row*RP; ys.append(y)
    for c in range(ncol):
        if i >= N: break
        x = c*PITCH + off
        ax.add_patch(RegularPolygon((x, y), 6, radius=PITCH/2*1.155, orientation=0, fc="none", ec="#bbb", lw=1))
        ax.add_patch(Circle((x, y), R, fc=PC[i//12], ec="#222", lw=.6))
        i += 1
    row += 1
hw = 8*PITCH; hd = (row-1)*RP + PITCH
ax.set_title(f"HEX honeycomb (each lifts straight out)\n{hw:.0f} × {hd:.0f} mm  ({hw*hd/100:.0f} cm²)  −{100*(1-hw*hd/(sw*sd)):.0f}%",
             weight="bold")

# --- radial wedges: 6 sections around the folded board ---
ax = axs[2]
ax.add_patch(Circle((0,0), 150, fc="#20323a", ec="#111", lw=1.5))
ax.add_patch(RegularPolygon((0,0), 4, radius=95, orientation=math.pi/4, fc="#3a5560", ec="#111", lw=1))
ax.text(0,0,"folded\nboard", ha="center", va="center", color="w", fontsize=8, weight="bold")
labels=["RED","YELLOW","BLUE","PURPLE","GREEN","FOOD"]; cols6=PC+[(0.9,0.85,0.55)]
for k in range(6):
    a0=k*60; ax.add_patch(Wedge((0,0),150,a0,a0+60, width=150, fc=cols6[k], ec="#111", lw=1.2, alpha=.55))
    aa=math.radians(a0+30); ax.text(112*math.cos(aa),112*math.sin(aa),labels[k],ha="center",va="center",fontsize=8,weight="bold")
ax.set_title("RADIAL wedges (mirrors the game)\ncorners → rulebook / mutations / power board", weight="bold")

for ax in axs: ax.set_aspect("equal"); ax.axis("off"); ax.autoscale()
fig.suptitle("Insert wall geometry — round parts pack far better than straight walls allow",
             weight="bold", fontsize=14)
fig.tight_layout()
p="/tmp/claude-1000/-home-youdonotexist-code-organism/bbf7149e-6408-42a3-bcf9-0890d4f37841/scratchpad/wall_options.png"
fig.savefig(p, dpi=118); print("wrote", p)
print(f"square {sw:.0f}x{sd:.0f}={sw*sd/100:.0f}cm2  hex {hw:.0f}x{hd:.0f}={hw*hd/100:.0f}cm2  saving {100*(1-hw*hd/(sw*sd)):.0f}%")
