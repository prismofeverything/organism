"""Top-down spec for the 6-compartment ORGANISM insert (5 player sections + a 6th for all food).
Answers "what insert do we need". Pure matplotlib -> ../.venv/bin/python pieces/build_insert_diagram.py
"""
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle, FancyBboxPatch, Circle
import colorsys
def hsl(h,s,l): return colorsys.hls_to_rgb(h/360.0,l/100.0,s/100.0)
PC = {"RED":hsl(353,62,62),"YELLOW":hsl(45,78,64),"BLUE":hsl(196,45,60),
      "PURPLE":hsl(266,55,63),"GREEN":hsl(118,45,70),"FOOD":hsl(45,60,80)}

CW, CD, WALL = 150, 168, 3           # compartment inner + divider
cw, cd = CW+WALL, CD+WALL
cols, rows = 3, 2
IW, ID = cols*cw, rows*cd
EXT_W, EXT_D, EXT_H = IW+2*WALL, ID+2*WALL, 80
slots = [(0,0,"RED"),(1,0,"YELLOW"),(2,0,"BLUE"),(0,1,"PURPLE"),(1,1,"GREEN"),(2,1,"FOOD")]
CONTENTS = ("12 pieces  (4·4·4)\n12 disks  (4·4·4)\n3 power tokens\n9 platforms")
FOOD_TXT = "ALL 60 food\n(nested stacks)\n— the “6th place”"

fig, ax = plt.subplots(figsize=(11, 8.6))
ax.add_patch(FancyBboxPatch((-EXT_W/2, -EXT_D/2), EXT_W, EXT_D,
             boxstyle="round,pad=0,rounding_size=8", fill=False, ec="#222", lw=3))
def cc(gx,gy): return ((gx-(cols-1)/2)*cw, ((rows-1)/2-gy)*cd)
for gx,gy,who in slots:
    ox,oy = cc(gx,gy)
    ax.add_patch(Rectangle((ox-CW/2, oy-CD/2), CW, CD, fc=PC[who], ec="#333", lw=1.4, alpha=.9))
    ax.text(ox, oy+CD/2-16, who, ha="center", va="top", weight="bold", fontsize=13, color="#1a1a1a")
    if who=="FOOD":
        ax.text(ox, oy-6, FOOD_TXT, ha="center", va="center", fontsize=10.5, color="#222")
        for i in range(8):                                   # 8 nested food stacks
            cx = ox-56+(i%4)*36; cy = oy-58+(i//4)*30
            ax.add_patch(Circle((cx,cy), 15, fc="#e9d98a", ec="#7a6a2a", lw=.8))
    else:
        ax.text(ox, oy+2, CONTENTS, ha="center", va="center", fontsize=10, color="#1a1a1a")
        for i in range(3):                                   # 3 disk stacks marker
            ax.add_patch(Circle((ox-40+i*40, oy-64), 15, fc="#ffffff88", ec="#333", lw=.8))
        ax.add_patch(Circle((ox+52, oy-64), 11, fc="#ffffff88", ec="#333", lw=.8))  # tokens/plat

ax.annotate("", xy=(-EXT_W/2, -EXT_D/2-16), xytext=(EXT_W/2, -EXT_D/2-16), arrowprops=dict(arrowstyle="<->"))
ax.text(0, -EXT_D/2-30, f"{EXT_W} mm  ({EXT_W/25.4:.1f} in)", ha="center", fontsize=11, weight="bold")
ax.annotate("", xy=(EXT_W/2+16, -EXT_D/2), xytext=(EXT_W/2+16, EXT_D/2), arrowprops=dict(arrowstyle="<->"))
ax.text(EXT_W/2+30, 0, f"{EXT_D} mm\n({EXT_D/25.4:.1f} in)", va="center", fontsize=11, weight="bold", rotation=90)
ax.set_title(f"ORGANISM insert — 6 compartments (5 players + food)   "
             f"{EXT_W}×{EXT_D}×{EXT_H} mm  ({EXT_W/25.4:.1f}×{EXT_D/25.4:.1f}×{EXT_H/25.4:.1f} in, single layer)\n"
             f"quad-folded board is the bottom layer beneath the insert",
             fontsize=12.5, weight="bold")
ax.set_xlim(-EXT_W/2-70, EXT_W/2+90); ax.set_ylim(-EXT_D/2-55, EXT_D/2+30)
ax.set_aspect("equal"); ax.axis("off")
fig.tight_layout()
p="/tmp/claude-1000/-home-youdonotexist-code-organism/bbf7149e-6408-42a3-bcf9-0890d4f37841/scratchpad/insert_spec.png"
fig.savefig(p, dpi=125); print("wrote", p)
