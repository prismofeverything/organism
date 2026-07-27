"""Modular insert solver for ORGANISM. Each player is one IDENTICAL rectangular module
(12 pieces + nested disk/token/platform stacks). 5 modules + 1 food module + the quad-folded
board pack into a box with footprint <= MAX mm (13in). Regularity = grids + identical modules,
so the search is a tiny brute-force over discrete choices (piece orientation, tiers, disk stacks,
modules-per-layer), minimizing box volume. Prints the ranked configs and draws the winner.

  ../.venv/bin/python pieces/solve_insert.py [MAX_mm=330]
"""
import sys, math, itertools
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle, Circle
import colorsys

MAX = float(sys.argv[1]) if len(sys.argv) > 1 else 330.0     # 13 in
GAP = 2.0                                                     # slot clearance
WALL = 2.5                                                    # insert wall
# real component dims (mm)
P_DIA, P_STAND, P_LIE_L, P_LIE_W, P_LIE_H = 37.0, 57.0, 59.0, 39.0, 36.0
DISK_DIA, DISK_BASE, DISK_PITCH = 37.0, 13.3, 7.5            # 12 per player
TOK_DIA, TOK_H = 30.0, 5.0                                    # 3 per player
PLAT_DIA, PLAT_H = 37.0, 2.2                                  # 9 per player
FOOD_DIA, FOOD_BASE, FOOD_PITCH = 28.0, 11.25, 6.92          # 60 total
BOARD = 270.0; BOARD_H = 14.0                                 # quad-folded board (bottom layer)

def grid(n):                                                 # near-square regular grid (cols,rows)
    c = math.ceil(math.sqrt(n)); return c, math.ceil(n / c)

def stack_h(n, base, pitch): return base + (n - 1) * pitch

def player_module(orient, tiers, disk_stacks):
    """Return (W, D, H, note) for one player's module."""
    ppt = math.ceil(12 / tiers)                              # pieces per tier
    if orient == "stand":
        c, r = grid(ppt); bw, bd = c * (P_DIA + GAP), r * (P_DIA + GAP); bh = tiers * (P_STAND + GAP)
    else:                                                    # lying pieces
        c, r = grid(ppt); bw, bd = c * (P_LIE_L + GAP), r * (P_LIE_W + GAP); bh = tiers * (P_LIE_H + GAP)
    # side strip: disk stacks + token stack + platform stack, in a column
    per = math.ceil(12 / disk_stacks)
    dh = stack_h(per, DISK_BASE, DISK_PITCH)
    strip_stacks = disk_stacks + 2                           # + token + platform
    sc, sr = 1, strip_stacks
    strip_w = sc * (max(DISK_DIA, TOK_DIA, PLAT_DIA) + GAP)
    strip_d = sr * (DISK_DIA + GAP)
    strip_h = max(dh, stack_h(3, TOK_H, TOK_H), stack_h(9, PLAT_H, PLAT_H))
    W = bw + strip_w; D = max(bd, strip_d); H = max(bh, strip_h)
    return W, D, H, f"pieces {orient} {tiers}-tier ({c}x{r}/tier); disks {disk_stacks}x{per}"

def food_module():
    ns = math.ceil(60 / 8); c, r = grid(ns)                 # 8 food per nested stack
    return c * (FOOD_DIA + GAP), r * (FOOD_DIA + GAP), stack_h(8, FOOD_BASE, FOOD_PITCH), "food 8x8 nested"

def tile(mW, mD, n, maxw, maxd):
    """Smallest (W,D) fitting n identical mW x mD modules in a grid, within maxw x maxd. None if impossible."""
    best = None
    for cols in range(1, n + 1):
        rows = math.ceil(n / cols)
        W, D = cols * mW, rows * mD
        if W <= maxw and D <= maxd:
            if best is None or W * D < best[0] * best[1]: best = (W, D, cols, rows)
    return best

def solve(maxmm):
    fW, fD, fH, fnote = food_module()
    results = []
    for orient, tiers, dstk, mlayers in itertools.product(["stand","lie"], [1,2,3], [1,2,3], [1,2,3]):
        pW, pD, pH, pnote = player_module(orient, tiers, dstk)
        mW = max(pW, fW) + WALL; mD = max(pD, fD) + WALL      # uniform module cell (incl food)
        mH = max(pH, fH)
        per_layer = math.ceil(6 / mlayers)
        inner = maxmm - 2 * WALL
        t = tile(mW, mD, per_layer, inner, inner)
        if not t: continue
        W, D, cols, rows = t
        floorW = max(W, BOARD); floorD = max(D, BOARD)        # board is the bottom layer
        if floorW > inner or floorD > inner: continue
        ext_w = floorW + 2 * WALL; ext_d = floorD + 2 * WALL
        ext_h = BOARD_H + mlayers * mH + 2 * WALL
        vol = ext_w * ext_d * ext_h
        results.append(dict(vol=vol, ew=ext_w, ed=ext_d, eh=ext_h, cols=cols, rows=rows,
                            mlayers=mlayers, mW=mW, mD=mD, mH=mH, orient=orient, tiers=tiers,
                            dstk=dstk, pnote=pnote))
    results.sort(key=lambda r: r["vol"])
    return results

def draw(r, path):
    fig, ax = plt.subplots(figsize=(9, 8))
    ew, ed = r["ew"], r["ed"]
    ax.add_patch(Rectangle((0,0), ew, ed, fill=False, ec="#222", lw=3))
    PC = [colorsys.hls_to_rgb(h/360,.62,.55) for h in (353,45,196,266,118)] + [(0.92,0.86,0.55)]
    labels = ["RED","YELLOW","BLUE","PURPLE","GREEN","FOOD"]
    per_layer = math.ceil(6 / r["mlayers"]); idx = 0
    # show layer 1 (top-most modules); note board underneath
    for gy in range(r["rows"]):
        for gx in range(r["cols"]):
            if idx >= per_layer: break
            x = WALL + gx*r["mW"]; y = WALL + gy*r["mD"]
            ax.add_patch(Rectangle((x,y), r["mW"]-WALL, r["mD"]-WALL, fc=PC[idx%6], ec="#333", lw=1.3, alpha=.9))
            ax.text(x+(r["mW"])/2, y+(r["mD"])/2, labels[idx%6], ha="center", va="center", weight="bold")
            idx += 1
    ax.text(ew/2, -18, f"{ew:.0f} mm ({ew/25.4:.1f} in)", ha="center", weight="bold")
    ax.text(-16, ed/2, f"{ed:.0f} mm ({ed/25.4:.1f} in)", va="center", rotation=90, weight="bold")
    ax.set_title(f"WINNER  {r['ew']:.0f}x{r['ed']:.0f}x{r['eh']:.0f} mm "
                 f"({r['ew']/25.4:.1f}x{r['ed']/25.4:.1f}x{r['eh']/25.4:.1f} in)\n"
                 f"{r['mlayers']} module-layer(s) of {per_layer}; {r['pnote']}; board on floor",
                 weight="bold", fontsize=11)
    ax.set_xlim(-40, ew+20); ax.set_ylim(-40, ed+20); ax.set_aspect("equal"); ax.axis("off")
    fig.tight_layout(); fig.savefig(path, dpi=125); print("wrote", path)

def layered(maxmm):
    """Shared regular GRID (not rigid modules): pieces in one gridded layer with players in
    contiguous 12-slot zones, all nested stacks in a 2nd layer, folded board on the floor.
    Regular (a grid) AND concise (tight layers). Returns dims + layer specs."""
    inner = maxmm - 2 * WALL
    PP = P_DIA + GAP
    cols = int(inner // PP)                                  # 8 at 330
    prows = math.ceil(60 / cols); pw, pd, ph = cols * PP, prows * PP, P_STAND + GAP
    # stacks: 15 disk(4) + 5 token(3) + 5 platform(9) + 8 food(8)
    nst = 15 + 5 + 5 + 8
    SP = P_DIA + GAP; scols = int(inner // SP); srows = math.ceil(nst / scols)
    sw, sd, sh = scols * SP, srows * SP, stack_h(8, FOOD_BASE, FOOD_PITCH)
    fw = max(pw, sw, BOARD); fd = max(pd, sd, BOARD)
    ew, ed = fw + 2 * WALL, fd + 2 * WALL
    eh = BOARD_H + ph + sh + WALL + 6                        # board + piece layer + stack layer + lid
    return dict(ew=ew, ed=ed, eh=eh, cols=cols, prows=prows, scols=scols, srows=srows, nst=nst,
                ph=ph, sh=sh)

def draw_layered(L, path):
    fig, axs = plt.subplots(1, 2, figsize=(13, 6.6))
    PC = [colorsys.hls_to_rgb(h/360,.60,.55) for h in (353,45,196,266,118)]
    r = 17; PP = P_DIA + GAP
    ax = axs[0]; ax.set_title(f"Layer 2 — 60 pieces (player zones), {L['cols']}×{L['prows']} grid", weight="bold")
    for i in range(60):
        gx, gy = i % L["cols"], i // L["cols"]
        ax.add_patch(Circle((gx*PP, -gy*PP), r, fc=PC[i//12], ec="#222", lw=.6))
    ax = axs[1]; ax.set_title(f"Layer 3 — {L['nst']} nested stacks (disks·tokens·platforms·food)", weight="bold")
    kinds = [("disk",15,PC),("tok",5,PC),("plat",5,PC)]; i=0
    for kind,n,_ in kinds:
        for j in range(n):
            gx,gy=i%L["scols"], i//L["scols"]; ax.add_patch(Circle((gx*PP,-gy*PP), r, fc=PC[j%5], ec="#222", lw=.6)); i+=1
    for j in range(8):
        gx,gy=i%L["scols"], i//L["scols"]; ax.add_patch(Circle((gx*PP,-gy*PP), 14, fc="#e9d98a", ec="#7a6a2a", lw=.8)); i+=1
    for ax in axs: ax.set_aspect("equal"); ax.axis("off"); ax.autoscale()
    fig.suptitle(f"RECOMMENDED shared-grid insert:  {L['ew']:.0f}×{L['ed']:.0f}×{L['eh']:.0f} mm  "
                 f"({L['ew']/25.4:.1f}×{L['ed']/25.4:.1f}×{L['eh']/25.4:.1f} in)  ·  board folded on the floor (layer 1)",
                 weight="bold", fontsize=13)
    fig.tight_layout(); fig.savefig(path, dpi=120); print("wrote", path)

if __name__ == "__main__":
    L = layered(MAX)
    print(f"\n=== RECOMMENDED shared-grid (regular + concise) ===")
    print(f"  box {L['ew']:.0f} x {L['ed']:.0f} x {L['eh']:.0f} mm  "
          f"({L['ew']/25.4:.1f} x {L['ed']/25.4:.1f} x {L['eh']/25.4:.1f} in)")
    print(f"  L1 board(folded) 14mm | L2 pieces {L['cols']}x{L['prows']} ({L['ph']:.0f}mm) | "
          f"L3 {L['nst']} stacks {L['scols']}x{L['srows']} ({L['sh']:.0f}mm)")
    draw_layered(L, "/tmp/claude-1000/-home-youdonotexist-code-organism/"
                    "bbf7149e-6408-42a3-bcf9-0890d4f37841/scratchpad/insert_layered.png")
    res = solve(MAX)
    print(f"\n=== best insert layouts, footprint <= {MAX:.0f}mm ({MAX/25.4:.1f}in) ===")
    print(f"{'box (mm)':>22} {'(in)':>18} {'layers':>7} {'grid':>6}  config")
    for r in res[:8]:
        print(f"{r['ew']:5.0f}x{r['ed']:4.0f}x{r['eh']:4.0f}   "
              f"{r['ew']/25.4:4.1f}x{r['ed']/25.4:4.1f}x{r['eh']/25.4:4.1f}   "
              f"{r['mlayers']:5d}  {r['cols']}x{r['rows']}  {r['orient']} {r['tiers']}t disks:{r['dstk']}")
    if res:
        draw(res[0], "/tmp/claude-1000/-home-youdonotexist-code-organism/"
                     "bbf7149e-6408-42a3-bcf9-0890d4f37841/scratchpad/insert_solve.png")
    else:
        print("no config fits — relax MAX or change the module")
