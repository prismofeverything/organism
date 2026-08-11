"""Packing solver for the ORGANISM physical box.

Given a bill of materials (component -> count) and an interior box depth, decide how
many vertical stacks/columns each component needs, lay those columns out as a compact
near-square tray, and report the smallest box that holds it all under the constraint
that both floor dimensions exceed the depth (x, y > z -> a flat "landscape" box).

Real component dimensions are measured from the built meshes (out/*_sculpt_graft,
renders/food/FOOD_*.stl) and the minimal-set spec (disc Ø37x9, cube 8mm).

  ../.venv/bin/python pieces/pack_box.py [maximal|minimal|bracket]

The 'bracket' mode fixes the floor at a range of footprints (the folded board is the
limiter, ~10in quad) and reports how tall the box must get to hold identical contents,
from the flat single-layer end to the board-limited multi-layer end.
"""
import sys, math, os
HERE = os.path.dirname(os.path.abspath(__file__))
# matplotlib is imported lazily inside draw() so this module stays importable under Blender's
# bundled python (which has no matplotlib) -- build_packed_box.py imports pack_box for its layout.

# ---- measured component library ------------------------------------------------
# footprint = top-down bounding size when the part sits in its storage orientation.
# 'stand' parts occupy the full box depth (one layer); 'stack' parts column up by pitch.
MM = 1.0
COMPONENTS = {
    # 3D hero pieces stand upright (their height is the tall dimension). round-ish footprint.
    "EAT":  dict(kind="stand", w=32.5, d=34.0, h=44.0, color="#cf5b52", shape="blob"),   # measured out/EAT_sculpt_graft.obj
    "MOVE": dict(kind="stand", w=35.7, d=35.2, h=53.0, color="#5aa0b4", shape="blob"),   # measured out/MOVE_sculpt_graft.obj
    "GROW": dict(kind="stand", w=36.6, d=36.6, h=31.8, color="#8a6bb0", shape="blob"),   # measured out/GROW_sculpt_graft.obj
    # 3D food nests deeply: base 11.25, each extra adds 6.92 (slip fit, measured).
    "FOOD": dict(kind="stack", dia=28.1, h=11.25, pitch=6.92, color="#d99a3a", shape="disc"),
    # minimal set: silhouette disks are "basically food" (food-style peg/socket) -> they NEST.
    "DISC": dict(kind="stack", dia=37.0, h=9.0,  pitch=7.0,  color="#c8b48a", shape="disc"),
    # 5 players x 3 power tokens = 15, small coins (Ø30 x 5) -> short nested stacks.
    "PTOK": dict(kind="stack", dia=30.0, h=5.0, pitch=5.0, color="#b9b9c2", shape="disc"),
    # player platforms = round tokens (measured plats/blue.png 540px @300dpi = 46mm).
    # mutation "cards" are BIG round ability discs (measured cards/card_*.png 1520px @300dpi = 123mm).
    "PLAT": dict(kind="stack", dia=46.0, h=2.0, pitch=2.0, color="#9a8fb5", shape="disc"),
    "MUT":  dict(kind="stack", dia=123.0, h=0.5, pitch=0.5, color="#6f9f8f", shape="disc"),
}

# The ONLY true large flats. player platforms + mutation cards are NOT here (they're round tray
# tokens above). footprint = stored (folded) mm; t = stacked thickness mm (estimates -> tune).
FLAT_GOODS = {
    "main-board (quad-fold ~10in)": dict(t=8.0, fp=(254, 254)),
    "power-board (folded)":         dict(t=4.0, fp=(230, 230)),
    "rulebook (222mm, 24pp)":       dict(t=3.0, fp=(222, 222)),
    "5 player aids":                dict(t=3.0, fp=(150, 100)),   # small; here only for thickness
}

BOMS = {
    # maximal "big box": full 3D game + the complete minimal set + tokens/platforms/mutations.
    # 8mm food cubes struck: the minimal disks are food-style, so real food nests on them.
    # PLAT=5 assumes 1 platform/player; if 5/player it's 25 (still a few tiny stacks -> negligible).
    # per player x5: 12 pieces (4/4/4) + 12 disks (4/4/4) + 3 power tokens + 9 platforms;
    # food (60) + mutations (26) are shared. Food goes in the "6th place" compartment.
    "maximal": {"EAT": 20, "MOVE": 20, "GROW": 20, "FOOD": 60,
                "PTOK": 15, "PLAT": 45, "MUT": 26},   # 3D-only product: minimal DISC set struck (no more 2D pieces)
    # minimal alone (figured out separately, next): silhouette disks + real food, no cubes.
    "minimal": {"DISC": 60},
}

def columns_for(comp, count, z_use):
    """How many storage columns a stacking component needs, and each column's footprint/height."""
    c = COMPONENTS[comp]
    if c["kind"] == "stand":
        # one part per column; column footprint = its silhouette; height = part height.
        return [dict(w=c["w"], d=c["d"], h=c["h"], n=1, comp=comp) for _ in range(count)]
    if c["kind"] == "stack":
        per = max(1, int((z_use - c["h"]) // c["pitch"]) + 1)   # tallest stack under z_use
        nstacks = math.ceil(count / per)
        cols = []
        left = count
        for _ in range(nstacks):
            n = min(per, left); left -= n
            h = c["h"] + (n - 1) * c["pitch"]
            cols.append(dict(w=c["dia"], d=c["dia"], h=h, n=n, comp=comp))
        return cols
    if c["kind"] == "cubeblk":
        s = c["side"]
        per_col = max(1, int(z_use // s))                       # cubes stacked in a column
        ncols = math.ceil(count / per_col)                      # square-ish block of columns
        side_cols = math.ceil(math.sqrt(ncols))
        w = side_cols * s
        return [dict(w=w, d=w, h=per_col * s, n=count, comp=comp, block=True)]
    raise ValueError(comp)

def shelf_pack(cells, target_w, gap):
    """First-fit-decreasing-height shelf packing into a strip of width target_w."""
    cells = sorted(cells, key=lambda c: (-max(c["w"], c["d"]), c["comp"]))
    placed, x, y, shelf_h, used_w = [], 0.0, 0.0, 0.0, 0.0
    for c in cells:
        w, d = c["w"] + gap, c["d"] + gap
        if x + w > target_w and x > 0:                          # wrap to next shelf
            y += shelf_h; x = 0.0; shelf_h = 0.0
        placed.append({**c, "x": x, "y": y})
        x += w; shelf_h = max(shelf_h, d); used_w = max(used_w, x)
    total_h = y + shelf_h
    return placed, used_w, total_h

def pack(bom, z_interior=60.0, gap=3.0, wall=2.5, lid=2.5):
    z_use = z_interior
    cells = []
    for comp, count in bom.items():
        if count: cells.extend(columns_for(comp, count, z_use))
    # search shelf width to get the most square footprint with both dims > depth.
    total_area = sum((c["w"] + gap) * (c["d"] + gap) for c in cells)
    best = None
    lo = math.sqrt(total_area) * 0.65
    for tw in [lo * (1 + i * 0.03) for i in range(80)]:
        placed, W, H = shelf_pack(cells, tw, gap)
        if min(W, H) <= z_interior:      # enforce x,y > z (flat box)
            continue
        score = max(W, H) + 0.35 * abs(W - H)                   # compact + squareish
        if best is None or score < best[0]:
            best = (score, placed, W, H)
    if best is None:                                            # fallback: single wide shelf
        placed, W, H = shelf_pack(cells, lo * 3, gap)
        best = (0, placed, W, H)
    _, placed, W, H = best
    inner_w, inner_d = W + gap, H + gap                         # +border gap
    ext = dict(x=inner_w + 2 * wall, y=inner_d + 2 * wall, z=z_interior + wall + lid)
    return dict(placed=placed, inner=(inner_w, inner_d, z_interior),
                ext=(ext["x"], ext["y"], ext["z"]), z_use=z_use, gap=gap)

def report(name, bom, res):
    iw, idd, iz = res["inner"]; ex, ey, ez = res["ext"]
    print(f"\n=== {name.upper()} BOX ===")
    # component summary
    per_comp = {}
    for c in res["placed"]:
        per_comp.setdefault(c["comp"], [0, 0])
        per_comp[c["comp"]][0] += 1
        per_comp[c["comp"]][1] += c.get("n", 1)
    print(f"{'component':7s} {'count':>6} {'columns':>8} {'per-col':>8}")
    for comp, cnt in bom.items():
        if not cnt: continue
        ncol = per_comp.get(comp, [0, 0])[0]
        print(f"{comp:7s} {cnt:6d} {ncol:8d} {('%.0f' % (cnt/ncol)) if ncol else '-':>8}")
    solid = 0.0
    for comp, cnt in bom.items():
        c = COMPONENTS[comp]
        if c["kind"] == "stand":  solid += cnt * c["w"] * c["d"] * c["h"] * 0.55
        elif c["kind"] == "stack":solid += cnt * math.pi * (c["dia"]/2)**2 * c["h"] * 0.7
        else:                     solid += cnt * c["side"]**3
    boxvol = ex * ey * ez
    print(f"\ninterior  : {iw:6.1f} x {idd:6.1f} x {iz:5.1f} mm")
    print(f"EXTERIOR  : {ex:6.1f} x {ey:6.1f} x {ez:5.1f} mm   "
          f"({ex/25.4:.1f} x {ey/25.4:.1f} x {ez/25.4:.1f} in)")
    print(f"constraint: x,y > z ?  {min(ex,ey):.0f} > {ez:.0f}  -> {'OK' if min(ex,ey)>ez else 'FAIL'}")
    print(f"footprint : {ex*ey/645.16:.1f} sq-in    volume {boxvol/1e3:.0f} cm^3")
    print(f"fill      : ~{100*solid/boxvol:.0f}% of box is component (rest = air/gaps/trays)")
    return per_comp

def draw(name, bom, res, path):
    import matplotlib; matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.patches import Rectangle, Circle
    iw, idd, iz = res["inner"]; ex, ey, ez = res["ext"]; gap = res["gap"]
    fig, ax = plt.subplots(figsize=(9, 9 * ey / ex))
    ax.add_patch(Rectangle((-2.5, -2.5), ex, ey, fill=False, ec="#333", lw=2))
    ax.add_patch(Rectangle((0, 0), iw, idd, fc="#f4f1ea", ec="#bbb", lw=1))
    for c in res["placed"]:
        comp = c["comp"]; C = COMPONENTS[comp]; col = C["color"]
        cx, cy = c["x"] + gap/2, c["y"] + gap/2
        if C.get("shape") == "disc":
            r = c["w"]/2
            ax.add_patch(Circle((cx + r, cy + r), r, fc=col, ec="#0003", lw=.6))
            if c.get("n", 1) > 1:
                ax.text(cx + r, cy + r, str(c["n"]), ha="center", va="center",
                        fontsize=7, color="white", weight="bold")
        else:
            ax.add_patch(Rectangle((cx, cy), c["w"], c["d"], fc=col, ec="#0003", lw=.6))
            lbl = comp if not c.get("block") else f"{comp}\nx{c['n']}"
            ax.text(cx + c["w"]/2, cy + c["d"]/2, lbl, ha="center", va="center",
                    fontsize=7, color="white", weight="bold")
    ax.set_xlim(-8, ex - 2.5 + 6); ax.set_ylim(-8, ey - 2.5 + 6); ax.set_aspect("equal")
    ax.invert_yaxis()
    ax.set_title(f"ORGANISM — {name} box   {ex:.0f} x {ey:.0f} x {ez:.0f} mm "
                 f"({ex/25.4:.1f}x{ey/25.4:.1f}x{ez/25.4:.1f} in)", weight="bold")
    ax.set_xlabel("mm"); fig.tight_layout(); fig.savefig(path, dpi=120)
    print("wrote", path)

def bracket(bom=None, floors_mm=(254, 279, 305, 330, 356), z_layer=60.0,
            gap=3.0, wall=2.5, lid=3.0, flat=FLAT_GOODS, eff=0.82, pack_eff=0.80):
    """Fix the floor at each footprint, report box height to hold identical contents.

    dense = reclaim air above the short pieces (fitted trays, small parts seated on the
    tall pieces' connectors) -> volume-limited. loose = plain flat layer-trays -> the
    tallest column per layer. Reality sits between. Flat paper goods add as a top slab.
    """
    bom = bom or BOMS["maximal"]
    cols = []
    for comp, count in bom.items():
        if count: cols.extend(columns_for(comp, count, z_layer))
    fp_sum = sum((c["w"] + gap) * (c["d"] + gap) for c in cols)          # mm^2
    vol_sum = sum((c["w"] + gap) * (c["d"] + gap) * c["h"] for c in cols)
    tallest = max(c["h"] for c in cols)
    flat_slab = sum(g["t"] for g in flat.values())
    flat_fp_max = max(max(g["fp"]) for g in flat.values())              # biggest flat long-edge
    rows = []
    for fw in floors_mm:
        A = fw * fw; usable = A * eff
        nlayers = math.ceil(fp_sum / usable)
        loose = nlayers * tallest + flat_slab
        dense = vol_sum / (A * pack_eff) + flat_slab
        if fp_sum <= usable: dense = max(dense, tallest + flat_slab)    # >=1 real layer
        ext = fw + 2 * wall
        rows.append(dict(floor=fw, ext=ext, layers=nlayers,
                         int_lo=dense, int_hi=loose,
                         ext_lo=dense + wall + lid, ext_hi=loose + wall + lid,
                         flat_fits=fw >= flat_fp_max))
    return dict(rows=rows, fp_sum=fp_sum, vol_sum=vol_sum, tallest=tallest,
                flat_slab=flat_slab, flat_fp_max=flat_fp_max, flat=flat)

def report_bracket(res):
    print("\n=== MAXIMAL BOX — HEIGHT BRACKET (floor fixed, box grows up) ===")
    print(f"component footprint {res['fp_sum']/100:.0f} cm^2   "
          f"bounding volume {res['vol_sum']/1e3:.0f} cm^3   tallest column {res['tallest']:.0f} mm")
    print(f"flat-goods slab on top = {res['flat_slab']:.0f} mm "
          f"(biggest flat footprint {res['flat_fp_max']:.0f} mm -> floor must be >= that):")
    for k, g in res["flat"].items():
        print(f"    {k:32s} {g['t']:4.0f} mm   fp {g['fp'][0]:.0f}x{g['fp'][1]:.0f}")
    print(f"\n{'floor':>10} {'layers':>6} {'interior H':>13} {'EXTERIOR box':>30} {'flat fits?':>10}")
    for r in res["rows"]:
        flag = "yes" if r["flat_fits"] else "NO (floor<platform)"
        cube = "" if r["ext"] > r["ext_hi"] else "  <- near-cubic"
        print(f"{r['floor']:6.0f}mm{r['floor']/25.4:4.1f}\" {r['layers']:6d} "
              f"{r['int_lo']:3.0f}-{r['int_hi']:<3.0f}mm  "
              f"{r['ext']:3.0f}x{r['ext']:3.0f}x{r['ext_lo']:3.0f}-{r['ext_hi']:<3.0f}mm "
              f"({r['ext']/25.4:.1f}sq x {r['ext_lo']/25.4:.1f}-{r['ext_hi']/25.4:.1f}in){cube}  {flag:>10}")
    lo, hi = res["rows"][-1], res["rows"][0]
    print(f"\nMIN height (flattest, {hi['floor']/25.4:.0f}in floor... "
          f"use {lo['floor']/25.4:.0f}in): {lo['ext']:.0f} sq x {lo['ext_lo']:.0f}-{lo['ext_hi']:.0f}mm")
    mid = res["rows"][1]
    print(f"SWEET SPOT (~11-12in): {mid['ext']:.0f} x {mid['ext']:.0f} x "
          f"{mid['ext_lo']:.0f}-{mid['ext_hi']:.0f}mm  "
          f"({mid['ext']/25.4:.1f} x {mid['ext']/25.4:.1f} x {mid['ext_lo']/25.4:.1f}-{mid['ext_hi']/25.4:.1f} in)")

if __name__ == "__main__":
    which = sys.argv[1] if len(sys.argv) > 1 else "maximal"
    if which == "bracket":
        report_bracket(bracket())
    else:
        bom = BOMS[which]
        res = pack(bom)
        report(which, bom, res)
        draw(which, bom, res, os.path.join(HERE, f"box_{which}.png"))
