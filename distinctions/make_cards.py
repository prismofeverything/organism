#!/usr/bin/env python3
"""Build the DISTINCTIONS deck for print: 64 card faces, a back, and proofs.

  python make_cards.py all                 everything, MPC preset
  python make_cards.py faces --preset tgc  just the faces, Game Crafter size
  python make_cards.py back
  python make_cards.py proof               contact sheet of the trimmed cards

The faces are `make_key.card`, the same drawing as the key, so the printed
deck and the key cannot disagree.  The physical card sizes are UNIVERSE's
presets (`universe/deck.py`): MakePlayingCards poker by default.

The back is a target of four rings, each cut into quadrants by a cross:

  quadrants   the cross carries two distinctions: the top half is a black
              field and the bottom white; the left half's figures are blue
              and the right's red.  Diagonally opposite quadrants are
              opposites in both, as every card has its opposite.
  rings       each cell shows one of the four marks a card can carry -- a
              circle (dots), a bar (a band), an eye (lenses) and rays
              (wedges) -- laid as a Latin square, so every ring and every
              quadrant shows all four, once, and no two of the sixteen cells
              are the same.
"""
import argparse
import csv
import math
import pathlib
import subprocess
import sys
import tempfile

from PIL import Image

import make_key as mk

HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE.parent / "universe"))
from deck import PRESETS  # noqa: E402  -- the shared physical card sizes

OUT = HERE / "out"

# card number = the six distinctions as bits, most significant first; the
# same numbering as src/cljc/distinctions/game.cljc `axes`
BITS = [("bg", 5, ["black", "white"]), ("fg", 4, ["red", "blue"]),
        ("comp", 3, ["circle", "bar"]), ("eye", 2, [False, True]),
        ("rays", 1, [False, True]), ("inv", 0, [False, True])]


def card_spec(n):
    return {k: vals[n >> bit & 1] for k, bit, vals in BITS}


def card_name(n):
    s = card_spec(n)
    parts = [s["bg"], s["fg"], s["comp"]]
    parts += [w for w in ("eye", "rays") if s[w]]
    parts += ["inverted"] if s["inv"] else []
    return "-".join(parts)


def rasterize(svg, w, h, dst):
    with tempfile.NamedTemporaryFile("w", suffix=".svg", delete=False) as fh:
        fh.write(svg)
    subprocess.run(["rsvg-convert", "-w", str(w), "-h", str(h), fh.name, "-o", str(dst)],
                   check=True)
    pathlib.Path(fh.name).unlink()


def svg_doc(w, h, body):
    return (f'<svg xmlns="http://www.w3.org/2000/svg" width="{w}" height="{h}" '
            f'viewBox="0 0 {w} {h}">\n{body}\n</svg>')


# ------------------------------------------------------------------- faces

def face_svg(n, spec):
    bleed = spec.ox                                  # px past the cut, each side
    return svg_doc(spec.w, spec.h, mk.card(bleed, bleed, spec.cut_w, bleed=bleed,
                                           **card_spec(n)))


def build_faces(preset="mpc"):
    spec = PRESETS[preset]()
    dst = OUT / preset / "faces"
    dst.mkdir(parents=True, exist_ok=True)
    rows = []
    for n in range(64):
        name = f"{n:02d}_{card_name(n)}.png"
        rasterize(face_svg(n, spec), spec.w, spec.h, dst / name)
        rows.append({"n": n, "file": name, **{k: v for k, v in card_spec(n).items()}})
    with open(OUT / preset / "manifest.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    print(f"  64 faces -> {dst}  ({spec})")


# -------------------------------------------------------------------- back

GROUND = "#24212a"          # between the cells; not black, so black cells read
RINGS = 4
EMBLEM = 0.45               # outer radius, in card widths
CORE = 0.20                 # centre disc radius, as a fraction of EMBLEM
GAP = 0.012                 # gutter between cells, in card widths
FEATURES = ["circle", "bar", "eye", "rays"]

# quadrants in SVG angle order (y down): 0 = right-bottom, then clockwise
#                 field    figure
QUADRANTS = [("white", "red"),     #  0..90    bottom right
             ("white", "blue"),    #  90..180  bottom left
             ("black", "blue"),    # 180..270  top left
             ("black", "red")]     # 270..360  top right


def _pt(cx, cy, r, a):
    return cx + r * math.cos(a), cy + r * math.sin(a)


def sector(cx, cy, r0, r1, a0, a1):
    """Annular sector path, angles in radians."""
    large = 1 if a1 - a0 > math.pi else 0
    p = [_pt(cx, cy, r1, a0), _pt(cx, cy, r1, a1), _pt(cx, cy, r0, a1), _pt(cx, cy, r0, a0)]
    return (f"M{p[0][0]:.2f},{p[0][1]:.2f} A{r1:.2f},{r1:.2f} 0 {large} 1 {p[1][0]:.2f},{p[1][1]:.2f} "
            f"L{p[2][0]:.2f},{p[2][1]:.2f} A{r0:.2f},{r0:.2f} 0 {large} 0 {p[3][0]:.2f},{p[3][1]:.2f}Z")


def feature(kind, cx, cy, r0, r1, a0, a1, color):
    """One mark repeated along a cell, in the cell's figure color."""
    rm, t = (r0 + r1) / 2, r1 - r0
    out = []
    if kind == "bar":                                # one band through the cell
        out.append(f'<path d="{sector(cx, cy, rm - 0.2 * t, rm + 0.2 * t, a0, a1)}" fill="{color}"/>')
        return out
    size = {"circle": 0.62 * t, "eye": 1.5 * t, "rays": 0.62 * t}[kind]
    k = max(2, round((a1 - a0) * rm / (size * 1.45)))
    for i in range(k):
        a = a0 + (i + 0.5) * (a1 - a0) / k
        x, y = _pt(cx, cy, rm, a)
        if kind == "circle":
            out.append(f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{0.31 * t:.2f}" fill="{color}"/>')
        elif kind == "eye":                          # a lens lying along the ring
            ux, uy = -math.sin(a), math.cos(a)       # tangent
            nx, ny = math.cos(a), math.sin(a)        # outward
            hw, hh = size / 2, 0.62 * t              # half-length, control offset
            p0, p1 = (x - ux * hw, y - uy * hw), (x + ux * hw, y + uy * hw)
            c0, c1 = (x + nx * hh, y + ny * hh), (x - nx * hh, y - ny * hh)
            out.append(f'<path d="M{p0[0]:.2f},{p0[1]:.2f} Q{c0[0]:.2f},{c0[1]:.2f} '
                       f'{p1[0]:.2f},{p1[1]:.2f} Q{c1[0]:.2f},{c1[1]:.2f} {p0[0]:.2f},{p0[1]:.2f}Z" '
                       f'fill="{color}"/>')
        else:                                        # a ray: fat inside, tapering out
            ux, uy = -math.sin(a), math.cos(a)
            bx, by = _pt(cx, cy, r0, a)
            tx, ty = _pt(cx, cy, r1, a)
            hb = 0.5 * size
            out.append(f'<polygon points="{bx - ux * hb:.2f},{by - uy * hb:.2f} {tx:.2f},{ty:.2f} '
                       f'{bx + ux * hb:.2f},{by + uy * hb:.2f}" fill="{color}"/>')
    return out


def back_svg(spec):
    """Built on the trimmed card, then laid into the bleed (see universe's
    render_back for why the emblem is centred on the even trimmed size)."""
    w, h = spec.cut_w, spec.cut_h
    cx, cy = spec.ox + w / 2, spec.oy + h / 2
    R = EMBLEM * w
    core = CORE * R
    gap = GAP * w
    band = (R - core) / RINGS
    out = [f'<rect width="{spec.w}" height="{spec.h}" fill="{GROUND}"/>']
    clip = 0
    for ring in range(RINGS):
        r0, r1 = core + ring * band + gap / 2, core + (ring + 1) * band - gap / 2
        for q, (field, figure) in enumerate(QUADRANTS):
            # trim the gutter off each side as a constant width, not an angle
            da0 = math.asin(min(1, gap / 2 / r0))
            a0, a1 = q * math.pi / 2, (q + 1) * math.pi / 2
            path = sector(cx, cy, r0, r1, a0 + da0, a1 - da0)
            clip += 1
            out.append(f'<clipPath id="k{clip}"><path d="{path}"/></clipPath>')
            out.append(f'<path d="{path}" fill="{mk.BW[field]}"/>')
            kind = FEATURES[(ring + q) % len(FEATURES)]
            out.append(f'<g clip-path="url(#k{clip})">')
            out += feature(kind, cx, cy, r0, r1, a0 + da0, a1 - da0, mk.RB[figure])
            out.append('</g>')
    # the centre: each quadrant's figure color meeting at the middle
    for q, (_, figure) in enumerate(QUADRANTS):
        da = math.asin(min(1, gap / 2 / (core - gap / 2)))
        a0, a1 = q * math.pi / 2 + da, (q + 1) * math.pi / 2 - da
        p0, p1 = _pt(cx, cy, core - gap / 2, a0), _pt(cx, cy, core - gap / 2, a1)
        off = gap / 2 / math.sin(math.pi / 4)
        m = _pt(cx, cy, off, (a0 + a1) / 2)
        out.append(f'<path d="M{m[0]:.2f},{m[1]:.2f} L{p0[0]:.2f},{p0[1]:.2f} '
                   f'A{core - gap / 2:.2f},{core - gap / 2:.2f} 0 0 1 {p1[0]:.2f},{p1[1]:.2f}Z" '
                   f'fill="{mk.RB[figure]}"/>')
    return svg_doc(spec.w, spec.h, "\n".join(out))


# ------------------------------------------------------------- pairs back
#
# Every pair of distinctions gets a ring -- six distinctions, fifteen pairs,
# fifteen rings -- and each ring's four quadrants hold that pair's four
# combinations: the pair's first distinction flips top to bottom, its second
# right to left, so diagonal quadrants are opposites in both.  A cell shows
# only its own two distinctions; the other four are drawn neutral (a grey
# field, a grey figure) or left out (no eye, no rays).  The rings are sized
# so the fifteenth reaches the corners of the bleed: no ground is left, and
# the outer rings run off the card.

AXES = ["bg", "fg", "comp", "eye", "rays", "inv"]
VALUES = {k: vals for k, _, vals in BITS}
NEUTRAL_FIELD, NEUTRAL_FIGURE = "#3b3742", "#b9b4c0"
NEUTRAL_EYE, NEUTRAL_RAY = "#e4e0e8", "#7c7884"
PAIR_GAP = 0.005            # gutter, in card widths


def ring_order(first=("bg", "fg")):
    """The fifteen pairs in an order where each ring shares a distinction
    with the one inside it, starting from background/foreground."""
    pairs = [(a, b) for i, a in enumerate(AXES) for b in AXES[i + 1:]]

    def walk(path):
        if len(path) == len(pairs):
            return path
        for p in pairs:
            if p not in path and set(p) & set(path[-1]):
                found = walk(path + [p])
                if found:
                    return found
        return None
    return walk([first])


def cell_colors(spec):
    bg, fg = spec.get("bg"), spec.get("fg")
    field = mk.BW[bg] if bg else NEUTRAL_FIELD
    figure = mk.RB[fg] if fg else NEUTRAL_FIGURE
    eye_c = mk.BW[mk.OTHER[bg]] if bg else NEUTRAL_EYE
    ray_c = mk.RB[mk.OTHER[fg]] if fg else NEUTRAL_RAY
    if spec.get("inv"):                    # as on the card: swap, and the eye
        field, figure = figure, field      # and rays trade palettes
        eye_c, ray_c = (mk.RB[mk.OTHER[fg]] if fg else NEUTRAL_RAY,
                        mk.BW[mk.OTHER[bg]] if bg else NEUTRAL_EYE)
    return field, figure, eye_c, ray_c


def pair_cell(cx, cy, r0, r1, a0, a1, spec):
    """One combination as a sliver of card: marks repeat along the arc, each
    slot stacked like the card itself -- bar behind, then rays, the eye, and
    the circle on top -- so an eye and a circle in one cell make a small eye."""
    field, figure, eye_c, ray_c = cell_colors(spec)
    t = r1 - r0
    out = []
    if r0 <= 0:                                       # the centre disc
        p0, p1 = _pt(cx, cy, r1, a0), _pt(cx, cy, r1, a1)
        out.append(f'<path d="M{cx:.2f},{cy:.2f} L{p0[0]:.2f},{p0[1]:.2f} A{r1:.2f},{r1:.2f} 0 0 1 '
                   f'{p1[0]:.2f},{p1[1]:.2f}Z" fill="{field}"/>')
        q0, q1 = _pt(cx, cy, r1 * 0.55, a0), _pt(cx, cy, r1 * 0.55, a1)
        out.append(f'<path d="M{cx:.2f},{cy:.2f} L{q0[0]:.2f},{q0[1]:.2f} A{r1 * .55:.2f},{r1 * .55:.2f} 0 0 1 '
                   f'{q1[0]:.2f},{q1[1]:.2f}Z" fill="{figure}"/>')
        return out
    out.append(f'<path d="{sector(cx, cy, r0, r1, a0, a1)}" fill="{field}"/>')
    rm = (r0 + r1) / 2
    comp = spec.get("comp")
    if comp == "bar":
        out.append(f'<path d="{sector(cx, cy, rm - .22 * t, rm + .22 * t, a0, a1)}" fill="{figure}"/>')
    elif comp is None:                                # figure with no shape: a thread
        out.append(f'<path d="{sector(cx, cy, rm - .07 * t, rm + .07 * t, a0, a1)}" fill="{figure}"/>')
    k = max(1, round((a1 - a0) * rm / (1.3 * t)))
    slot = (a1 - a0) * rm / k
    for i in range(k):
        a = a0 + (i + 0.5) * (a1 - a0) / k
        x, y = _pt(cx, cy, rm, a)
        ux, uy = -math.sin(a), math.cos(a)            # along the ring
        nx, ny = math.cos(a), math.sin(a)             # outward
        if spec.get("rays"):
            bx, by = _pt(cx, cy, r0, a)
            tx, ty = _pt(cx, cy, r1, a)
            hb = 0.3 * t
            out.append(f'<polygon points="{bx - ux * hb:.2f},{by - uy * hb:.2f} {tx:.2f},{ty:.2f} '
                       f'{bx + ux * hb:.2f},{by + uy * hb:.2f}" fill="{ray_c}"/>')
        if spec.get("eye"):
            hw, hh = min(0.46 * slot, 0.95 * t), 0.78 * t
            p0, p1 = (x - ux * hw, y - uy * hw), (x + ux * hw, y + uy * hw)
            c0, c1 = (x + nx * hh, y + ny * hh), (x - nx * hh, y - ny * hh)
            out.append(f'<path d="M{p0[0]:.2f},{p0[1]:.2f} Q{c0[0]:.2f},{c0[1]:.2f} '
                       f'{p1[0]:.2f},{p1[1]:.2f} Q{c1[0]:.2f},{c1[1]:.2f} {p0[0]:.2f},{p0[1]:.2f}Z" '
                       f'fill="{eye_c}"/>')
        if comp == "circle":
            out.append(f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{0.25 * t:.2f}" fill="{figure}"/>')
    return out


def ring_turns(n, seed):
    """A random turn for each of n rings, in degrees; seed None turns none.
    Seeded, so a turned back can be rebuilt exactly or another seed tried."""
    if seed is None:
        return [0.0] * n
    import random
    rng = random.Random(seed)
    return [rng.uniform(0, 360) for _ in range(n)]


def turned(cx, cy, deg, parts):
    """Wrap one ring's elements so the whole ring turns as a piece."""
    if not deg:
        return parts
    return [f'<g transform="rotate({deg:.3f} {cx:.2f} {cy:.2f})">', *parts, '</g>']


def back_pairs_svg(spec, seed=None):
    w, h = spec.cut_w, spec.cut_h
    cx, cy = spec.ox + w / 2, spec.oy + h / 2
    order = ring_order()
    reach = math.hypot(spec.w / 2, spec.h / 2)       # to the corners of the bleed
    t = reach / len(order)
    gap = PAIR_GAP * w
    out = [f'<rect width="{spec.w}" height="{spec.h}" fill="{GROUND}"/>']
    clip = 0
    turns = ring_turns(len(order), seed)
    for ring, (a, b) in enumerate(order):
        body = []
        r0 = 0 if ring == 0 else ring * t + gap / 2
        r1 = (ring + 1) * t - gap / 2
        for q in range(4):
            # SVG angles run clockwise from the right: quadrant q spans
            # q*90..(q+1)*90, so 0 = bottom right, 1 = bottom left, 2 = top
            # left, 3 = top right.  Top/bottom carries a, right/left carries b.
            va = 1 if q in (0, 1) else 0
            vb = 1 if q in (1, 2) else 0
            cell = {a: VALUES[a][va], b: VALUES[b][vb]}
            da = math.asin(min(1, gap / 2 / max(r0, gap)))
            a0, a1 = q * math.pi / 2 + da, (q + 1) * math.pi / 2 - da
            if ring == 0:
                off = gap / 2 / math.sin(math.pi / 4)
                ccx, ccy = _pt(cx, cy, off, (a0 + a1) / 2)
                body += pair_cell(ccx, ccy, 0, r1 - off, a0, a1, cell)
                continue
            path = sector(cx, cy, r0, r1, a0, a1)
            clip += 1
            body.append(f'<clipPath id="p{clip}"><path d="{path}"/></clipPath>')
            body.append(f'<g clip-path="url(#p{clip})">')
            body += pair_cell(cx, cy, r0, r1, a0, a1, cell)
            body.append('</g>')
        out += turned(cx, cy, turns[ring], body)
    print("  rings, centre out: " + "  ".join(f"{a}/{b}" for a, b in order))
    return svg_doc(spec.w, spec.h, "\n".join(out))


def build_back_pairs(preset="mpc", seed=None):
    spec = PRESETS[preset]()
    (OUT / preset).mkdir(parents=True, exist_ok=True)
    svg = back_pairs_svg(spec, seed)
    name = "back-pairs" + ("" if seed is None else f"-turned-{seed}")
    (OUT / preset / f"{name}.svg").write_text(svg)
    rasterize(svg, spec.w, spec.h, OUT / preset / f"{name}.png")
    print(f"  pairs back -> {OUT / preset / name}.png")


# ------------------------------------------------------------ jigsaw back
#
# Stack all 64 cards and cut the stack into rings and sectors: each card gives
# exactly one piece, the piece of its own face that lies where it sits, and the
# pieces fit back together into one disc.  Which card sits where comes from
# jigsaw_tiling.py: every ring is a group of cards that agree on all but two
# (16 rings of 4) or three (8 rings of 8) distinctions, with every combination
# of those once around the ring.  Sectors directly opposite each other hold
# cards that are opposite in every varying distinction.

JIGSAW_GAP = 0.004          # gutter, in card widths

# around the ring, clockwise from the top: opposite sectors are complements,
# and six of the eight steps change a single distinction (all eight can't,
# with complements opposite: four single steps flip an even number of bits)
EIGHTHS = [0b000, 0b001, 0b011, 0b010, 0b111, 0b110, 0b100, 0b101]


def wedge(cx, cy, r1, a0, a1):
    p0, p1 = _pt(cx, cy, r1, a0), _pt(cx, cy, r1, a1)
    return (f"M{cx:.2f},{cy:.2f} L{p0[0]:.2f},{p0[1]:.2f} "
            f"A{r1:.2f},{r1:.2f} 0 0 1 {p1[0]:.2f},{p1[1]:.2f}Z")


def jigsaw_cells(group):
    """(card, a0, a1) for each of the group's cards, angles in radians,
    SVG-clockwise from the right."""
    axes, base = group["vary"], group["base"]
    bit = {k: b for k, b, _ in BITS}

    def card(values):
        return base | sum(1 << bit[a] for a, v in zip(axes, values) if v)
    if len(axes) == 2:
        # the first distinction flips top to bottom, the second right to left
        out = []
        for q in range(4):          # 0 bottom right, 1 bottom left, 2 top left, 3 top right
            out.append((card([q in (0, 1), q in (1, 2)]),
                        q * math.pi / 2, (q + 1) * math.pi / 2))
        return out
    step = math.pi / 4
    top = -math.pi / 2
    return [(card([code >> 2 & 1, code >> 1 & 1, code & 1]),
             top + s * step, top + (s + 1) * step) for s, code in enumerate(EIGHTHS)]


def back_jigsaw_svg(spec, tiling, seed=None):
    w = spec.cut_w
    cx, cy = spec.ox + w / 2, spec.oy + spec.cut_h / 2
    t = math.hypot(spec.w / 2, spec.h / 2) / len(tiling)
    bleed = spec.ox
    out = [f'<rect width="{spec.w}" height="{spec.h}" fill="{GROUND}"/>']
    used = []
    gap = JIGSAW_GAP * w
    stroke = f'fill="none" stroke="{GROUND}" stroke-width="{gap:.2f}"'
    turns = ring_turns(len(tiling), seed)
    for ring, group in enumerate(tiling):
        r0, r1 = ring * t, (ring + 1) * t
        # A turn moves the cuts, not the pictures: each card still gives the
        # part of its own face that lies under its (turned) sector, so every
        # piece stays true to its card and the disc stays filled to the edge.
        spin = (f' transform="rotate({turns[ring]:.3f} {cx:.2f} {cy:.2f})"'
                if turns[ring] else "")
        cells = jigsaw_cells(group)
        for n, a0, a1 in cells:
            used.append(n)
            path = wedge(cx, cy, r1, a0, a1) if ring == 0 else sector(cx, cy, r0, r1, a0, a1)
            cid = f"j{n}"
            out.append(f'<clipPath id="{cid}"><path d="{path}"{spin}/></clipPath>')
            out.append(f'<g clip-path="url(#{cid})">')
            out.append(mk.card(bleed, bleed, w, bleed=bleed, **card_spec(n)))
            out.append('</g>')
        for _, a0, _ in cells:
            (x0, y0), (x1, y1) = _pt(cx, cy, r0, a0), _pt(cx, cy, r1, a0)
            out.append(f'<line x1="{x0:.2f}" y1="{y0:.2f}" x2="{x1:.2f}" y2="{y1:.2f}" {stroke}{spin}/>')
    assert sorted(used) == list(range(64)), "every card exactly once"
    # ring boundaries on top
    out += [f'<circle cx="{cx:.2f}" cy="{cy:.2f}" r="{ring * t:.2f}" {stroke}/>'
            for ring in range(1, len(tiling))]
    return svg_doc(spec.w, spec.h, "\n".join(out))


def build_back_jigsaw(preset="mpc", vary=3, seed=None):
    import json
    spec = PRESETS[preset]()
    tiling = json.loads((HERE / f"jigsaw-{vary}.json").read_text())
    (OUT / preset).mkdir(parents=True, exist_ok=True)
    svg = back_jigsaw_svg(spec, tiling, seed)
    name = f"back-jigsaw-{vary}" + ("" if seed is None else f"-turned-{seed}")
    (OUT / preset / f"{name}.svg").write_text(svg)
    rasterize(svg, spec.w, spec.h, OUT / preset / f"{name}.png")
    print(f"  jigsaw back ({len(tiling)} rings) -> {OUT / preset / name}.png")


def build_back(preset="mpc"):
    spec = PRESETS[preset]()
    (OUT / preset).mkdir(parents=True, exist_ok=True)
    (OUT / preset / "back.svg").write_text(back_svg(spec))
    rasterize(back_svg(spec), spec.w, spec.h, OUT / preset / "back.png")
    print(f"  back -> {OUT / preset / 'back.png'}")


# ------------------------------------------------------------------- proof

def build_proof(cols=8, card_w=180, pad=14):
    """The 64 trimmed faces in card order, and the back beside them."""
    spec = PRESETS["cut"]()
    ch = round(card_w * mk.ASPECT)
    rows = 64 // cols
    W = (cols + 2) * (card_w + pad) + pad
    H = rows * (ch + pad) + pad
    sheet = Image.new("RGB", (W, H), (236, 234, 238))
    with tempfile.TemporaryDirectory() as tmp:
        for n in range(64):
            p = pathlib.Path(tmp) / f"{n}.png"
            rasterize(face_svg(n, spec), card_w, ch, p)
            sheet.paste(Image.open(p), (pad + (n % cols) * (card_w + pad),
                                        pad + (n // cols) * (ch + pad)))
        p = pathlib.Path(tmp) / "back.png"
        rasterize(back_svg(spec), card_w * 2, ch * 2, p)
        sheet.paste(Image.open(p), (pad + cols * (card_w + pad), pad))
    dst = OUT / "proof.png"
    dst.parent.mkdir(exist_ok=True)
    sheet.save(dst)
    print(f"  proof -> {dst}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("what", choices=["all", "faces", "back", "pairs", "jigsaw", "proof"])
    ap.add_argument("--preset", default="mpc", choices=list(PRESETS))
    ap.add_argument("--turn", type=int, default=None, metavar="SEED",
                    help="pairs/jigsaw backs: turn each ring by a random angle "
                         "(seeded), written as back-*-turned-SEED")
    a = ap.parse_args()
    if a.what in ("all", "faces"):
        build_faces(a.preset)
    if a.what in ("all", "back"):
        build_back(a.preset)
    if a.what in ("all", "pairs"):
        build_back_pairs(a.preset, a.turn)
    if a.what in ("all", "jigsaw"):
        for vary in (3, 2):
            if (HERE / f"jigsaw-{vary}.json").exists():
                build_back_jigsaw(a.preset, vary, a.turn)
    if a.what in ("all", "proof"):
        build_proof()


if __name__ == "__main__":
    main()
