#!/usr/bin/env python3
"""The hand space as a figure: three axes converging on the singularity.

Shape on the left, any-suits up the middle, color on the right, all leaning
in towards the one hand at the top.  Each node carries a glyph for its number
pattern -- dots for cards, joined where they match -- its name, its odds, and
a real example hand.  Arrows run from the middle out to either side, and the
two sequences curve up to the apex.

Everything is vector: the card art is traced from the mattes by
trace_shapes.py and placed with the same rosette geometry the printed cards
use, so an example here is the card.
"""
import json
import math
import pathlib

from PIL import ImageFont

import deck

HERE = pathlib.Path(__file__).parent

NAMES = {"one pair": "DYAD", "two pair": "SPLIT TETRAD", "three of a kind": "TRIAD",
         "full house": "SPLIT PENTAD", "four of a kind": "TETRAD",
         "five of a kind": "PENTAD", "straight": "SEQUENCE"}
GROUPS = {"one pair": [2], "two pair": [2, 2], "three of a kind": [3],
          "full house": [3, 2], "four of a kind": [4], "five of a kind": [5],
          "straight": [1, 1, 1, 1, 1]}
PREFIX = {"mixed": "", "color": "COLOR ", "shape": "SHAPE ", "perfect": ""}

CX, W = 1700, 3400
YTOP = 470                    # the singularity, and the top of the scale
SCALE = 500.0                 # pixels per decade of rarity
MINGAP = 262                  # ...but never closer than one node block is tall
XMIN, XMAX = 480, 1650        # the fan, from apex to foot
BIG = 94                      # card width for the top hand on each axis
CARD_W = 78
CARD_H = round(CARD_W * 1050 / 750)
FIELD = 636 / 750 / 2         # rosette radius as a fraction of card width

INK = "#1a1622"
GREY = "#7c7884"
LINE = "#b9b5c4"
FAINT = "#dedbe6"
SANS = "Inter, Helvetica, Arial, sans-serif"
DISPLAY = "URW Gothic, Century Gothic, Inter, sans-serif"


def esc(t):
    return t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def text(x, y, s, size, fill=INK, weight="400", anchor="middle", family=SANS, ls=0):
    sp = f' letter-spacing="{ls}"' if ls else ""
    return (f'<text x="{x:.1f}" y="{y:.1f}" font-family="{family}" font-size="{size}" '
            f'font-weight="{weight}" fill="{fill}" text-anchor="{anchor}"{sp}>{esc(s)}</text>')


# ----------------------------------------------------------------- the cards

PATHS = json.loads((HERE / "shapes" / "paths.json").read_text())
_PTS = {}
for _n, _d in PATHS.items():
    _v = [float(v) for v in _d.replace("M", " ").replace("L", " ")
          .replace("Z", " ").replace(",", " ").split()]
    _PTS[_n] = list(zip(_v[0::2], _v[1::2]))


def rosette(shape, n, table):
    """Copy placements in units of one shape's enclosing radius, plus the
    distance the whole arrangement reaches -- the same construction deck.py
    rasterises, so the vector card matches the printed one."""
    t = table[f"{shape}:{n}"]
    base = 0.0 if n == 1 else t / math.sin(math.pi / n)
    out, reach = [], 0.0
    for k in range(n):
        phi = 360.0 * k / n
        rad = math.radians(phi)
        cx, cy = base * math.sin(rad), -base * math.cos(rad)
        out.append((phi, cx, cy))
        cos, sin = math.cos(rad), math.sin(rad)
        for px, py in _PTS[shape]:
            rx, ry = px * cos - py * sin, px * sin + py * cos      # clockwise
            reach = max(reach, math.hypot(rx + cx, ry + cy))
    return out, reach


def card(x, y, color, shape, number, table, w=CARD_W):
    """One example card, top-left at (x, y).  color/shape/number arrive as the
    indices hands.json stores."""
    n = number + 1
    h = w * 1050 / 750
    shape = deck.SHAPES[shape]
    places, reach = rosette(shape, n, table)
    s = FIELD * w / reach
    cx, cy = x + w / 2, y + h / 2
    ink = deck.COLORS[deck.COLOR_ORDER[color]]
    g = [f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" rx="5" '
         f'fill="#ffffff" stroke="{FAINT}" stroke-width="1.5"/>']
    for phi, ox, oy in places:
        g.append(f'<g transform="translate({cx + ox*s:.2f},{cy + oy*s:.2f}) '
                 f'rotate({phi:.2f}) scale({s:.4f})">'
                 f'<use href="#sh-{shape}" fill="{ink}"/></g>')
    g.append(text(x + w * 0.14, y + h * 0.183, str(n), round(w * 0.19), ink, "600",
                  "middle", DISPLAY))
    return "".join(g)


def hand(x, y, cards, table, w=CARD_W):
    """Five cards in a row, centred on x."""
    gap = max(4, round(w * 0.077))
    x0 = x - (5 * w + 4 * gap) / 2
    return "".join(card(x0 + i * (w + gap), y, c, s, nn, table, w)
                   for i, (c, s, nn) in
                   enumerate(sorted(cards, key=lambda t: (t[2], t[0], t[1]))))


# ---------------------------------------------------------------- the glyphs

def glyph(x, y, groups, r=26, ink=INK):
    """Dots are cards; a ring joining them means they share a number.  Five
    loose dots is the sequence, where nothing matches at all."""
    out = []
    if groups == [1, 1, 1, 1, 1]:
        for i in range(5):
            out.append(f'<circle cx="{x - 46 + i*23:.1f}" cy="{y:.1f}" r="6" fill="{ink}"/>')
        return "".join(out)
    span = sum(2 * r + 14 for _ in groups) - 14
    cx = x - span / 2 + r
    for k in groups:
        if k == 2:
            pts = [(cx, y - r * 0.82), (cx, y + r * 0.82)]
        else:
            pts = [(cx + r * math.sin(2 * math.pi * i / k),
                    y - r * math.cos(2 * math.pi * i / k)) for i in range(k)]
        if len(pts) > 2:
            out.append('<polygon points="' + " ".join(f"{a:.1f},{b:.1f}" for a, b in pts)
                       + f'" fill="none" stroke="{ink}" stroke-width="2.4"/>')
        else:
            out.append(f'<line x1="{pts[0][0]:.1f}" y1="{pts[0][1]:.1f}" '
                       f'x2="{pts[1][0]:.1f}" y2="{pts[1][1]:.1f}" '
                       f'stroke="{ink}" stroke-width="2.4"/>')
        for a, b in pts:
            out.append(f'<circle cx="{a:.1f}" cy="{b:.1f}" r="6" fill="{ink}"/>')
        cx += 2 * r + 14
    return "".join(out)


# ------------------------------------------------------------------ assembly

def main():
    data = json.loads((HERE / "hands.json").read_text())
    total = data["total"]
    cell = {(r["numbers"], r["suit"]): (r["count"], i, r["example"])
            for i, r in enumerate(data["rows"], start=1)}
    table = deck.ring_table()

    # One vertical level per distinct probability, rarest at the top.  Spacing
    # is logarithmic, but opened out to MINGAP wherever two hands sit so close
    # together that their blocks would collide -- so the ORDER is exact
    # everywhere, and only the spacing gives, and only where it must.  Levels
    # come out unevenly spaced along any one axis, which is the honest result.
    counts = sorted({v[0] for v in cell.values()}, reverse=True)   # commonest first
    ys, y, prev = {}, 0.0, None
    for c in counts:
        if prev is not None:
            y -= max(math.log10(prev / c) * SCALE, MINGAP)
        ys[c] = y
        prev = c
    lo = min(ys.values())
    ys = {c: YTOP + (v - lo) for c, v in ys.items()}
    span = max(ys.values()) - YTOP
    H = round(max(ys.values()) + 300 + 150)

    axes = {}
    for suit in ("shape", "mixed", "color"):
        got = [(n, cell[(n, suit)]) for n in NAMES if (n, suit) in cell]
        got.sort(key=lambda t: t[1][0])                # rarest first
        axes[suit] = got

    # let the canvas follow the flare, not the other way round
    strip = 5 * CARD_W + 4 * 6
    maxoff = max(XMIN + (ys[cell[(n, suit)][0]] - YTOP) / span * (XMAX - XMIN)
                 for suit in ("shape", "color") for n, _ in axes[suit])
    GUTTER = 170                              # plain margin
    CX = round(maxoff + strip / 2 + GUTTER)
    W = 2 * CX

    def axis_x(suit, y):
        if suit == "mixed":
            return CX
        side = -1 if suit == "shape" else 1
        return CX + side * (XMIN + (y - YTOP) / span * (XMAX - XMIN))

    def place(suit, n):
        y = ys[cell[(n, suit)][0]]
        return axis_x(suit, y), y

    APEX = (CX, YTOP)

    svg = [f'<svg xmlns="http://www.w3.org/2000/svg" '
           f'xmlns:xlink="http://www.w3.org/1999/xlink" width="{W}" height="{H}" '
           f'viewBox="0 0 {W} {H}">',
           '<defs>']
    for name, d in PATHS.items():
        svg.append(f'<path id="sh-{name}" d="{d}" fill-rule="evenodd"/>')
    svg.append(f'<marker id="ah" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" '
               f'markerHeight="7" orient="auto-start-reverse">'
               f'<path d="M0,1 L10,5 L0,9 z" fill="{LINE}"/></marker>')
    svg.append(f'<marker id="ah2" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" '
               f'markerHeight="7" orient="auto-start-reverse">'
               f'<path d="M0,1 L10,5 L0,9 z" fill="{GREY}"/></marker>')
    svg.append('</defs>')
    svg.append(f'<rect width="{W}" height="{H}" fill="#ffffff"/>')

    svg.append(text(CX, 158, "UNIVERSE", 66, INK, "400", "middle", DISPLAY, 10))
    svg.append(text(CX, 214, "five card hands", 30, GREY, "400", "middle"))

    # one rule per distinct probability: every hand sits on its own line, in
    # order, and the two that are exactly tied share one
    for c, yy in sorted(ys.items(), key=lambda kv: kv[1]):
        svg.append(f'<line x1="{GUTTER}" y1="{yy:.1f}" x2="{W-GUTTER}" y2="{yy:.1f}" '
                   f'stroke="#f1eff5" stroke-width="1.5"/>')

    # axis spines: the fan is linear in y, so each axis is a straight line
    for suit in ("shape", "mixed", "color"):
        _, ytop = place(suit, axes[suit][0][0])
        _, ybot = place(suit, axes[suit][-1][0])
        y0, y1 = ytop + 48, ybot + 250        # stop under the top glyph
        svg.append(f'<line x1="{axis_x(suit, y0):.1f}" y1="{y0:.1f}" '
                   f'x2="{axis_x(suit, y1):.1f}" y2="{y1:.1f}" '
                   f'stroke="{FAINT}" stroke-width="3"/>')

    # arrows from the middle out to either side
    have = {suit: {n for n, _ in axes[suit]} for suit in axes}
    for n, _ in axes["mixed"]:
        for suit in ("shape", "color"):
            if n not in have[suit]:
                continue
            ax, ay = place("mixed", n)
            bx, by = place(suit, n)
            ux, uy = bx - ax, by - ay
            L = math.hypot(ux, uy)
            ax, ay = ax + ux / L * 140, ay + uy / L * 140
            bx, by = bx - ux / L * 140, by - uy / L * 140
            svg.append(f'<line x1="{ax:.1f}" y1="{ay:.1f}" x2="{bx:.1f}" y2="{by:.1f}" '
                       f'stroke="{LINE}" stroke-width="2.5" marker-end="url(#ah)"/>')

    # the two sequences sweeping up to the apex.  A real elliptical arc, not a
    # cubic: a Bezier stretched this far goes slack in the middle and kinks at
    # the end, which read as two straight lines meeting at a corner.  The two
    # arcs are the left and right sides of one big oval round the top.
    title_f = ImageFont.truetype(
        "/usr/share/fonts/opentype/inter/Inter-SemiBold.otf", 28)
    for suit, side in (("shape", -1), ("color", 1)):
        sx, sy = place(suit, "straight")
        label = PREFIX[suit] + NAMES["straight"]
        half = (title_f.getlength(label) + 1.2 * len(label)) / 2
        x0, y0 = sx + side * (half + 18), sy + 50        # off the end of the title
        x1, y1 = CX + side * 110, APEX[1] + 2
        r = math.hypot(x1 - x0, y1 - y0) * 0.60          # 0.5 would be a half circle
        sweep = 1 if side < 0 else 0
        svg.append(f'<path d="M{x0:.1f},{y0:.1f} '
                   f'A{r:.1f},{r:.1f} 0 0 {sweep} {x1:.1f},{y1:.1f}" fill="none" '
                   f'stroke="{GREY}" stroke-width="3" marker-end="url(#ah2)"/>')

    # nodes
    for suit in ("shape", "mixed", "color"):
        head = axes[suit][0][0]               # the pinnacle of this axis
        for n, (count, rank, example) in axes[suit]:
            x, y = place(suit, n)
            up = n == head
            odds = total / count
            txt = f"1 in {odds:,.1f}" if odds < 100 else f"1 in {round(odds):,}"
            svg.append(glyph(x, y, GROUPS[n], r=33 if up else 26))
            svg.append(text(x, y + (74 if up else 60), PREFIX[suit] + NAMES[n],
                            34 if up else 28, INK, "600", "middle", SANS, 1.2))
            svg.append(text(x, y + (106 if up else 88), f"{txt}   ·   #{rank}",
                            24 if up else 22, GREY))
            svg.append(hand(x, y + (122 if up else 102), example, table,
                            BIG if up else CARD_W))

    # the apex.  Any of the twelve suits would do; shown as the purple eye,
    # the deck's own mark.
    count, rank, _ = cell[("straight", "perfect")]
    example = [[0, 0, k] for k in range(5)]        # purple, eye, 1..5
    violet = deck.COLORS["purple"]
    ax, ay = APEX
    svg.append(f'<circle cx="{ax}" cy="{ay}" r="104" fill="none" stroke="{violet}" '
               f'stroke-width="3.5"/>')
    svg.append(glyph(ax, ay, GROUPS["straight"], ink=violet))
    svg.append(text(ax, ay + 148, "SINGULARITY", 36, violet, "600", "middle", SANS, 2))
    svg.append(text(ax, ay + 182, f"1 in {round(total/count):,}   ·   #{rank}", 23, GREY))
    svg.append(hand(ax, ay + 214, example, table, BIG))

    # axis feet
    for suit, label in (("shape", "SHAPE"), ("mixed", "MIX"), ("color", "COLOR")):
        x, y = place(suit, axes[suit][-1][0])
        svg.append(text(x, y + 268, label, 34, LINE, "600", "middle", SANS, 4))

    svg.append('</svg>')
    out = HERE / "out" / "universe-pyramid.svg"
    out.parent.mkdir(exist_ok=True)
    out.write_text("\n".join(svg))
    print(f"  {W}x{H}  {out.stat().st_size/1024:.0f} KB -> {out}")


if __name__ == "__main__":
    main()
