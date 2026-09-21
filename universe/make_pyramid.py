#!/usr/bin/env python3
"""The hand space as a figure: three axes converging on the singularity.

Shape on the left, any-suits up the middle, colour on the right, all leaning
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

import deck

HERE = pathlib.Path(__file__).parent

NAMES = {"one pair": "DYAD", "two pair": "SPLIT TETRAD", "three of a kind": "TRIAD",
         "full house": "SPLIT PENTAD", "four of a kind": "TETRAD",
         "five of a kind": "PENTAD", "straight": "SEQUENCE"}
GROUPS = {"one pair": [2], "two pair": [2, 2], "three of a kind": [3],
          "full house": [3, 2], "four of a kind": [4], "five of a kind": [5],
          "straight": [1, 1, 1, 1, 1]}
PREFIX = {"mixed": "", "colour": "COLOUR ", "shape": "SHAPE ", "perfect": ""}

CX, W, H = 1850, 3700, 4340
P = 372                       # pitch between nodes on an axis
Y0 = 1060                     # top node of the two side axes
XTOP, DX = 530, 158           # fan of the side axes
APEX = (CX, 560)
CARD_W = 88
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


def card(x, y, colour, shape, number, table):
    """One example card, top-left at (x, y).  colour/shape/number arrive as the
    indices hands.json stores."""
    n = number + 1
    shape = deck.SHAPES[shape]
    places, reach = rosette(shape, n, table)
    s = FIELD * CARD_W / reach
    cx, cy = x + CARD_W / 2, y + CARD_H / 2
    ink = deck.COLOURS[deck.COLOUR_ORDER[colour]]
    g = [f'<rect x="{x:.1f}" y="{y:.1f}" width="{CARD_W}" height="{CARD_H}" rx="5" '
         f'fill="#ffffff" stroke="{FAINT}" stroke-width="1.5"/>']
    for phi, ox, oy in places:
        g.append(f'<g transform="translate({cx + ox*s:.2f},{cy + oy*s:.2f}) '
                 f'rotate({phi:.2f}) scale({s:.4f})">'
                 f'<use href="#sh-{shape}" fill="{ink}"/></g>')
    g.append(text(x + 11, y + 20, str(n), 15, ink, "600", "middle", DISPLAY))
    return "".join(g)


def hand(x, y, cards, table):
    """Five cards in a row, centred on x."""
    total = 5 * CARD_W + 4 * 7
    x0 = x - total / 2
    return "".join(card(x0 + i * (CARD_W + 7), y, c, s, nn, table)
                   for i, (c, s, nn) in
                   enumerate(sorted(cards, key=lambda t: (t[2], t[0], t[1]))))


# ---------------------------------------------------------------- the glyphs

def glyph(x, y, groups, r=30):
    """Dots are cards; a ring joining them means they share a number.  Five
    loose dots is the sequence, where nothing matches at all."""
    out = []
    if groups == [1, 1, 1, 1, 1]:
        for i in range(5):
            out.append(f'<circle cx="{x - 52 + i*26:.1f}" cy="{y:.1f}" r="6.5" fill="{INK}"/>')
        return "".join(out)
    span = sum(2 * r + 16 for _ in groups) - 16
    cx = x - span / 2 + r
    for k in groups:
        if k == 2:
            pts = [(cx, y - r * 0.82), (cx, y + r * 0.82)]
        else:
            pts = [(cx + r * math.sin(2 * math.pi * i / k),
                    y - r * math.cos(2 * math.pi * i / k)) for i in range(k)]
        if len(pts) > 2:
            out.append('<polygon points="' + " ".join(f"{a:.1f},{b:.1f}" for a, b in pts)
                       + f'" fill="none" stroke="{INK}" stroke-width="2.4"/>')
        else:
            out.append(f'<line x1="{pts[0][0]:.1f}" y1="{pts[0][1]:.1f}" '
                       f'x2="{pts[1][0]:.1f}" y2="{pts[1][1]:.1f}" '
                       f'stroke="{INK}" stroke-width="2.4"/>')
        for a, b in pts:
            out.append(f'<circle cx="{a:.1f}" cy="{b:.1f}" r="6.5" fill="{INK}"/>')
        cx += 2 * r + 16
    return "".join(out)


# ------------------------------------------------------------------ assembly

def main():
    data = json.loads((HERE / "hands.json").read_text())
    total = data["total"]
    cell = {(r["numbers"], r["suit"]): (r["count"], i, r["example"])
            for i, r in enumerate(data["rows"], start=1)}
    table = deck.ring_table()

    axes = {}
    for suit in ("shape", "mixed", "colour"):
        got = [(n, cell[(n, suit)]) for n in NAMES if (n, suit) in cell]
        got.sort(key=lambda t: t[1][0])                # rarest first
        axes[suit] = got

    def place(suit, i):
        if suit == "mixed":
            return CX, Y0 + 0.5 * P + i * P
        side = -1 if suit == "shape" else 1
        return CX + side * (XTOP + i * DX), Y0 + i * P

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

    svg.append(text(150, 150, "UNIVERSE · the nineteen hands", 62, INK, "400",
                    "start", DISPLAY))
    svg.append(text(150, 202, "seven number patterns, each one reachable in any suits, "
                    "in one colour, or in one shape — leaning in towards the single "
                    "hand that is both.", 27, GREY, "400", "start"))
    svg.append(text(150, 240, "dots are cards; a ring joining them means they share a "
                    "number.  five loose dots is the sequence, where nothing matches.",
                    27, GREY, "400", "start"))

    # axis spines
    for suit in ("shape", "mixed", "colour"):
        x0, y0 = place(suit, 0)
        x1, y1 = place(suit, len(axes[suit]) - 1)
        svg.append(f'<line x1="{x0:.1f}" y1="{y0 - 120:.1f}" x2="{x1:.1f}" '
                   f'y2="{y1 + 250:.1f}" stroke="{FAINT}" stroke-width="3"/>')

    # arrows from the middle out to either side
    idx = {suit: {n: i for i, (n, _) in enumerate(axes[suit])} for suit in axes}
    for n, _ in axes["mixed"]:
        for suit in ("shape", "colour"):
            if n not in idx[suit]:
                continue
            ax, ay = place("mixed", idx["mixed"][n])
            bx, by = place(suit, idx[suit][n])
            ux, uy = bx - ax, by - ay
            L = math.hypot(ux, uy)
            ax, ay = ax + ux / L * 150, ay + uy / L * 150
            bx, by = bx - ux / L * 150, by - uy / L * 150
            svg.append(f'<line x1="{ax:.1f}" y1="{ay:.1f}" x2="{bx:.1f}" y2="{by:.1f}" '
                       f'stroke="{LINE}" stroke-width="2.5" marker-end="url(#ah)"/>')

    # the two sequences sweeping up to the apex
    for suit, side in (("shape", -1), ("colour", 1)):
        sx, sy = place(suit, idx[suit]["straight"])
        svg.append(f'<path d="M{sx + side*140:.1f},{sy - 52:.1f} '
                   f'C{sx + side*560:.1f},{sy - 380:.1f} '
                   f'{CX + side*560:.1f},{APEX[1] - 30:.1f} '
                   f'{CX + side*110:.1f},{APEX[1] + 2:.1f}" fill="none" '
                   f'stroke="{GREY}" stroke-width="3" marker-end="url(#ah2)"/>')

    # nodes
    for suit in ("shape", "mixed", "colour"):
        for i, (n, (count, rank, example)) in enumerate(axes[suit]):
            x, y = place(suit, i)
            odds = total / count
            txt = f"1 in {odds:,.1f}" if odds < 100 else f"1 in {round(odds):,}"
            svg.append(glyph(x, y, GROUPS[n]))
            svg.append(text(x, y + 72, PREFIX[suit] + NAMES[n], 30, INK, "600",
                            "middle", SANS, 1.2))
            svg.append(text(x, y + 104, f"{txt}   ·   #{rank}", 23, GREY))
            svg.append(hand(x, y + 124, example, table))

    # the apex
    count, rank, example = cell[("straight", "perfect")]
    ax, ay = APEX
    svg.append(f'<circle cx="{ax}" cy="{ay}" r="104" fill="none" stroke="{INK}" '
               f'stroke-width="3.5"/>')
    svg.append(glyph(ax, ay, GROUPS["straight"]))
    svg.append(text(ax, ay + 148, "SINGULARITY", 36, INK, "600", "middle", SANS, 2))
    svg.append(text(ax, ay + 182, f"1 in {round(total/count):,}   ·   #{rank}", 23, GREY))
    svg.append(text(ax, ay + 212, "one colour and one shape — the whole suit", 22, LINE))
    svg.append(hand(ax, ay + 232, example, table))

    # axis feet
    for suit, label in (("shape", "SHAPE"), ("mixed", "ANY SUITS"), ("colour", "COLOUR")):
        x, y = place(suit, len(axes[suit]) - 1)
        svg.append(text(x, y + 300, label, 34, LINE, "600", "middle", SANS, 4))

    svg.append('</svg>')
    out = HERE / "out" / "universe-pyramid.svg"
    out.parent.mkdir(exist_ok=True)
    out.write_text("\n".join(svg))
    print(f"  {W}x{H}  {out.stat().st_size/1024:.0f} KB -> {out}")


if __name__ == "__main__":
    main()
