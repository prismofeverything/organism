#!/usr/bin/env python3
"""DISTINCTIONS in one card: six two-way distinctions, and then all 64 cards.

The six, and how a card is drawn from them (back to front):

  background   black | white       the card's field
  foreground   red   | blue        the figure
  composition  circle | bar        a circle in the middle, or a bar top to
                                   bottom; the bar is behind everything
  inversion    no | yes            yes swaps them: field in foreground colour,
                                   figure in background colour
  rays         no | yes            sixteen rays from the centre, fat there and
                                   tapering out, behind the eye and circle
  eye          no | yes            an eye-shaped field the circle sits in

The rays and the eye take the colours the card isn't using, and follow the
inversion in opposite directions: plain, the rays are the other of red/blue and
the eye the other of black/white; inverted, the rays are the other of
black/white and the eye the other of red/blue.  So across the deck each of the
four colours is a ray colour sixteen times, and an eye colour sixteen times.

The grid is a Karnaugh map: rows walk background/foreground/inversion and
columns walk composition/eye/rays, both in Gray code, so every card differs
from each of its neighbours (wrapping round the edges too) in exactly one
distinction.  Every one of the 64 lands exactly once; that is checked.
"""
import pathlib

from PIL import ImageFont

HERE = pathlib.Path(__file__).parent
INTER_R = "/usr/share/fonts/opentype/inter/Inter-Regular.otf"
GOTHIC_F = "/usr/share/fonts/opentype/urw-base35/URWGothic-Demi.otf"
INTER = "Inter, Helvetica, Arial, sans-serif"
DISPLAY = "URW Gothic, Century Gothic, Inter, sans-serif"
INK = "#1a1622"
GREY = "#7c7884"
EDGE = "#d8d6dc"

BW = {"black": "#16141a", "white": "#ffffff"}
RB = {"red": "#d42a20", "blue": "#1f4fc0"}
OTHER = {"black": "white", "white": "black", "red": "blue", "blue": "red"}
ASPECT = 1050 / 750                        # poker card, same as UNIVERSE

# card geometry, in card widths; the centre is (0.5, ASPECT / 2)
CIRCLE_R = 0.19
BAR_HALF = 0.14
EYE_TIP = 0.46                             # half the eye's width
EYE_CTRL = 0.52                            # quadratic control -> half-height 0.26
RAYS, RAY_BASE, RAY_OUT = 16, 0.075, 1.0     # base half-width at the centre
CORNER = 0.05

DIMS = [
    ("background", ["black", "white"]),
    ("foreground", ["red", "blue"]),
    ("composition", ["circle", "bar"]),
    ("eye", ["no eye", "eye"]),
    ("rays", ["no rays", "rays"]),
    ("inversion", ["plain", "inverted"]),
]
# the legend builds one card up: each row's pair differs only in its own
# distinction, on top of everything the rows above it added
LEGEND = {"composition": lambda on: {"comp": "bar" if on else "circle"},
          "eye": lambda on: {"eye": on},
          "rays": lambda on: {"eye": True, "rays": on},
          "inversion": lambda on: {"eye": True, "rays": True, "inv": on}}


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def text(x, y, s, size, fill=INK, weight="400", anchor="middle", family=INTER, ls=0):
    sp = f' letter-spacing="{ls}"' if ls else ""
    return (f'<text x="{x:.1f}" y="{y:.1f}" font-family="{family}" font-size="{size}" '
            f'font-weight="{weight}" fill="{fill}" text-anchor="{anchor}"{sp}>{esc(s)}</text>')


_clip = [0]


def card(x, y, w, bg="white", fg="red", comp="circle", inv=False, eye=False,
         rays=False):
    """One card, top-left at (x, y), w wide."""
    import math
    h = w * ASPECT
    cx, cy = x + w / 2, y + h / 2
    if not inv:
        field, figure = BW[bg], RB[fg]
        ray_c, eye_c = RB[OTHER[fg]], BW[OTHER[bg]]
    else:
        field, figure = RB[fg], BW[bg]
        ray_c, eye_c = BW[OTHER[bg]], RB[OTHER[fg]]
    _clip[0] += 1
    cid = f"c{_clip[0]}"
    r = CORNER * w
    out = [f'<clipPath id="{cid}"><rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" '
           f'height="{h:.1f}" rx="{r:.1f}"/></clipPath>',
           f'<g clip-path="url(#{cid})">',
           f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" '
           f'fill="{field}"/>']
    if comp == "bar":
        b = BAR_HALF * w
        out.append(f'<rect x="{cx - b:.1f}" y="{y:.1f}" width="{2 * b:.1f}" '
                   f'height="{h:.1f}" fill="{figure}"/>')
    if rays:                                   # each a thin triangle, base at the centre
        for i in range(RAYS):
            a = 2 * math.pi * i / RAYS - math.pi / 2
            ux, uy = math.cos(a), math.sin(a)
            bw, L = RAY_BASE * w, RAY_OUT * w
            pts = [(cx - uy * bw, cy + ux * bw), (cx + ux * L, cy + uy * L),
                   (cx + uy * bw, cy - ux * bw)]
            out.append('<polygon points="' + " ".join(f"{px:.1f},{py:.1f}" for px, py in pts)
                       + f'" fill="{ray_c}"/>')
    if eye:
        t, c = EYE_TIP * w, EYE_CTRL * w
        out.append(f'<path d="M{cx - t:.1f},{cy:.1f} Q{cx:.1f},{cy - c:.1f} '
                   f'{cx + t:.1f},{cy:.1f} Q{cx:.1f},{cy + c:.1f} {cx - t:.1f},{cy:.1f}Z" '
                   f'fill="{eye_c}"/>')
    if comp == "circle":
        out.append(f'<circle cx="{cx:.1f}" cy="{cy:.1f}" r="{CIRCLE_R * w:.1f}" '
                   f'fill="{figure}"/>')
    out.append('</g>')
    if field == BW["white"]:                # white field needs an edge to read
        out.append(f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" '
                   f'rx="{r:.1f}" fill="none" stroke="{EDGE}" stroke-width="{max(1.5, w / 90):.1f}"/>')
    return "\n".join(out)


def gray(i):
    return i ^ (i >> 1)


def grid_card(r, c):
    """Which card sits at (row, col): Gray code on both axes."""
    g, k = gray(r), gray(c)
    return dict(bg=["black", "white"][g >> 2 & 1], fg=["red", "blue"][g >> 1 & 1],
                inv=bool(g & 1), comp=["circle", "bar"][k >> 2 & 1],
                eye=bool(k >> 1 & 1), rays=bool(k & 1))


def main():
    lab_f = ImageFont.truetype(INTER_R, 46)
    head_f = ImageFont.truetype(GOTHIC_F, 150)

    MINI, SWATCH, PAIR, LABEL_GAP, ROW = 108, 54, 250, 78, 250
    mini_h = MINI * ASPECT
    GW, GGAP, N = 118, 10, 8
    gw = N * GW + (N - 1) * GGAP
    gh = N * GW * ASPECT + (N - 1) * GGAP

    label_w = max(lab_f.getlength(d + ":") for d, _ in DIMS)
    legend_w = label_w + LABEL_GAP + PAIR + MINI
    margin, COLGAP = 190, 200
    body_w = legend_w + COLGAP + gw
    W = round(max(body_w, head_f.getlength("DISTINCTIONS") + 14 * 12) + 2 * margin)
    CX = W / 2
    title_y = 300
    count_y = title_y + 230
    grid_y = count_y + 96
    H = round(grid_y + gh + 110 + margin)

    # legend on the left, its rows spread down the height of the grid
    left = CX - body_w / 2
    top, bot = grid_y + mini_h / 2, grid_y + gh - mini_h / 2 - 50
    row_y = [top + i * (bot - top) / (len(DIMS) - 1) for i in range(len(DIMS))]

    out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" '
           f'viewBox="0 0 {W} {H}">',
           f'<rect width="{W}" height="{H}" fill="#ffffff"/>',
           text(CX, title_y, "DISTINCTIONS", 150, INK, "400", "middle", DISPLAY, 14)]

    x_lab = left + label_w
    x0 = x_lab + LABEL_GAP + MINI / 2          # centre of the first of the pair
    for (name, values), y in zip(DIMS, row_y):
        out.append(text(x_lab, y + 16, name + ":", 46, GREY, "400", "end"))
        for m, v in enumerate(values):
            x = x0 + m * PAIR
            if name in ("background", "foreground"):
                fill = BW.get(v) or RB[v]
                edge = f' stroke="{EDGE}" stroke-width="3"' if v == "white" else ""
                out.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{SWATCH}" '
                           f'fill="{fill}"{edge}/>')
            else:
                out.append(card(x - MINI / 2, y - mini_h / 2, MINI,
                                **LEGEND[name](m == 1)))
            out.append(text(x, y + mini_h / 2 + 40, v, 32, GREY))

    gx0 = left + legend_w + COLGAP
    gcx = gx0 + gw / 2
    out.append(text(gcx, count_y, "64 cards", 58, GREY, "400", "middle", INTER, 6))
    seen = set()
    for r in range(N):
        for c in range(N):
            spec = grid_card(r, c)
            seen.add(tuple(sorted(spec.items())))
            out.append(card(gx0 + c * (GW + GGAP), grid_y + r * (GW * ASPECT + GGAP),
                            GW, **spec))
    assert len(seen) == 64, f"grid covers {len(seen)} of 64"
    out.append(text(gcx, grid_y + gh + 80,
                    "each card differs from its neighbours by one distinction",
                    34, GREY))

    out.append('</svg>')
    dst = HERE / "out" / "distinctions-key.svg"
    dst.parent.mkdir(exist_ok=True)
    dst.write_text("\n".join(out))
    print(f"  {W}x{H}  {dst.stat().st_size/1024:.0f} KB  grid covers all 64 -> {dst}")


if __name__ == "__main__":
    main()
