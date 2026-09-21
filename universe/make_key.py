#!/usr/bin/env python3
"""The deck in one card: three colors, four shapes, five numbers -- and then
all sixty, laid out so the lattice shows.

Six rows of ten.  Colors band the rows two at a time, numbers band the columns
two at a time, and the shape steps on by one with every colour and every
number, which sets the whole field shimmering diagonally.  Every one of the
3 x 4 x 5 combinations lands exactly once; that is checked, not hoped for.

Vector, using the same traced outlines and the same rosette geometry as the
printed cards.
"""
import json
import pathlib

from PIL import ImageFont

import deck
from make_pyramid import card, text

HERE = pathlib.Path(__file__).parent
INTER_R = "/usr/share/fonts/opentype/inter/Inter-Regular.otf"
GOTHIC_F = "/usr/share/fonts/opentype/urw-base35/URWGothic-Demi.otf"
INTER = "Inter, Helvetica, Arial, sans-serif"
DISPLAY = "URW Gothic, Century Gothic, Inter, sans-serif"
INK = "#1a1622"
GREY = "#7c7884"

SHAPE_ORDER = ["eye", "star", "pyramid", "helix"]
STEP, RADIUS, SWATCH = 172, 62, 48
LABEL_GAP, ROW = 78, 210
COLS, ROWS = 10, 6                 # six rows of ten
GW, GGAP = 61, 5                   # grid card width, and the gap between;
                                   # three fifths of full size, so the grid
                                   # reads as one field rather than sixty things


def grid_card(r, c):
    """Which card sits at (row, col).  Color bands the rows in twos, number
    bands the columns in twos, and the shape advances with both."""
    k, h = divmod(r, 2)            # color, and which half of its band
    n, j = divmod(c, 2)            # number, and which half of its band
    return k, (2 * h + j + k + n) % 4, n


def main():
    table = deck.ring_table()
    lab_f = ImageFont.truetype(INTER_R, 46)
    num_f = ImageFont.truetype(GOTHIC_F, 124)
    head_f = ImageFont.truetype(GOTHIC_F, 150)

    rows = [("three colors:", 3, SWATCH), ("four shapes:", 4, RADIUS),
            ("five numbers:", 5, num_f.getlength("5") / 2)]
    widths = [lab_f.getlength(l) + LABEL_GAP + (k - 1) * STEP + 2 * hf
              for l, k, hf in rows]

    gw = COLS * GW + (COLS - 1) * GGAP
    gh = ROWS * round(GW * 1050 / 750) + (ROWS - 1) * GGAP
    margin = 190
    W = round(max(max(widths), gw, head_f.getlength("UNIVERSE") + 14 * 8) + 2 * margin)
    CX = W / 2
    title_y = 300
    row_y = [title_y + 240 + i * ROW for i in range(3)]
    count_y = row_y[-1] + 250
    grid_y = count_y + 96
    H = round(grid_y + gh + margin)

    paths = json.loads((HERE / "shapes" / "paths.json").read_text())
    out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" '
           f'viewBox="0 0 {W} {H}">', '<defs>']
    for name, d in paths.items():
        out.append(f'<path id="sh-{name}" d="{d}" fill-rule="evenodd"/>')
    out.append('</defs>')
    out.append(f'<rect width="{W}" height="{H}" fill="#ffffff"/>')
    out.append(text(CX, title_y, "UNIVERSE", 150, INK, "400", "middle", DISPLAY, 14))

    for i, ((label, k, hf), lw, y) in enumerate(zip(rows, widths, row_y)):
        left = CX - lw / 2                              # whole line centred
        out.append(text(left + lab_f.getlength(label), y + 16, label, 46, GREY,
                        "400", "end"))
        x0 = left + lab_f.getlength(label) + LABEL_GAP + hf
        for m in range(k):
            x = x0 + m * STEP
            if i == 0:
                out.append(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{SWATCH}" '
                           f'fill="{deck.COLORS[deck.COLOR_ORDER[m]]}"/>')
            elif i == 1:
                out.append(f'<g transform="translate({x:.1f},{y:.1f}) '
                           f'scale({RADIUS})"><use href="#sh-{SHAPE_ORDER[m]}" '
                           f'fill="{INK}"/></g>')
            else:
                out.append(text(x, y + 42, str(m + 1), 124, INK, "400", "middle",
                                DISPLAY))

    out.append(text(CX, count_y, "60 cards", 58, GREY, "400", "middle", INTER, 6))

    seen = set()
    gx0, gh_card = CX - gw / 2, GW * 1050 / 750
    for r in range(ROWS):
        for c in range(COLS):
            col, shp, num = grid_card(r, c)
            seen.add((col, shp, num))
            out.append(card(gx0 + c * (GW + GGAP), grid_y + r * (gh_card + GGAP),
                            col, shp, num, table, GW))
    assert len(seen) == 60, f"grid covers {len(seen)} of 60"

    out.append('</svg>')
    dst = HERE / "out" / "universe-key.svg"
    dst.parent.mkdir(exist_ok=True)
    dst.write_text("\n".join(out))
    print(f"  {W}x{H}  {dst.stat().st_size/1024:.0f} KB  "
          f"grid covers all 60 -> {dst}")


if __name__ == "__main__":
    main()
