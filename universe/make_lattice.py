#!/usr/bin/env python3
"""The 19 hands are not 19 separate things.

They are seven number patterns crossed with four suit grades, nine of the
twenty-eight cells being impossible.  And the suit grade is itself two
independent questions -- all one color?  all one shape? -- which makes it a
diamond rather than a ladder, because neither implies the other.

This draws that lattice with rarity along the horizontal axis, so position is
the ranking: anything further right beats anything further left, whatever row
it sits in.  Each row is one family, branching up into its color flush and
down into its shape flush.
"""
import json
import math
import pathlib

from PIL import Image, ImageDraw, ImageFont

HERE = pathlib.Path(__file__).parent
INTER = "/usr/share/fonts/opentype/inter/Inter-{}.otf"
GOTHIC = "/usr/share/fonts/opentype/urw-base35/URWGothic-Demi.otf"

# strongest base at the top, so the diagram reads the way the ranking does;
# the left edge then climbs as a staircase towards the commoner hands
ROWS = [
    ("five of a kind", "Five of a Kind"),
    ("four of a kind", "Four of a Kind"),
    ("straight", "Straight"),
    ("full house", "Full House"),
    ("three of a kind", "Three of a Kind"),
    ("two pair", "Two Pair"),
    ("one pair", "Pair"),
]
GRADE = {"mixed": "", "color": "ONE COLOR", "shape": "ONE SHAPE", "perfect": "COLOR + SHAPE"}

# impossible upgrades, and why
BLOCKED = {
    ("four of a kind", "shape"): "no shape flush: a shape holds only 3 colors",
    ("five of a kind", "color"): "no flush at all: a color holds only 4 shapes,"
                                  " a shape only 3 colors",
    ("full house", "perfect"): None, ("three of a kind", "perfect"): None,
    ("two pair", "perfect"): None, ("one pair", "perfect"): None,
    ("four of a kind", "perfect"): None, ("five of a kind", "perfect"): None,
    ("five of a kind", "shape"): None,
}

W, H = 3300, 2550
LEFT, RIGHT = 640, 3150
LO, HI = 0.12, 5.88                      # decades of "one hand in N"
TOP, PITCH = 520, 258
BRANCH = 40                              # vertical offset of the two branches
R = 15                                   # node radius

INK = (26, 22, 34)
GREY = (128, 124, 136)
LINE = (168, 164, 178)
FAINT = (198, 195, 206)
HAIR = (231, 229, 236)


def main():
    data = json.loads((HERE / "hands.json").read_text())
    total = data["total"]
    cell = {}
    for rank, r in enumerate(data["rows"], start=1):
        cell[(r["numbers"], r["suit"])] = (r["count"], rank)

    def x_of(count):
        return LEFT + (math.log10(total / count) - LO) / (HI - LO) * (RIGHT - LEFT)

    img = Image.new("RGB", (W, H), "white")
    d = ImageDraw.Draw(img)
    f_title = ImageFont.truetype(GOTHIC, 58)
    f_sub = ImageFont.truetype(INTER.format("Regular"), 28)
    f_row = ImageFont.truetype(INTER.format("SemiBold"), 32)
    f_odds = ImageFont.truetype(INTER.format("Medium"), 26)
    f_grade = ImageFont.truetype(INTER.format("Medium"), 19)
    f_mult = ImageFont.truetype(INTER.format("Regular"), 21)
    f_rank = ImageFont.truetype(GOTHIC, 20)
    f_note = ImageFont.truetype(INTER.format("Regular"), 23)

    d.text((130, 108), "UNIVERSE · how the hands fit together", font=f_title,
           fill=INK, anchor="ls")
    d.text((130, 152), "nineteen hands, but only seven patterns — each crossed with "
                       "up to three grades of flush.", font=f_sub, fill=GREY)
    d.text((130, 192), "rarity runs left to right, so anything further right beats "
                       "anything further left, whatever row it is in.",
           font=f_sub, fill=GREY)

    # key: the branch every row makes, drawn once in the empty top right
    kx, ky = 2430, 452
    d.text((kx - 10, ky - 146), "EVERY ROW BRANCHES LIKE THIS", font=f_grade, fill=GREY)
    pts = {"m": (kx, ky), "c": (kx + 210, ky - 52), "s": (kx + 210, ky + 52),
           "p": (kx + 430, ky)}
    for a, b in (("m", "c"), ("m", "s"), ("c", "p"), ("s", "p")):
        d.line(pts[a] + pts[b], fill=LINE, width=4)
    for k, (px, py) in pts.items():
        d.ellipse((px - 11, py - 11, px + 11, py + 11),
                  fill=INK if k != "p" else "white", outline=INK, width=3)
    d.text((pts["m"][0] - 22, pts["m"][1]), "the hand", font=f_mult, fill=GREY, anchor="rm")
    d.text((pts["c"][0], pts["c"][1] - 26), "ONE COLOR", font=f_grade, fill=GREY, anchor="ms")
    d.text((pts["s"][0], pts["s"][1] + 40), "ONE SHAPE", font=f_grade, fill=GREY, anchor="ms")
    d.text((pts["p"][0] + 22, pts["p"][1]), "both — straight only",
           font=f_mult, fill=GREY, anchor="lm")

    # decade grid
    bottom = TOP + len(ROWS) * PITCH - 60
    for e in range(0, 6):
        gx = LEFT + (e - LO) / (HI - LO) * (RIGHT - LEFT)
        d.line((gx, 250, gx, bottom + 44), fill=HAIR, width=2)
        d.text((gx, bottom + 78), f"1 in {10**e:,}", font=f_grade, fill=FAINT, anchor="ms")

    for i, (num, label) in enumerate(ROWS):
        y = TOP + i * PITCH
        d.text((130, y), label, font=f_row, fill=INK, anchor="lm")

        pos = {}
        for suit, dy in (("mixed", 0), ("color", -BRANCH), ("shape", BRANCH), ("perfect", 0)):
            if (num, suit) in cell:
                pos[suit] = (x_of(cell[(num, suit)][0]), y + dy)

        # edges, labelled with what the upgrade costs
        def edge(a, b):
            if a not in pos or b not in pos:
                return
            (x1, y1), (x2, y2) = pos[a], pos[b]
            d.line((x1, y1, x2, y2), fill=FAINT, width=4)
            mult = cell[(num, a)][0] / cell[(num, b)][0]
            d.text(((x1 + x2) / 2, (y1 + y2) / 2 - 16),
                   f"÷{mult:,.0f}", font=f_mult, fill=GREY, anchor="ms")

        edge("mixed", "color")
        edge("mixed", "shape")
        edge("color", "perfect")
        edge("shape", "perfect")

        for suit, (px, py) in pos.items():
            count, rank = cell[(num, suit)]
            d.ellipse((px - R, py - R, px + R, py + R), fill=INK)
            d.text((px, py + 1), str(rank), font=f_rank, fill=(255, 255, 255), anchor="mm")
            odds = total / count
            txt = f"1 in {odds:,.1f}" if odds < 100 else f"1 in {round(odds):,}"
            d.text((px, py - R - 12), txt, font=f_odds, fill=INK, anchor="ms")
            if GRADE[suit]:
                d.text((px, py + R + 30), GRADE[suit], font=f_grade, fill=GREY, anchor="ms")

        for (n, su), why in BLOCKED.items():
            if n == num and why:
                d.text((RIGHT + 24, y + BRANCH + 4), why, font=f_mult, fill=FAINT,
                       anchor="rs")

    # key
    ky = bottom + 150
    d.line((130, ky - 46, W - 130, ky - 46), fill=FAINT, width=2)
    kx = 150
    d.text((kx, ky), "READING IT", font=f_grade, fill=GREY)
    notes = [
        "Seven number patterns, the ones you already know, crossed with four suit grades.  "
        "Nine of the twenty-eight cells are impossible, which leaves nineteen.",
        "\u00f7 marks what an upgrade costs.  The numbers in the dots are the ranks from "
        "the hand chart.  Color and shape are independent, neither implying the other, "
        "so the suit axis is a diamond rather than a ladder.",
        "The straight's diamond closes exactly: 80 \u00d7 255 and 255 \u00d7 80 both land "
        "on 20,400, so it costs the same whichever constraint you add first.",
        "Across rows they compound instead \u2014 a flush costs a pair 98\u00d7 and four of "
        "a kind 494\u00d7.  The better your numbers, the dearer the flush on top.",
    ]
    for k, t in enumerate(notes):
        d.text((kx + 190, ky + k * 36), t, font=f_note, fill=GREY)

    out = HERE / "out" / "universe-hand-lattice.png"
    img.save(out)
    img.save(HERE / "out" / "universe-hand-lattice.pdf", "PDF", resolution=300.0)
    print(f"  {img.size[0]}x{img.size[1]} -> {out}")


if __name__ == "__main__":
    main()
