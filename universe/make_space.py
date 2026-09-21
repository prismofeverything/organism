#!/usr/bin/env python3
"""The hand space as one object.

The deck is a 3 x 4 x 5 block of cards.  A hand's suit grade is nothing more
than the smallest axis-aligned sub-block it fits inside: the whole block, a
colour slab, a shape slab, or one line of five.  That sub-block's cross-section
-- how many cards share a number, 12 or 4 or 3 or 1 -- is what decides both
which number patterns can exist in it and how rare they are.

So the four sub-blocks are the columns and rarity is the height.  Every colour
flush in the deck stands in one column, every shape flush in another, and the
number patterns are the links across.  The columns climb and shorten to the
right, ending in a single hand.

No perspective: a projected floor would add depth to screen height and wreck
the one comparison the picture is for.  The three dimensions are drawn as what
they are -- four solid blocks along the bottom.
"""
import json
import math
import pathlib

from PIL import Image, ImageDraw, ImageFont

HERE = pathlib.Path(__file__).parent
INTER = "/usr/share/fonts/opentype/inter/Inter-{}.otf"
GOTHIC = "/usr/share/fonts/opentype/urw-base35/URWGothic-Demi.otf"

W, H = 2550, 3300
FOOT, VS, LO = 2660, 400.0, 0.15        # baseline, pixels per decade, low end
NODE = 12

INK = (26, 22, 34)
GREY = (124, 120, 132)
LINE = (168, 164, 178)
FAINT = (203, 200, 211)
HAIR = (234, 232, 239)

# column, x position, block, cards per number
COLUMNS = [
    ("mixed",   420, (3, 4, 5), "ANY SUITS",            "the whole deck"),
    ("colour",  990, (1, 4, 5), "ALL ONE COLOUR",       "a colour slab"),
    ("shape",  1560, (3, 1, 5), "ALL ONE SHAPE",        "a shape slab"),
    ("perfect", 2090, (1, 1, 5), "ONE COLOUR + SHAPE",  "a single suit"),
]
XOF = {c[0]: c[1] for c in COLUMNS}
SHORT = {"one pair": "Pair", "two pair": "Two Pair", "three of a kind": "Three of a Kind",
         "full house": "Full House", "straight": "Straight",
         "four of a kind": "Four of a Kind", "five of a kind": "Five of a Kind"}
ORDER = ["one pair", "two pair", "three of a kind", "full house", "straight",
         "four of a kind", "five of a kind"]


def block(d, x, y, dims, unit=17):
    """An isometric solid standing on (x, y): the sub-deck as a shape."""
    a, b, c = dims
    u = (-unit * 1.30 * a, -unit * 0.72 * a)      # colours, back-left
    v = (unit * 1.30 * b, -unit * 0.72 * b)       # shapes, back-right
    w = (0.0, -unit * 1.10 * c)                   # numbers, up

    def p(i, j, k):
        return (x + i * u[0] + j * v[0] + k * w[0],
                y + i * u[1] + j * v[1] + k * w[1])

    d.polygon([p(0, 0, 0), p(1, 0, 0), p(1, 0, 1), p(0, 0, 1)],
              fill=(222, 219, 231), outline=LINE)          # left
    d.polygon([p(0, 0, 0), p(0, 1, 0), p(0, 1, 1), p(0, 0, 1)],
              fill=(238, 236, 244), outline=LINE)          # right
    d.polygon([p(0, 0, 1), p(1, 0, 1), p(1, 1, 1), p(0, 1, 1)],
              fill=(250, 249, 252), outline=LINE)          # top


def spread(items, gap):
    ys = [y for _, y in items]
    for i in range(1, len(ys)):
        ys[i] = max(ys[i], ys[i - 1] + gap)
    return list(zip([n for n, _ in items], ys))


def main():
    data = json.loads((HERE / "hands.json").read_text())
    total = data["total"]
    cell = {(r["numbers"], r["suit"]): (r["count"], i)
            for i, r in enumerate(data["rows"], start=1)}
    dec = {k: math.log10(total / v[0]) for k, v in cell.items()}

    def y_of(k):
        return FOOT - (dec[k] - LO) * VS

    img = Image.new("RGB", (W, H), "white")
    d = ImageDraw.Draw(img)
    f_title = ImageFont.truetype(GOTHIC, 56)
    f_sub = ImageFont.truetype(INTER.format("Regular"), 27)
    f_name = ImageFont.truetype(INTER.format("SemiBold"), 25)
    f_odds = ImageFont.truetype(INTER.format("Regular"), 23)
    f_head = ImageFont.truetype(INTER.format("Medium"), 22)
    f_small = ImageFont.truetype(INTER.format("Regular"), 21)
    f_rank = ImageFont.truetype(GOTHIC, 17)

    d.text((130, 112), "UNIVERSE · the hand space", font=f_title, fill=INK, anchor="ls")
    d.text((130, 154), "the deck is a 3×4×5 block of cards, and a hand's suit grade is "
                       "just the smallest sub-block it fits inside.", font=f_sub, fill=GREY)
    d.text((130, 192), "how thin that block is — 12, 4, 3 or 1 cards to a number — "
                       "decides both what can live there and how rare it is.",
           font=f_sub, fill=GREY)

    d.text((176, 372), "SEVEN PATTERNS, FOUR BLOCKS", font=f_head, fill=INK)
    for k, t in enumerate([
        "Each column is the same deck seen through a tighter constraint.  Going",
        "right the block gets thinner, so fewer number patterns fit — and every one",
        "that still does is rarer than its twin to the left.  That is the whole",
        "ranking: nineteen hands, but only seven things to learn.",
    ]):
        d.text((176, 416 + k * 32), t, font=f_small, fill=GREY)

    # rarity rules, read straight across every column
    for e in range(0, 6):
        gy = FOOT - (e - LO) * VS
        d.line((250, gy, W - 130, gy), fill=HAIR, width=2)
        d.text((238, gy), f"1 in {10**e:,}", font=f_small, fill=FAINT, anchor="rm")

    # links: one number pattern, wherever it can live
    for n in ORDER:
        chain = [c for c, *_ in COLUMNS if (n, c) in cell]
        for a, b in zip(chain, chain[1:]):
            d.line((XOF[a], y_of((n, a)), XOF[b], y_of((n, b))), fill=FAINT, width=3)

    for suit, x, dims, head, sub in COLUMNS:
        present = [n for n in ORDER if (n, suit) in cell]
        top = min(y_of((n, suit)) for n in present)
        d.line((x, FOOT - 40, x, top - 46), fill=LINE, width=3)

        present.sort(key=lambda n: y_of((n, suit)))
        for n, ly in spread([(n, y_of((n, suit))) for n in present], 62):
            ny = y_of((n, suit))
            count, rank = cell[(n, suit)]
            odds = total / count
            txt = f"1 in {odds:,.1f}" if odds < 100 else f"1 in {round(odds):,}"
            tx = x + 34
            d.text((tx, ly - 4), SHORT[n], font=f_name, fill=INK, anchor="ls")
            d.text((tx, ly + 26), txt, font=f_odds, fill=GREY, anchor="ls")
            if abs(ly - ny) > 6:
                d.line((tx - 8, ly - 4, x + NODE + 4, ny), fill=FAINT, width=2)
            d.ellipse((x - NODE, ny - NODE, x + NODE, ny + NODE), fill=INK)
            d.text((x, ny + 1), str(rank), font=f_rank, fill=(255, 255, 255), anchor="mm")

        # the sub-block itself, standing at the foot of its column
        block(d, x, FOOT + 210, dims)
        d.text((x, FOOT + 268), head, font=f_head, fill=INK, anchor="ms")
        a, b, c = dims
        d.text((x, FOOT + 300), f"{sub} · {a}×{b}×{c} · {a*b*c} cards",
               font=f_small, fill=GREY, anchor="ms")
        d.text((x, FOOT + 330), f"{a*b} to a number", font=f_small, fill=GREY, anchor="ms")
        d.text((x, FOOT + 362), f"so {len(present)} hand{'s' if len(present) > 1 else ''} fit",
               font=f_small, fill=LINE, anchor="ms")

    notes = [
        "Every colour flush in the deck stands in the second column, every shape flush "
        "in the third.  The links across are the number patterns — each one drawn "
        "wherever it can live.",
        "A column stops where its block runs out of thickness.  A colour slab is four "
        "shapes thick, so nothing in it can beat four of a kind; a shape slab is three "
        "colours thick, so it stops at a full house;",
        "a single suit is one card thick, so the only hand in it is the straight.  That "
        "is why the rarest hand in the deck is the only one in its column.",
        "Colour and shape are incomparable as constraints — neither implies the other "
        "— but as blocks they are simply 4 thick and 3 thick, which is why they still "
        "fall in an order.",
        "Nothing here is chosen: 7 number patterns across 4 sub-blocks, less the 9 that "
        "are too thin to hold, is 19.",
    ]
    ny = 3090
    d.line((130, ny - 40, W - 130, ny - 40), fill=FAINT, width=2)
    for k, t in enumerate(notes):
        d.text((130, ny + k * 32), t, font=f_small, fill=GREY)

    out = HERE / "out" / "universe-hand-space.png"
    img.save(out)
    img.save(HERE / "out" / "universe-hand-space.pdf", "PDF", resolution=300.0)
    print(f"  {img.size[0]}x{img.size[1]} -> {out}")


if __name__ == "__main__":
    main()
