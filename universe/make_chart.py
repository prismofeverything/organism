#!/usr/bin/env python3
"""Render the hand-ranking chart: name and odds on the left, a real example
hand on the right, rarest first.  Counts come from hands.json, which is an
exhaustive enumeration (see hands.py)."""
import json
import pathlib

from PIL import Image, ImageDraw, ImageFont

import deck

HERE = pathlib.Path(__file__).parent
INTER = "/usr/share/fonts/opentype/inter/Inter-{}.otf"
GOTHIC = "/usr/share/fonts/opentype/urw-base35/URWGothic-Demi.otf"

# (number pattern, suit pattern) -> name, what it is
NAMES = {
    ("straight", "perfect"):        ("Singularity", "all five of one colour and shape — a whole suit"),
    ("four of a kind", "colour"):   ("Colour Four of a Kind", "four of one number in one colour, so all four shapes"),
    ("full house", "shape"):        ("Shape Full House", "three and two, all one shape"),
    ("straight", "shape"):          ("Shape Straight", "all five numbers, all one shape"),
    ("three of a kind", "shape"):   ("Shape Three of a Kind", "three of one number, all one shape"),
    ("full house", "colour"):       ("Colour Full House", "three and two, all one colour"),
    ("straight", "colour"):         ("Colour Straight", "all five numbers, all one colour"),
    ("two pair", "shape"):          ("Shape Two Pair", "two pairs, all one shape"),
    ("five of a kind", "mixed"):    ("Five of a Kind", "five of one number"),
    ("three of a kind", "colour"):  ("Colour Three of a Kind", "three of one number, all one colour"),
    ("one pair", "shape"):          ("Shape Pair", "a pair, all one shape"),
    ("two pair", "colour"):         ("Colour Two Pair", "two pairs, all one colour"),
    ("one pair", "colour"):         ("Colour Pair", "a pair, all one colour"),
    ("four of a kind", "mixed"):    ("Four of a Kind", "four of one number"),
    ("straight", "mixed"):          ("Straight", "all five numbers"),
    ("full house", "mixed"):        ("Full House", "three of one number, two of another"),
    ("three of a kind", "mixed"):   ("Three of a Kind", "three of one number"),
    ("two pair", "mixed"):          ("Two Pair", "two pairs"),
    ("one pair", "mixed"):          ("Pair", "two of one number"),
}

FOOTNOTES = [
    "There is no high-card hand.  Only five numbers exist, so any hand without a repeat"
    " is already the whole run 1–2–3–4–5.",
    "A colour-and-shape flush is always a straight, because a suit holds exactly five"
    " cards and taking five takes them all.",
    "Five of a kind can never be a flush — a colour holds only four shapes and a shape"
    " only three colours.  For the same reason there is no shape four of a kind.",
    "Every flush of either kind beats four of a kind: splitting the suit leaves only 20"
    " cards in a colour and 15 in a shape.  Ranks 2 and 3 are exactly tied, at 240 hands each.",
]

CW = 92                      # example card width
CH = round(CW * 1050 / 750)
GAP = 8
MARGIN = 110
W = 2550


def money(n):
    return f"{n:,}"


def odds(total, count):
    """One hand in how many.  Keep a decimal on the common hands, where
    rounding 2.4 to 2 would overstate them by a fifth."""
    v = total / count
    return f"1 in {v:,.1f}" if v < 100 else f"1 in {round(v):,}"


def main():
    data = json.loads((HERE / "hands.json").read_text())
    total = data["total"]
    rows = data["rows"]

    f_title = ImageFont.truetype(GOTHIC, 62)
    f_sub = ImageFont.truetype(INTER.format("Regular"), 27)
    f_name = ImageFont.truetype(INTER.format("SemiBold"), 33)
    f_desc = ImageFont.truetype(INTER.format("Regular"), 23)
    f_odds = ImageFont.truetype(INTER.format("Medium"), 30)
    f_pct = ImageFont.truetype(INTER.format("Regular"), 22)
    f_rank = ImageFont.truetype(GOTHIC, 30)
    f_note = ImageFont.truetype(INTER.format("Regular"), 22)

    pitch = CH + 16
    top = 292
    H = top + len(rows) * pitch + 56 + len(FOOTNOTES) * 34 + 52

    img = Image.new("RGB", (W, H), "white")
    d = ImageDraw.Draw(img)
    ink = (26, 22, 34)
    grey = (122, 118, 130)
    rule = (226, 224, 230)

    d.text((MARGIN, 84), "UNIVERSE · hands", font=f_title, fill=ink, anchor="ls")
    d.text((MARGIN, 140),
           f"every pattern five cards can make, rarest first.  "
           f"{money(total)} possible hands from 60 cards.",
           font=f_sub, fill=grey)
    d.text((MARGIN, 182),
           "the suit is split in two, so there are three kinds of flush: one colour, "
           "one shape, or both at once.",
           font=f_sub, fill=grey)
    d.line((MARGIN, 248, W - MARGIN, 248), fill=ink, width=3)

    cards_x = W - MARGIN - (5 * CW + 4 * GAP)
    odds_x = cards_x - 70
    spec = deck.PRESETS["cut"]()
    table = deck.ring_table()
    cache = {}

    f_lab = ImageFont.truetype(INTER.format("Medium"), 20)
    d.text((MARGIN + 74, 272), "HAND", font=f_lab, fill=(180, 177, 188))
    d.text((odds_x, 272), "ODDS", font=f_lab, fill=(180, 177, 188), anchor="rs")
    d.text((cards_x, 272), "EXAMPLE", font=f_lab, fill=(180, 177, 188))

    for i, r in enumerate(rows):
        y = top + i * pitch
        name, desc = NAMES[(r["numbers"], r["suit"])]
        d.text((MARGIN + 44, y + CH / 2 - 2), str(i + 1), font=f_rank, fill=rule, anchor="rm")
        d.text((MARGIN + 74, y + CH / 2 - 14), name, font=f_name, fill=ink, anchor="lm")
        d.text((MARGIN + 74, y + CH / 2 + 22), desc, font=f_desc, fill=grey, anchor="lm")
        d.text((odds_x, y + CH / 2 - 13), odds(total, r["count"]),
               font=f_odds, fill=ink, anchor="rm")
        pct = 100 * r["count"] / total
        d.text((odds_x, y + CH / 2 + 20),
               f"{pct:.4f}%" if pct < 1 else f"{pct:.2f}%",
               font=f_pct, fill=grey, anchor="rm")

        for j, (c, s, n) in enumerate(sorted(r["example"], key=lambda t: (t[2], t[0], t[1]))):
            key = (c, s, n)
            if key not in cache:
                face = deck.render_face(deck.COLOUR_ORDER[c], deck.SHAPES[s], n + 1,
                                        spec, table)
                cache[key] = face.resize((CW, CH), Image.LANCZOS)
            x = cards_x + j * (CW + GAP)
            img.paste(cache[key], (x, y))
            d.rectangle((x, y, x + CW - 1, y + CH - 1), outline=rule)
        if i < len(rows) - 1:
            d.line((MARGIN, y + pitch - 8, W - MARGIN, y + pitch - 8), fill=(242, 241, 245))

    y = top + len(rows) * pitch + 34
    d.line((MARGIN, y, W - MARGIN, y), fill=rule, width=2)
    for k, note in enumerate(FOOTNOTES):
        d.text((MARGIN, y + 26 + k * 34), "·  " + note, font=f_note, fill=grey)

    out = HERE / "out" / "universe-hands.png"
    out.parent.mkdir(exist_ok=True)
    img.save(out)
    img.save(HERE / "out" / "universe-hands.pdf", "PDF", resolution=300.0)
    print(f"  {img.size[0]}x{img.size[1]}  ({img.size[1]/300:.2f}in tall at 300dpi) -> {out}")


if __name__ == "__main__":
    main()
