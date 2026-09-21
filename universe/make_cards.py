#!/usr/bin/env python3
"""Build the UNIVERSE deck: 60 card faces, a back, print sheets and proofs.

  python make_cards.py all                 everything, MPC preset
  python make_cards.py faces --preset tgc  just the faces, Game Crafter size
  python make_cards.py sheets --paper a4   print-at-home imposition
  python make_cards.py proof               contact sheets, color and grey
"""
import argparse
import csv
import pathlib

import numpy as np
from PIL import Image, ImageDraw, ImageFont

import deck

HERE = pathlib.Path(__file__).parent
OUT = HERE / "out"
PROOF = HERE / "proof"
UI = "/usr/share/fonts/opentype/inter/Inter-Regular.otf"

PAPER = {  # width, height in inches
    "letter": (8.5, 11.0),
    "a4": (8.268, 11.693),
}


def tag(color, shape, number):
    return f"{color}-{shape}-{number}"


# ----------------------------------------------------------------- the cards

def build_faces(preset="mpc"):
    spec = deck.PRESETS[preset]()
    dst = OUT / preset / "faces"
    dst.mkdir(parents=True, exist_ok=True)
    table = deck.ring_table()
    rows = []
    for i, (c, s, n) in enumerate(deck.deck(), start=1):
        img = deck.render_face(c, s, n, spec, table)
        name = f"{i:02d}_{tag(c, s, n)}.png"
        img.save(dst / name)
        rows.append({"n": i, "file": name, "color": c, "shape": s, "number": n})
    with open(OUT / preset / "manifest.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=["n", "file", "color", "shape", "number"])
        w.writeheader()
        w.writerows(rows)
    print(f"  {len(rows)} faces -> {dst}  ({spec})")
    return spec


def build_back(preset="mpc"):
    spec = deck.PRESETS[preset]()
    (OUT / preset).mkdir(parents=True, exist_ok=True)
    deck.render_back(spec).save(OUT / preset / "back.png")
    print(f"  back -> {OUT / preset / 'back.png'}")


# ------------------------------------------------------------- print at home

def crop_marks(d, xs, ys, x0, y0, x1, y1, length=28, color=(120, 120, 120)):
    for x in xs:
        d.line((x, y0 - length, x, y0 - 4), fill=color, width=2)
        d.line((x, y1 + 4, x, y1 + length), fill=color, width=2)
    for y in ys:
        d.line((x0 - length, y, x0 - 4, y), fill=color, width=2)
        d.line((x1 + 4, y, x1 + length, y), fill=color, width=2)


def build_sheets(paper="letter", dpi=300, cols=3, rows=3):
    """Cards butted edge to edge so one cut serves two cards, with crop marks
    out in the margins."""
    spec = deck.PRESETS["cut"]()
    table = deck.ring_table()
    pw, ph = (round(v * dpi) for v in PAPER[paper])
    bw, bh = cols * spec.cut_w, rows * spec.cut_h
    if bw > pw or bh > ph:
        raise SystemExit(f"{cols}x{rows} cards ({bw}x{bh}) will not fit {paper}")
    x0, y0 = (pw - bw) // 2, (ph - bh) // 2
    dst = OUT / "print-at-home"
    dst.mkdir(parents=True, exist_ok=True)

    cards = deck.deck()
    pages, page, d, placed = [], None, None, 0
    small = ImageFont.truetype(UI, 22)
    for idx, (c, s, n) in enumerate(cards):
        slot = idx % (cols * rows)
        if slot == 0:
            page = Image.new("RGB", (pw, ph), "white")
            d = ImageDraw.Draw(page)
            pages.append(page)
        r, col = divmod(slot, cols)
        page.paste(deck.render_face(c, s, n, spec, table),
                   (x0 + col * spec.cut_w, y0 + r * spec.cut_h))
        placed += 1
        if slot == cols * rows - 1 or idx == len(cards) - 1:
            crop_marks(d,
                       [x0 + i * spec.cut_w for i in range(cols + 1)],
                       [y0 + j * spec.cut_h for j in range(rows + 1)],
                       x0, y0, x0 + bw, y0 + bh)
            d.text((x0, y0 + bh + 44),
                   f"UNIVERSE  ·  page {len(pages)}  ·  "
                   f"{paper} @ {dpi}dpi  ·  print at 100%, do not scale to fit",
                   font=small, fill=(140, 140, 140))
            placed = 0
    # one sheet of backs, repeated to match
    back = Image.new("RGB", (pw, ph), "white")
    bd = ImageDraw.Draw(back)
    b = deck.render_back(spec)
    for r in range(rows):
        for col in range(cols):
            back.paste(b, (x0 + col * spec.cut_w, y0 + r * spec.cut_h))
    crop_marks(bd, [x0 + i * spec.cut_w for i in range(cols + 1)],
               [y0 + j * spec.cut_h for j in range(rows + 1)], x0, y0, x0 + bw, y0 + bh)
    backs = [back] * len(pages)
    duplex = [p for pair in zip(pages, backs) for p in pair]

    for i, p in enumerate(pages, start=1):
        p.save(dst / f"{paper}-front-{i:02d}.png")
    back.save(dst / f"{paper}-back.png")
    for stem, seq in (("fronts", pages), ("backs", backs), ("duplex", duplex)):
        seq[0].save(dst / f"universe-{paper}-{stem}.pdf", "PDF", resolution=dpi,
                    save_all=True, append_images=seq[1:])
    print(f"  {len(pages)} front pages ({cols}x{rows}) + backs + duplex -> {dst}")


# ------------------------------------------------------------------- proofing

def build_proof():
    spec = deck.PRESETS["cut"]()
    table = deck.ring_table()
    PROOF.mkdir(exist_ok=True)
    sc = 0.30
    cw, ch = round(spec.cut_w * sc), round(spec.cut_h * sc)
    gap, pad, head = 9, 24, 54
    cols, rows = 5, 12                       # number across, color x shape down
    W = pad * 2 + cols * cw + (cols - 1) * gap
    H = head + pad * 2 + rows * ch + (rows - 1) * gap
    sheet = Image.new("RGB", (W, H), (245, 245, 247))
    d = ImageDraw.Draw(sheet)
    d.text((pad, 20), "UNIVERSE  ·  3 colors × 4 shapes × 5 numbers = 60 cards",
           font=ImageFont.truetype(UI, 26), fill=(40, 40, 46))
    for i, (c, s, n) in enumerate(deck.deck()):
        r, col = divmod(i, cols)
        sheet.paste(deck.render_face(c, s, n, spec, table).resize((cw, ch), Image.LANCZOS),
                    (pad + col * (cw + gap), head + pad + r * (ch + gap)))
    sheet.save(PROOF / "contact.png")
    sheet.convert("L").save(PROOF / "contact-grey.png")
    print(f"  contact sheets -> {PROOF}/contact.png (+ -grey)")


def build_palette_proof():
    """Show the lightness staircase that carries the color axis."""
    from PIL import ImageFont
    f = ImageFont.truetype(UI, 24)
    fb = ImageFont.truetype(UI, 30)
    W, H, sw = 980, 300, 300
    img = Image.new("RGB", (W, H), "white")
    d = ImageDraw.Draw(img)

    def lin(c):
        c = np.asarray(c, float) / 255
        return np.where(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055) ** 2.4)

    for i, (name, hx) in enumerate(deck.COLORS.items()):
        rgb = deck.hex_rgb(hx)
        y = lin(rgb) @ np.array([0.2126, 0.7152, 0.0722])
        g = int(round((y * 12.92 if y <= 0.0031308 else 1.055 * y ** (1 / 2.4) - 0.055) * 255))
        L = 116 * (np.cbrt(y) if y > (6 / 29) ** 3 else y / (3 * (6 / 29) ** 2) + 4 / 29) - 16
        x = 20 + i * (sw + 15)
        d.rectangle((x, 76, x + sw, 196), fill=rgb)
        d.rectangle((x, 196, x + sw, 252), fill=(g, g, g))
        d.text((x, 40), f"{name}  {hx}", font=fb, fill=(30, 30, 36))
        d.text((x + 8, 262), f"L* {L:4.1f}   as grey {g}", font=f, fill=(90, 90, 96))
    d.text((20, 8), "the color axis is a lightness staircase — lower band is the "
                    "same swatch desaturated", font=f, fill=(140, 140, 146))
    img.save(PROOF / "palette.png")
    print(f"  palette -> {PROOF}/palette.png")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("what", nargs="?", default="all",
                    choices=["all", "faces", "back", "sheets", "proof", "palette"])
    ap.add_argument("--preset", default="mpc", choices=list(deck.PRESETS))
    ap.add_argument("--paper", default="letter", choices=list(PAPER))
    a = ap.parse_args()
    if a.what in ("all", "faces"):
        build_faces(a.preset)
    if a.what in ("all", "back"):
        build_back(a.preset)
    if a.what in ("all", "sheets"):
        build_sheets(a.paper)
    if a.what in ("all", "proof"):
        build_proof()
    if a.what in ("all", "palette"):
        build_palette_proof()


if __name__ == "__main__":
    main()
