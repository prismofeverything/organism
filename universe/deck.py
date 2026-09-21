#!/usr/bin/env python3
"""UNIVERSE -- a 60 card deck on three axes: 3 colours x 4 shapes x 5 numbers.

Every card carries its number and shape in the top-left and bottom-right
corners (the second copy rotated 180 degrees), and in the middle that many
copies of the shape arranged radially, each turned by its own angle so the
whole rosette has n-fold rotational symmetry.
"""
import json
import math
import pathlib

import numpy as np
from PIL import Image, ImageDraw, ImageFont
from scipy import ndimage

HERE = pathlib.Path(__file__).parent

# ---------------------------------------------------------------- vocabulary

SHAPES = ["eye", "helix", "pyramid", "star"]
NUMBERS = [1, 2, 3, 4, 5]

# The colour axis runs purple -> green -> yellow, darkest to lightest, and it
# is that lightness order that carries it: read as greys the three are 66, 119
# and 182 against paper at 255, so they stay apart for a colourblind player
# with no hue to go on.  Within that each is pushed as far as it will go --
# the purple to the most saturated violet that still sits clearly darkest
# (chroma peaks at middling lightness, so being the dark one costs it
# something), the green to its chroma peak at hue 145, where it is a solid
# forest green rather than the teal an evenly spaced hue triad would force,
# and the yellow left exactly where it was.  Chroma is held at 88 per cent of
# the sRGB gamut edge so CMYK has somewhere to land.
COLOURS = {
    "purple": "#651694",   # L* 28, C 75, h 316
    "green":  "#26883F",   # L* 50, C 54, h 145
    "yellow": "#E6AD24",   # L* 74, C 72, h  82
}

# Index order for the colour axis, shared with hands.py so a card index means
# the same thing in the enumeration and on the page.
COLOUR_ORDER = list(COLOURS)

# Each shape's own resting orientation, applied before anything else.
BASE_ROTATION = {"eye": 0.0, "helix": 0.0, "pyramid": 0.0, "star": 0.0}

FONT = "/usr/share/fonts/opentype/urw-base35/URWGothic-Demi.otf"

# Neighbouring copies must keep this much daylight between them, as a fraction
# of a copy's radius.  Packing to mere non-overlap made the four- and five-star
# rosettes impossible to count at a glance, which is the one thing the middle
# of the card has to do.
CLEARANCE = 0.075

# ------------------------------------------------------------------ geometry

class Spec:
    """Physical card. All the layout numbers below are in *cut* pixels."""

    def __init__(self, dpi=300, cut=(2.5, 3.5), bleed=0.12, name="mpc"):
        self.name, self.dpi, self.bleed_in = name, dpi, bleed
        self.cut_w = round(cut[0] * dpi)
        self.cut_h = round(cut[1] * dpi)
        self.w = round((cut[0] + 2 * bleed) * dpi)
        self.h = round((cut[1] + 2 * bleed) * dpi)
        self.ox = (self.w - self.cut_w) / 2.0     # cut-space origin, in card px
        self.oy = (self.h - self.cut_h) / 2.0

    def at(self, x, y):
        return (self.ox + x, self.oy + y)

    def __repr__(self):
        return (f"<{self.name} {self.w}x{self.h}px  cut {self.cut_w}x{self.cut_h}"
                f"  bleed {self.bleed_in}\" @ {self.dpi}dpi>")


PRESETS = {
    # MakePlayingCards / BoardGamesMaker poker: 822x1122, 36px bleed each side
    "mpc": lambda: Spec(bleed=0.12, name="mpc"),
    # The Game Crafter poker deck: 825x1125, 0.125" bleed each side
    "tgc": lambda: Spec(bleed=0.125, name="tgc"),
    # trimmed, for proofs and home printing
    "cut": lambda: Spec(bleed=0.0, name="cut"),
}

LAYOUT = dict(
    margin_x=52,      # index block, left edge
    margin_y=50,      # index block, top edge
    index_w=104,      # index column width
    num_cap=74,       # numeral cap height
    num_gap=12,       # numeral -> glyph
    glyph_d=100,      # index glyph: circumscribed diameter, so all four
                      # shapes carry the same weight in the corner
    field_d=636,      # diameter the central rosette is normalised to
    corner_r=37.5,    # 1/8" -- for the proof's rounded corners only
)

# ------------------------------------------------------------------- helpers

def hex_rgb(h):
    return tuple(int(h[i:i + 2], 16) for i in (1, 3, 5))


def load_shape(name):
    mask = Image.open(HERE / "shapes" / f"{name}.png").convert("L")
    meta = json.loads((HERE / "shapes" / "shapes.json").read_text())[name]
    rot = BASE_ROTATION.get(name, 0.0)
    if rot:
        cx, cy = meta["circle_centre"]
        mask = mask.rotate(-rot, resample=Image.BICUBIC, center=(cx, cy))
    return mask, meta


def _tile(mask, meta, rho, margin=0.0):
    """The shape on a transparent square, its enclosing circle centred and of
    radius `rho`.  Because all ink lies inside that circle, the tile can be
    rotated about its own centre without anything escaping.  `margin` leaves
    room for a later dilation."""
    r = meta["circle_radius"]
    cx, cy = meta["circle_centre"]
    s = rho / r
    w, h = mask.size
    small = mask.resize((max(1, round(w * s)), max(1, round(h * s))), Image.LANCZOS)
    side = 2 * math.ceil(rho + margin) + 6
    tile = Image.new("L", (side, side), 0)
    tile.paste(small, (round(side / 2 - cx * s), round(side / 2 - cy * s)))
    return tile


def _compose(tile, n, ring_r):
    """n copies of `tile` on a ring of radius `ring_r`, copy k turned by
    k*360/n so the union has n-fold rotational symmetry.  Returns the union
    alpha, the sum of the individual alphas, and the rotation centre."""
    side = tile.size[0]
    b = 2 * math.ceil(ring_r + side / 2) + 6
    cx = cy = b / 2.0
    union = np.zeros((b, b), np.float32)
    total = 0.0
    for k in range(n):
        phi = 360.0 * k / n
        piece = tile.rotate(-phi, resample=Image.BICUBIC, center=(side / 2, side / 2)) \
            if phi else tile
        px = cx + ring_r * math.sin(math.radians(phi))
        py = cy - ring_r * math.cos(math.radians(phi))
        pad = Image.new("L", (b, b), 0)
        pad.paste(piece, (round(px - side / 2), round(py - side / 2)))
        arr = np.asarray(pad, np.float32)
        total += float(arr.sum())
        np.maximum(union, arr, out=union)
    return union, total, (cx, cy)


def _grow(tile, px):
    """Dilate by a disk, so the overlap test measures the gap between copies
    rather than the ink itself."""
    if px < 1:
        return tile
    k = int(px)
    yy, xx = np.mgrid[-k:k + 1, -k:k + 1]
    disk = (xx * xx + yy * yy) <= k * k
    return Image.fromarray(ndimage.grey_dilation(np.asarray(tile), footprint=disk), "L")


def pack_ring(name, n, tol=0.0, rho=160):
    """Smallest ring radius (in units of the shape's enclosing radius) that
    still keeps the copies clear of each other.

    t = 1 means neighbouring enclosing circles are exactly tangent, which is
    always safe; below that the real, non-circular outlines are what decide,
    so we shrink until the rasterised copies actually start to touch.  Lets a
    wide shape like the eye nest far tighter than its circle would allow."""
    if n == 1:
        return 0.0
    mask, meta = load_shape(name)
    grow = round(CLEARANCE * rho)
    tile = _grow(_tile(mask, meta, rho, margin=grow + 2), grow)
    base = 1.0 / math.sin(math.pi / n)

    def overlaps(t):
        union, total, _ = _compose(tile, n, t * base * rho)
        return (total - float(union.sum())) / total > tol

    # dilated copies are tangent once t reaches 1 + CLEARANCE, so that is the
    # ceiling -- but nudge it up rather than ever hand back a clash
    lo, hi = 0.25, 1.0 + CLEARANCE + 0.02
    while overlaps(hi) and hi < 2.0:
        hi += 0.05
    for _ in range(18):
        mid = (lo + hi) / 2
        if overlaps(mid):
            lo = mid
        else:
            hi = mid
    return hi


_RING_CACHE = HERE / "shapes" / "rings.json"


def ring_table(rebuild=False):
    if _RING_CACHE.exists() and not rebuild:
        return json.loads(_RING_CACHE.read_text())
    table = {f"{s}:{n}": round(pack_ring(s, n), 5) for s in SHAPES for n in NUMBERS}
    _RING_CACHE.write_text(json.dumps(table, indent=2, sort_keys=True) + "\n")
    return table


def rosette(name, n, out_diameter, table=None):
    """The finished rosette as an alpha mask, scaled so its ink exactly spans
    a circle of `out_diameter` about the rotation centre, and cropped.
    Returns (mask, centre_xy_within_mask)."""
    table = table or ring_table()
    mask, meta = load_shape(name)
    t = table[f"{name}:{n}"]
    base = 0.0 if n == 1 else t / math.sin(math.pi / n)
    rho = 1300.0 / (base + 1.0)
    tile = _tile(mask, meta, rho)
    union, _, (cx, cy) = _compose(tile, n, base * rho)

    ys, xs = np.nonzero(union > 32)
    reach = float(np.hypot(xs - cx, ys - cy).max())      # true outer radius
    scale = (out_diameter / 2.0) / reach

    img = Image.fromarray(union.astype(np.uint8), "L")
    x0, y0, x1, y1 = int(xs.min()), int(ys.min()), int(xs.max()) + 1, int(ys.max()) + 1
    img = img.crop((x0, y0, x1, y1))
    new = (max(1, round(img.width * scale)), max(1, round(img.height * scale)))
    img = img.resize(new, Image.LANCZOS)
    return img, ((cx - x0) * scale, (cy - y0) * scale)


# -------------------------------------------------------------------- drawing

def stamp(card, mask, colour, cx, cy):
    """Paste `mask` as flat `colour`, centred on (cx, cy) in card pixels."""
    ink = Image.new("RGB", mask.size, colour)
    card.paste(ink, (round(cx - mask.width / 2), round(cy - mask.height / 2)), mask)


def digit_mask(d, cap_px, font_path=FONT):
    """A numeral cropped to its own ink and scaled to an exact cap height, so
    1 and 4 sit on the same baseline and optical centre."""
    probe = ImageFont.truetype(font_path, 400)
    tmp = Image.new("L", (900, 900), 0)
    ImageDraw.Draw(tmp).text((450, 450), str(d), font=probe, fill=255, anchor="mm")
    bbox = tmp.getbbox()
    glyph = tmp.crop(bbox)
    s = cap_px / glyph.height
    return glyph.resize((max(1, round(glyph.width * s)), round(cap_px)), Image.LANCZOS)


def index_block(shape_mask, meta, number, colour, L):
    """The corner index -- number over shape -- on its own transparent tile."""
    num = digit_mask(number, L["num_cap"])
    glyph = _tile(shape_mask, meta, L["glyph_d"] / 2).crop(
        (3, 3, L["glyph_d"] + 3, L["glyph_d"] + 3))
    w = max(L["index_w"], glyph.width)
    h = L["num_cap"] + L["num_gap"] + glyph.height
    tile = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    ink = Image.new("RGB", num.size, colour)
    tile.paste(ink, (round((w - num.width) / 2), 0), num)
    ink = Image.new("RGB", glyph.size, colour)
    tile.paste(ink, (round((w - glyph.width) / 2), L["num_cap"] + L["num_gap"]), glyph)
    return tile


def render_face(colour_name, shape, number, spec, table=None, bg="#FFFFFF"):
    L = LAYOUT
    colour = hex_rgb(COLOURS[colour_name])
    card = Image.new("RGB", (spec.w, spec.h), hex_rgb(bg))

    ros, (rx, ry) = rosette(shape, number, L["field_d"], table)
    fx, fy = spec.at(spec.cut_w / 2, spec.cut_h / 2)
    ink = Image.new("RGB", ros.size, colour)
    card.paste(ink, (round(fx - rx), round(fy - ry)), ros)

    shape_mask, shape_meta = load_shape(shape)
    tile = index_block(shape_mask, shape_meta, number, colour, L)
    x, y = spec.at(L["margin_x"], L["margin_y"])
    card.paste(tile, (round(x), round(y)), tile)
    x, y = spec.at(spec.cut_w - L["margin_x"] - tile.width,
                   spec.cut_h - L["margin_y"] - tile.height)
    flipped = tile.rotate(180)
    card.paste(flipped, (round(x), round(y)), flipped)
    return card


def _ring(card, names, colours, centre, ring_r, rho, phase=0.0):
    """One ring of shapes, each turned to face outward."""
    cx, cy = centre
    n = len(names)
    for k, name in enumerate(names):
        mask, meta = load_shape(name)
        phi = phase + 360.0 * k / n
        tile = _tile(mask, meta, rho)
        if phi:
            tile = tile.rotate(-phi, resample=Image.BICUBIC,
                               center=(tile.width / 2, tile.height / 2))
        stamp(card, tile, hex_rgb(COLOURS[colours[k % len(colours)]]),
              cx + ring_r * math.sin(math.radians(phi)),
              cy - ring_r * math.cos(math.radians(phi)))


def annulus(diameter, radius, width, feather=1.0):
    """An antialiased ring as an alpha mask, straight from the distance field.

    PIL's ellipse has no antialiasing, and supersampling a line only three
    pixels wide still comes back visibly stepped, so the coverage is computed
    rather than drawn."""
    # even, always: stamp() pastes at round(centre - n/2), and for odd n that
    # rounds a half-integer, putting the ring half a pixel off centre and
    # costing the back its exact half-turn symmetry
    n = int(round(diameter))
    n += n % 2
    g = np.arange(n) - (n - 1) / 2.0
    r = np.hypot(*np.meshgrid(g, g, indexing="xy"))
    cover = np.clip((width / 2.0 - np.abs(r - radius)) / feather + 0.5, 0.0, 1.0)
    return Image.fromarray((cover * 255).astype(np.uint8), "L")


def render_back(spec, ground="#191324"):
    """Two concentric rings: four eyes inside, and twelve of the remaining
    three shapes outside, alternating yellow and purple.

    The shapes repeat every three places and the colours every two, so the
    pattern comes back around every six -- half of twelve.  Turn the card end
    over end and every symbol lands on a copy of itself in the same colour,
    which is the symmetry that matters, a card being a rectangle: there is no
    way to tell from the back which way up one is being held.

    The emblem is built on the trimmed size, which is even in both directions,
    and then laid into the bleed.  Some presets bleed by half a pixel (the
    Game Crafter trims 0.125in off 825px), and centring the emblem on that
    would round the symbols apart by one pixel and spoil the symmetry.
    """
    w, h = spec.cut_w, spec.cut_h
    face = Image.new("RGB", (w, h), hex_rgb(ground))
    centre = (w / 2.0, h / 2.0)

    outer = LAYOUT["field_d"] / 2 * 1.04
    ring_r = outer / (1 + math.sin(math.pi / 12))
    rho = ring_r * math.sin(math.pi / 12) * 0.92
    _ring(face, ["pyramid", "star", "helix"] * 4, ["yellow", "purple"],
          centre, ring_r, rho)

    gap = ring_r - rho / 0.92
    stamp(face, annulus(2 * gap + 16, gap, 4.0), hex_rgb(COLOURS["yellow"]), *centre)

    inner = gap * 0.90
    r_in = inner / (1 + math.sin(math.pi / 4))
    _ring(face, ["eye"] * 4, ["green"], centre, r_in,
          r_in * math.sin(math.pi / 4) * 0.94)

    if (spec.w, spec.h) == (w, h):
        return face
    card = Image.new("RGB", (spec.w, spec.h), hex_rgb(ground))
    card.paste(face, (round(spec.ox), round(spec.oy)))
    return card


def deck():
    return [(c, s, n) for c in COLOURS for s in SHAPES for n in NUMBERS]
