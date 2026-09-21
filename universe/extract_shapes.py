#!/usr/bin/env python3
"""Turn the hand-inked source PNGs into clean, colorisable alpha mattes.

The sources are black ink on white at 3000x3000.  We invert luminance into an
alpha channel (so the antialiased brush edges survive), drop any stroke that
runs off the canvas -- the pyramid's four light rays are all clipped -- and
crop to the remaining ink.

Alongside each matte we record the minimum enclosing circle of the ink, which
is what the rosette packer uses to place copies without overlap.
"""
import json
import math
import pathlib

import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage

HERE = pathlib.Path(__file__).parent
SRC = HERE / "inputs"
DST = HERE / "shapes"

SHAPES = ["eye", "helix", "pyramid", "star"]

# Evens out the drawing pressure across the four marks, measured as ink area
# over enclosing-circle area: eye 0.33, pyramid 0.23, star 0.27, and the helix
# thinnest at 0.19 until nudged.  Source pixels; the shapes land on the card at
# roughly a fifth of this scale, so this also keeps the finest strokes above
# what CMYK can hold.
WEIGHT = {"eye": 0, "helix": 6, "pyramid": 0, "star": 0}

# What to do with a stroke that runs off the edge of its canvas.  The pyramid's
# four light rays are decoration and are simply dropped.  The star's lower-left
# arm is one of its five points, so it is reconstructed instead: both edges of
# the arm are near-perfect straight lines (fit residuals 6.5 and 8.7px), so the
# arm is one of its five points, so it is reconstructed instead.  The value is
# the brush radius in source pixels: all four intact arm tips measure 76, the
# artist's brush being a circle, and each arm is two strokes of it converging.
# Reconstructing on that basis puts the new tip 1749px from the star's hub,
# against 1502 / 1499 / 1635 / 1785 for the four that survived.
CLIPPED = {"pyramid": "drop", "star": 76}
PAD = 8          # px of transparent padding kept around the ink
INK_CUTOFF = 8   # alpha below this is treated as paper, not ink
LINK_LEVEL = 2   # connectivity threshold: low, so a stroke and its antialiased
                 # halo count as one component
MIN_AREA = 50    # px; anything smaller is a fragment of a clipped stroke


def _anchored_quad(x, y, x0, y0, falloff=150.0):
    """Least-squares quadratic through (x0, y0) exactly, weighted towards it.

    Both matter.  Passing through the anchor keeps the reconstruction flush
    with the ink that is actually there -- an unanchored fit sat 13px off at
    the seam and left a step.  Weighting towards it means the slope and
    curvature that get extrapolated are the ones near the join, not an average
    over the whole arm."""
    d = x - x0
    wt = np.exp(-np.abs(d) / falloff)
    A = np.c_[d * d, d] * wt[:, None]
    k = np.linalg.lstsq(A, (y - y0) * wt, rcond=None)[0]
    return np.array([k[0], k[1] - 2 * k[0] * x0, k[0] * x0 * x0 - k[1] * x0 + y0])


def extend_clipped(alpha, brush, pad=420, level=2, track=400, seam=3.0):
    """Carry a stroke that was cut off by the canvas edge out past it.

    The brush is a circle, so an arm of the star is two strokes of that radius
    converging, and its tip is simply the brush where the two centrelines
    meet.  So: read the wedge's two edges inward from the border, push each
    one in by the brush radius to recover the stroke centreline it came from,
    carry those two centrelines out past the edge until they cross, and sweep
    the brush along them.  The tip rounds itself off.
    """
    alpha = np.pad(alpha, pad)
    ink = alpha > level
    h, w = ink.shape
    k = 4                                       # supersample for a clean edge
    add = Image.new("L", (w * k, h * k), 0)
    draw = ImageDraw.Draw(add)
    made = 0

    for side in ("left", "right", "top", "bottom"):
        vertical = side in ("left", "right")
        base = pad if side in ("left", "top") else (w if vertical else h) - pad - 1
        out = -1 if side in ("left", "top") else 1
        line = ink[:, base] if vertical else ink[base, :]
        idx = np.nonzero(line)[0]
        if not len(idx):
            continue
        runs, start = [], idx[0]
        for u, v in zip(idx, idx[1:]):
            if v != u + 1:
                runs.append((start, u))
                start = v
        runs.append((start, idx[-1]))

        for lo, hi in runs:
            mid, rows, width0 = (lo + hi) / 2.0, [], hi - lo + 1
            for step in range(track):
                u = base - out * step                       # inward
                strip = ink[:, u] if vertical else ink[u, :]
                c = int(round(mid))
                if not strip[c]:
                    near = np.nonzero(strip)[0]
                    if not len(near):
                        break
                    c = int(near[np.argmin(abs(near - mid))])
                p0 = c
                while p0 > 0 and strip[p0 - 1]:
                    p0 -= 1
                p1 = c
                while p1 < len(strip) - 1 and strip[p1 + 1]:
                    p1 += 1
                if p1 - p0 + 1 > 2.8 * width0:              # merged into the body
                    break
                rows.append((step, p0, p1))
                mid = (p0 + p1) / 2.0
            if len(rows) < 60 or width0 < 2 * brush:
                continue
            R = np.array(rows, float)
            T = R[:, 0]

            # push each edge in by the brush radius -> its stroke centreline
            cents = []
            for col, sign in ((1, +1.0), (2, -1.0)):
                # normal direction from a smooth fit, position from the ink
                # itself -- the fit is several pixels out near the border
                e = np.polyfit(T, R[:, col], 2)
                sl = np.polyval(np.polyder(e), T)
                n = np.hypot(1.0, sl)
                ox = T - sign * brush * sl / n
                oy = R[:, col] + sign * brush / n
                j = int(np.argmin(np.abs(T - seam)))
                cents.append(_anchored_quad(ox, oy, ox[j], oy[j]))
            cu, cl = cents

            roots = np.roots(cu - cl)
            cross = [r.real for r in roots if abs(r.imag) < 1e-9 and r.real < seam]
            if not cross:
                continue
            tip_t = max(cross)
            if seam - tip_t > track:                        # implausible reach
                continue
            tip_u = float(np.polyval(cu, tip_t))

            def xy(t, u):
                # on a left/right border the edge reading is a y and the walk
                # runs along x; on top/bottom it is the other way round
                return (base - out * t, u) if vertical else (u, base - out * t)

            def disc(t, u, r):
                x, y = xy(t, u)
                draw.ellipse([(x - r) * k, (y - r) * k, (x + r) * k, (y + r) * k], fill=255)

            # start the sweep well inside the seam: a disc centred up to one
            # diameter in still reaches across it, and without those the
            # envelope pinches in just outside the border.  Sweeping over ink
            # that is already there costs nothing -- this is a max composite.
            for c in (cu, cl):
                t = seam + 2.5 * brush
                while t >= tip_t:
                    disc(t, float(np.polyval(c, t)), brush)
                    t -= 1.0
            disc(tip_t, tip_u, brush)
            made += 1

    if not made:
        return alpha[pad:-pad, pad:-pad], 0
    # BOX, not LANCZOS: a windowed-sinc downsample rings and leaves a dust of
    # stray specks around the new edge
    grown = np.array(add.resize((w, h), Image.BOX))
    # keep only what lies beyond the original canvas, so the lead-in cannot
    # nudge a single pixel of the artist's own ink
    grown[pad:-pad, pad:-pad] = 0
    return np.maximum(alpha, grown), made


def min_enclosing_circle(points, iters=64):
    """Ritter's algorithm plus a few shrink-wrap passes. Good to <0.1%."""
    p = points[np.random.default_rng(0).choice(len(points), min(len(points), 20000), replace=False)]
    # Ritter seed
    a = p[np.argmin(p[:, 0])]
    b = p[np.argmax(np.sum((p - a) ** 2, axis=1))]
    c = p[np.argmax(np.sum((p - b) ** 2, axis=1))]
    centre = (b + c) / 2.0
    radius = np.linalg.norm(b - c) / 2.0
    for _ in range(iters):
        d = np.linalg.norm(p - centre, axis=1)
        far = np.argmax(d)
        if d[far] <= radius + 1e-9:
            break
        # grow just enough to swallow the outlier
        new_r = (radius + d[far]) / 2.0
        centre = centre + (p[far] - centre) * (new_r - radius) / d[far]
        radius = new_r
    # final exact-ish pass over every point
    d = np.linalg.norm(points - centre, axis=1)
    radius = float(d.max())
    return centre, radius


def arm_angles(alpha, centre, n_bins=720):
    """Angular profile of how far the ink reaches from `centre` (screen coords,
    0 deg = up, clockwise positive).  Used to find a shape's natural 'up'."""
    ys, xs = np.nonzero(alpha > 128)
    dx = xs - centre[0]
    dy = ys - centre[1]
    r = np.hypot(dx, dy)
    theta = np.degrees(np.arctan2(dx, -dy)) % 360.0
    idx = (theta / 360.0 * n_bins).astype(int) % n_bins
    reach = np.zeros(n_bins)
    np.maximum.at(reach, idx, r)
    return reach


def main():
    DST.mkdir(exist_ok=True)
    meta = {}
    for name in SHAPES:
        grey = np.array(Image.open(SRC / f"{name}.png").convert("L")).astype(np.int16)
        alpha = (255 - grey).clip(0, 255).astype(np.uint8)

        # drop strokes that are clipped by the canvas edge
        policy = CLIPPED.get(name, "drop")
        extended = 0
        if policy != "drop":
            alpha, extended = extend_clipped(alpha, policy, level=LINK_LEVEL)

        lbl, n = ndimage.label(alpha > LINK_LEVEL)
        h, w = alpha.shape
        areas = ndimage.sum(np.ones_like(lbl), lbl, range(1, n + 1))
        dropped = []
        for i, sl in enumerate(ndimage.find_objects(lbl), start=1):
            touches = (sl[0].start == 0 or sl[1].start == 0
                       or sl[0].stop >= h or sl[1].stop >= w)
            if (touches and policy == "drop") or areas[i - 1] < MIN_AREA:
                dropped.append(["clipped" if touches else "speck", int(areas[i - 1])])
                alpha[lbl == i] = 0
        alpha[alpha < INK_CUTOFF] = 0

        grow = WEIGHT.get(name, 0)
        if grow:
            yy, xx = np.mgrid[-grow:grow + 1, -grow:grow + 1]
            alpha = ndimage.grey_dilation(alpha, footprint=(xx * xx + yy * yy) <= grow * grow)

        ys, xs = np.nonzero(alpha)
        y0, y1 = ys.min(), ys.max() + 1
        x0, x1 = xs.min(), xs.max() + 1
        cropped = alpha[y0:y1, x0:x1]
        cropped = np.pad(cropped, PAD)

        pts = np.stack(np.nonzero(cropped > 127)[::-1], axis=1).astype(float)  # (x, y)
        centre, radius = min_enclosing_circle(pts)
        cy, cx = ndimage.center_of_mass(cropped.astype(float))

        Image.fromarray(cropped, mode="L").save(DST / f"{name}.png")
        meta[name] = {
            "size": [int(cropped.shape[1]), int(cropped.shape[0])],
            "circle_centre": [round(float(centre[0]), 2), round(float(centre[1]), 2)],
            "circle_radius": round(float(radius), 2),
            "centroid": [round(float(cx), 2), round(float(cy), 2)],
            "dropped_components": dropped,
            "ink_fraction": round(float((cropped > 127).mean()), 4),
        }
        print(f"{name:8s} {cropped.shape[1]:5d}x{cropped.shape[0]:<5d} "
              f"r={radius:7.1f} centre=({centre[0]:7.1f},{centre[1]:7.1f}) "
              f"dropped={len(dropped)} extended={extended}")

    (DST / "shapes.json").write_text(json.dumps(meta, indent=2) + "\n")


if __name__ == "__main__":
    main()
