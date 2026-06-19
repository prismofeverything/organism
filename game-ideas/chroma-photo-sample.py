#!/usr/bin/env python3
"""PROVISIONAL Chroma calibration-photo hue sampler.

Reads a single backlit overlap-grid photo, segments each translucent disc,
and reports its mean sRGB + nearest match in the CURRENT color schema
(chroma-core.js chip hexes, both RYB and CMY palettes).

This is a PRELIMINARY test against the current schema only (Mohammad approved
2026-06-09). It applies NO changes to the canonical schema/mixing model — the
real calibration waits on the physical color swatch arriving later this week.
NOT the same as chroma-analyze.js (that scans game-balance sim logs, not photos).

Usage: python3 chroma-photo-sample.py <photo.jpg>
"""
import sys, json
import numpy as np
from PIL import Image
from scipy import ndimage
from colorsys import rgb_to_hsv

# --- current color schema, lifted verbatim from chroma-core.js chip:{} ---
SCHEMA = {
    "RYB": {"R": "#e23b3b", "O": "#ef8a2b", "Y": "#f4d030",
            "G": "#3fae54", "B": "#2f6fd0", "P": "#7a3fb0"},
    "CMY": {"C": "#22c3d6", "M": "#d63cae", "Y": "#f4d030",
            "R": "#e23b3b", "G": "#3fae54", "B": "#2f6fd0"},
}
NAME = {"R": "Red", "O": "Orange", "Y": "Yellow", "G": "Green", "B": "Blue",
        "P": "Purple", "C": "Cyan", "M": "Magenta"}


def hex2rgb(h):
    h = h.lstrip("#")
    return np.array([int(h[i:i+2], 16) for i in (0, 2, 4)], float)


def nearest(rgb, pal):
    best, bd = None, 1e18
    for k, hx in SCHEMA[pal].items():
        d = float(np.linalg.norm(rgb - hex2rgb(hx)))
        if d < bd:
            bd, best = d, k
    return best, round(bd, 1)


def main(path):
    img = Image.open(path).convert("RGB")
    # downscale for speed; keep aspect
    scale = 800.0 / max(img.size)
    small = img.resize((int(img.size[0]*scale), int(img.size[1]*scale)))
    arr = np.asarray(small, float) / 255.0
    H, W, _ = arr.shape

    # HSV per pixel (vectorized-ish via apply is slow; use numpy formulas)
    mx = arr.max(2); mn = arr.min(2); d = mx - mn
    S = np.where(mx > 0, d / np.maximum(mx, 1e-9), 0.0)
    V = mx
    # discs = saturated, mid-bright. background sheet = low S high V; surround = low V.
    mask = (S > 0.28) & (V > 0.20) & (V < 0.99)
    lbl, n = ndimage.label(mask)
    sizes = ndimage.sum(np.ones_like(lbl), lbl, range(1, n+1))
    big = [i+1 for i, s in enumerate(sizes) if s > (H*W) * 0.0008]  # drop noise

    discs = []
    arr255 = arr * 255.0
    for lab in big:
        m = lbl == lab
        ys, xs = np.where(m)
        cy, cx = ys.mean(), xs.mean()
        pix = arr255[m]
        mean_rgb = pix.mean(0)
        # reject dark/low-sat edge artifacts (dark surround leaking through mask):
        # a real backlit disc is bright AND saturated in its MEAN colour.
        mr = mean_rgb / 255.0
        m_sat = (mr.max() - mr.min()) / max(mr.max(), 1e-9)
        on_border = (xs.min() <= 1 or xs.max() >= W-2 or ys.min() <= 1 or ys.max() >= H-2)
        if mean_rgb.max() < 90 or m_sat < 0.20 or on_border:
            continue
        # bimodality probe: split pixels by hue, measure spread -> overlap detector
        hues = np.array([rgb_to_hsv(*(p/255.0))[0] for p in pix])
        hue_std = float(np.std(hues))
        discs.append({
            "cx": float(cx), "cy": float(cy), "area_px": int(m.sum()),
            "mean_rgb": [int(round(v)) for v in mean_rgb],
            "hue_std": round(hue_std, 3),
        })

    # order top->bottom, left->right (row banding by cy)
    discs.sort(key=lambda dd: (round(dd["cy"]/ (H/6)), dd["cx"]))

    out = []
    for i, dd in enumerate(discs, 1):
        rgb = np.array(dd["mean_rgb"], float)
        nr, dr = nearest(rgb, "RYB")
        nc, dc = nearest(rgb, "CMY")
        overlap = dd["hue_std"] > 0.12  # heuristic: two-color blob
        out.append({
            "id": i,
            "pos": [round(dd["cx"]), round(dd["cy"])],
            "mean_rgb": dd["mean_rgb"],
            "hex": "#%02x%02x%02x" % tuple(dd["mean_rgb"]),
            "nearest_RYB": f"{NAME[nr]} ({nr}) Δ{dr}",
            "nearest_CMY": f"{NAME[nc]} ({nc}) Δ{dc}",
            "likely_overlap": overlap,
            "hue_std": dd["hue_std"],
        })

    report = {
        "_PROVISIONAL": True,
        "_note": ("Preliminary auto-sample vs CURRENT schema only. Unverified. "
                  "Disc centers/means are best-effort segmentation, not lab-grade. "
                  "Real calibration pending physical swatch (later this week). "
                  "NO canonical changes applied."),
        "_caveat_orange": ("The center overlap blob is an ORANGE chit over light-blue, "
                           "NOT red. The intended red+blue->purple test is INVALID in "
                           "this shot; exclude that data point."),
        "source": path,
        "discs_found": len(out),
        "discs": out,
    }
    print(json.dumps(report, indent=1))


if __name__ == "__main__":
    main(sys.argv[1])
