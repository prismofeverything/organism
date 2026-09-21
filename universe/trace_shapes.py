#!/usr/bin/env python3
"""Turn the alpha mattes into SVG outlines.

Each shape is traced with marching squares, simplified, and normalised so its
minimum enclosing circle is the unit circle at the origin -- which is the same
frame the rosette packer works in, so the SVG can reuse its geometry directly.
Holes come out as extra subpaths and are handled by an even-odd fill.
"""
import json
import pathlib

import numpy as np
from PIL import Image
from shapely.geometry import LineString
from skimage import measure

HERE = pathlib.Path(__file__).parent
TOL = 3.0        # source px; the shapes land on a card about 40x smaller


def trace(name, meta):
    a = np.asarray(Image.open(HERE / "shapes" / f"{name}.png")).astype(float) / 255.0
    a = np.pad(a, 1)                                     # close contours at the edge
    cx, cy = meta["circle_centre"]
    r = meta["circle_radius"]
    subpaths, pts = [], 0
    for contour in measure.find_contours(a, 0.5):
        if len(contour) < 8:
            continue
        xy = np.stack([contour[:, 1] - 1.0, contour[:, 0] - 1.0], axis=1)
        line = LineString(xy).simplify(TOL, preserve_topology=False)
        c = np.asarray(line.coords)
        if len(c) < 4:
            continue
        c = (c - (cx, cy)) / r                           # unit enclosing circle
        pts += len(c)
        d = f"M{c[0,0]:.4f},{c[0,1]:.4f}" + "".join(
            f"L{x:.4f},{y:.4f}" for x, y in c[1:]) + "Z"
        subpaths.append(d)
    return "".join(subpaths), pts


def main():
    meta = json.loads((HERE / "shapes" / "shapes.json").read_text())
    out = {}
    for name in ("eye", "helix", "pyramid", "star"):
        d, pts = trace(name, meta[name])
        out[name] = d
        # how far the outline reaches from the origin, for the rosette packer
        nums = np.array([float(v) for v in d.replace("M", " ").replace("L", " ")
                         .replace("Z", " ").replace(",", " ").split()])
        xy = nums.reshape(-1, 2)
        print(f"  {name:8s} {pts:5d} points  {len(d)/1024:5.1f} KB  "
              f"reach {np.hypot(xy[:,0], xy[:,1]).max():.3f}")
    (HERE / "shapes" / "paths.json").write_text(json.dumps(out) + "\n")
    print(f"  -> shapes/paths.json")


if __name__ == "__main__":
    main()
