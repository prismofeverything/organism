"""Resolve a self-intersecting SVG silhouette into a clean simple boundary (venv-side, uses
shapely — Blender's bundled Python has neither shapely nor svgelements). Densely samples the SVG
path, runs shapely make_valid to union self-overlaps into one filled region, and writes the clean
exterior (+ any holes) as JSON for build_minimal.py to extrude.

  ../.venv/bin/python clean_svg.py inputs/grow.svg inputs/grow_clean.json 1600
"""
import sys, json
import numpy as np
from svgelements import SVG, Path
from shapely.geometry import Polygon
from shapely import make_valid
from shapely.affinity import translate

svg_path, out_json = sys.argv[1], sys.argv[2]
N = int(sys.argv[3]) if len(sys.argv) > 3 else 1600

svg = SVG.parse(svg_path)
paths = [e for e in svg.elements() if isinstance(e, Path)]
if not paths:
    raise SystemExit(f"no <path> in {svg_path}")
p = paths[0]
pts = [(float(p.point(t).x), float(p.point(t).y)) for t in np.linspace(0.0, 1.0, N, endpoint=False)]

poly = make_valid(Polygon(pts))
if poly.geom_type == "MultiPolygon":
    poly = max(poly.geoms, key=lambda g: g.area)     # keep the largest filled region
# Recenter on the AREA CENTROID: for these C-n symmetric silhouettes the centroid IS the rotational-
# symmetry center, so the grafted peg/socket (placed at the origin downstream) land on the shape's
# true center. The bounding-box center used before was ~1.6 mm off for the C5 star (EAT).
cx, cy = poly.centroid.x, poly.centroid.y
poly = translate(poly, xoff=-cx, yoff=-cy)
data = {
    "exterior":  [[float(x), float(y)] for x, y in poly.exterior.coords],
    "interiors": [[[float(x), float(y)] for x, y in r.coords] for r in poly.interiors],
    "area": float(poly.area),
    "centroid_svg": [float(cx), float(cy)],
}
json.dump(data, open(out_json, "w"))
print(f"{svg_path}: valid_in={Polygon(pts).is_valid} -> area={poly.area:.1f} "
      f"exterior={len(data['exterior'])} holes={len(data['interiors'])} centered_on_centroid -> {out_json}")
