"""Vectorize the ORGANISM rulebook cover to SVG: a faithful COLOR vector + an OUTLINE (linework)
version. Flat-color art -> quantize to clean regions -> vtracer spline trace -> strip fills for the
outline. Run: .venv/bin/python pieces/vectorize_cover.py [colors] [speckle] [stroke]"""
import sys, os, re, vtracer
from PIL import Image

SRC = sys.argv[1] if len(sys.argv) > 1 else "pieces/clip_assets/box_top.png"   # 3683 master (crisp text)
OUTDIR = "resources/public/img"
COLORS  = int(sys.argv[2]) if len(sys.argv) > 2 else 12
SPECKLE = int(sys.argv[3]) if len(sys.argv) > 3 else 4    # low: keeps thin strokes (e.g. the "I" in TIEDMERS)
STROKE  = sys.argv[4] if len(sys.argv) > 4 else "6"       # scaled for 3683 canvas

# 1. quantize -> flat regions, kills anti-alias fuzz at color boundaries so traces stay clean
img = Image.open(SRC).convert("RGB")
q = img.quantize(colors=COLORS, method=Image.MEDIANCUT, dither=Image.Dither.NONE).convert("RGB")
qpath = "/tmp/claude-1000/-home-pattern-code-organism/1a8e89a9-18d2-4a11-be32-4efbc830a54d/scratchpad/cover_quant.png"
q.save(qpath)

# 2. color vector
color_svg = os.path.join(OUTDIR, "cover-vector.svg")
vtracer.convert_image_to_svg_py(qpath, color_svg, colormode="color", hierarchical="stacked",
    mode="spline", filter_speckle=SPECKLE, color_precision=8, layer_difference=16,
    corner_threshold=60, length_threshold=4.0, splice_threshold=45, path_precision=3)

# 3. outline = same geometry, fills removed, boundaries stroked
svg = open(color_svg).read()
outline = re.sub(r'fill\s*=\s*"[^"]*"', 'fill="none"', svg)
# add a stroke to every path
outline = outline.replace("<path ", f'<path stroke="#141414" stroke-width="{STROKE}" ')
open(os.path.join(OUTDIR, "cover-outline.svg"), "w").write(outline)

npaths = svg.count("<path")
print(f"colors={COLORS} speckle={SPECKLE} stroke={STROKE} | paths={npaths}")
print("wrote", color_svg)
print("wrote", os.path.join(OUTDIR, "cover-outline.svg"))
for f in (color_svg, os.path.join(OUTDIR, "cover-outline.svg")):
    print(f"  {f}: {os.path.getsize(f)//1024} KB")
