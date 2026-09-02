#!/usr/bin/env bash
# The canonical ORGANISM hero still — "what's in the box".
#
# This deliberately does NOT contain its own copy of the layout. That is exactly
# what went wrong: build_layout.py (Jun), build_unbox.py (Jul) and build_anim.py
# (Aug) each carried a drifting version of the hero geometry, so the real-scale
# fixes landed in build_anim.py only. Rendering build_unbox.py produced a shot
# with 2D minimal disks that are no longer in the product, mutation cards at 0.43x
# their real size, a rulebook and player aids at ~0.67x, and an off-centre box
# cover. build_anim.py holds the ONE corrected layout (real scale, 1u = 1mm);
# this asks it for the single frame where everything has landed.
#
#   FRAME=390       END — the board has finished unfolding, everything is in place
#   ZS=445          push the closing swoop PAST that frame, so the camera holds
#                   the hero framing instead of diving into the board
#   TWOD=0          no 2D minimal disks (minimal edition only)
#   PLDENS/PLEMIT=0 no plasma column blowing out the middle of the board
#
# Framing: the stock hero camera leaves ~38% of the frame empty and sits off
# centre. HP*/HA* pull it in and pan right; then the render is cropped to the
# actual content with a uniform margin, which beats chasing the camera (moving
# closer magnifies the near player sets fastest, so the bottom margin collapses
# before the sides are tight).
#
#   ./render_hero.sh                      # 2560x1600 -> cropped ~2448x1548
#   RESX=3840 RESY=2400 ./render_hero.sh  # print master
set -euo pipefail
cd "$(dirname "$0")"

RESX="${RESX:-2560}" RESY="${RESY:-1600}" SAMPLES="${SAMPLES:-48}"
MARGIN="${MARGIN:-0.045}"                 # uniform breathing room, fraction of the content
FR="${FR:-/mnt/data/archive/organism-renders/hero}"

TWOD=0 FRAME=390 ZS=445 PLDENS=0 PLEMIT=0 \
HPX=0 HPY=-1450 HPZ=837 HAX=0 HAY=74 HAZ=127 \
RESX="$RESX" RESY="$RESY" SAMPLES="$SAMPLES" FR="$FR" \
  ~/.claude/bin/safe-blender -b --python build_anim.py

../.venv/bin/python - "$FR/single_0390.png" "$FR/organism-hero-tight.png" "$MARGIN" <<'CROP'
import sys
import numpy as np
from PIL import Image
src, dst, margin = sys.argv[1], sys.argv[2], float(sys.argv[3])
im = Image.open(src).convert("RGB")
a = np.asarray(im).astype(int); W, H = im.size
bg = np.median(np.concatenate([a[0:8, 0:8].reshape(-1, 3), a[0:8, -8:].reshape(-1, 3)]), axis=0)
ys, xs = np.where(np.abs(a - bg).sum(axis=2) > 38)
x0, x1, y0, y1 = xs.min(), xs.max(), ys.min(), ys.max()
pad = int(round(margin * max(x1 - x0 + 1, y1 - y0 + 1)))
# Horizontally the crop stays SYMMETRIC about the frame centre. The camera looks
# straight down the board's axis, so the board is centred in the render; cropping
# to the raw content box instead would shove it off-centre, because the layout
# carries more furniture on one side (mutations + power board) than the other.
half = max(W / 2.0 - (x0 - pad), (x1 + pad) - W / 2.0)
cx0, cx1 = int(max(0, W / 2.0 - half)), int(min(W, W / 2.0 + half))
out = im.crop((cx0, max(0, y0 - pad), cx1, min(H, y1 + 1 + pad)))
out.save(dst)
print(f"cropped {W}x{H} -> {out.size[0]}x{out.size[1]} ({pad}px margin, board centred)")
CROP

echo "hero still -> $FR/organism-hero-tight.png"
echo "keep the master in resources/public/img/ — /mnt/data is scratch and gets swept."
