#!/usr/bin/env bash
# Build a labelled contact sheet (one frame per clip) for reviewing them all at once.
#   ./make_contact.sh   ->   scene/contact_sheet.png
set -e
cd "$(dirname "$0")"
TMP=$(mktemp -d)
CLIPS=(box board three_org two_org power eat move grow circulate conflict perish glow)
for c in "${CLIPS[@]}"; do
  f=scene/clip_$c.mp4
  [ -f "$f" ] || { echo "skip $c (no mp4)"; continue; }
  d=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$f")
  t=$(awk "BEGIN{print $d*0.8}")                 # 80% in: the resolved state
  ffmpeg -ss "$t" -i "$f" -vframes 1 "$TMP/$c.png" -y 2>/dev/null
done
../.venv/bin/python - "$TMP" <<'PY'
import sys, os
from PIL import Image, ImageDraw, ImageFont
tmp = sys.argv[1]
order = ["box","board","three_org","two_org","power","eat","move","grow","circulate","conflict","perish","glow"]
imgs = [(n, os.path.join(tmp, n+".png")) for n in order if os.path.exists(os.path.join(tmp, n+".png"))]
cell, cols, lab = 430, 4, 30
rows = (len(imgs) + cols - 1) // cols
sheet = Image.new("RGB", (cols*cell, rows*(cell+lab)), (18, 18, 20))
draw = ImageDraw.Draw(sheet)
try: font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf", 22)
except Exception: font = ImageFont.load_default()
for i, (name, path) in enumerate(imgs):
    r, c = divmod(i, cols); x, y = c*cell, r*(cell+lab)
    sheet.paste(Image.open(path).convert("RGB").resize((cell, cell)), (x, y+lab))
    draw.text((x+8, y+4), name, font=font, fill=(235, 235, 235))
sheet.save("scene/contact_sheet.png")
print("wrote scene/contact_sheet.png", sheet.size)
PY
rm -rf "$TMP"
