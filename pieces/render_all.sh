#!/usr/bin/env bash
# Regenerate the ENTIRE ORGANISM clip set from durable source:
#   12 mechanic/board clips + intro beauty still + contact sheet.
# Frames -> renders/clips/<name>/ (symlinked to /mnt/data; root / is tight).
# mp4s   -> scene/clip_<name>.mp4 ; stills -> scene/.
#
# Safe by default: Blender 5.1.2, --threads 2, nice -19 (heavy renders have frozen
# this box). Bump THREADS if the machine has headroom.
#   ./render_all.sh                 # full set, full quality
#   SAMPLES=16 RES=720 ./render_all.sh   # fast preview pass
# Env: BLENDER THREADS NICE SAMPLES RES (board clips read these via make_clips.sh).
cd "$(dirname "$0")"
# auto-detect newest installed blender-5.1.* (5.1.1 / 5.1.2); override with BLENDER=/path
export BLENDER=${BLENDER:-$(ls -d "$HOME"/Downloads/blender-5.1.*/blender 2>/dev/null | sort -V | tail -1)}
export BLENDER=${BLENDER:-$HOME/Downloads/blender-5.1.2-linux-x64/blender}
export THREADS=${THREADS:-2}
export NICE=${NICE:-19}
export SAMPLES=${SAMPLES:-48}
export RES=${RES:-1080}
BL="$BLENDER"
say(){ echo "[$(date +%H:%M:%S)] $*"; }
ff(){ nice -n "$NICE" ffmpeg -y -framerate 24 -i "$1/f%04d.png" -c:v libx264 -pix_fmt yuv420p -crf 18 "$2" 2>/dev/null; }

say "BLENDER=$BL THREADS=$THREADS SAMPLES=$SAMPLES RES=$RES"

# --- 10 board/mechanic clips (build_clip.py via make_clips.sh) ---
# loop one-at-a-time so a single failure doesn't abort the rest (make_clips.sh has set -e)
for c in move eat grow circulate conflict perish two_org three_org board glow; do
  say "--- clip: $c ---"
  ./make_clips.sh "$c" || say "FAILED clip $c"
done

# --- power (own bg, samples 24 per CLIPS.md) ---
say "--- clip: power ---"
SAMPLES=24 nice -n "$NICE" "$BL" --background --threads "$THREADS" --python build_power.py \
  && ff renders/clips/power scene/clip_power.mp4 || say "FAILED power"

# --- box product shot (3D cover + hero pieces) ---
# NB: does NOT (re)generate scene/organism_title.png — that title card has no source
# script (CLIPS.md claims box makes it, but it doesn't). It is the one non-regenerable asset.
say "--- clip: box ---"
nice -n "$NICE" "$BL" --background --threads "$THREADS" --python build_box.py \
  && ff renders/clips/box scene/clip_box.mp4 || say "FAILED box"

# --- intro beauty still ---
say "--- intro still ---"
nice -n "$NICE" "$BL" --background --threads "$THREADS" --python render_intro.py || say "FAILED intro"

# --- contact sheet (one frame per clip, for reviewing the whole set) ---
say "--- contact sheet ---"
./make_contact.sh || say "FAILED contact sheet"

say "=== DONE ==="
ls -la scene/clip_*.mp4 2>/dev/null
