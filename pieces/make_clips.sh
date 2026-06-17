#!/usr/bin/env bash
# Render one or more storyboard clips to 1080² mp4s in scene/clip_<name>.mp4
#   ./make_clips.sh move eat grow ...
# Env: SAMPLES (default 48), RES (default 1080), THREADS (2), NICE (19)
# THREADS=2 / nice -19 are intentionally gentle: threads-4 has frozen this box on big
# animation renders. Bump THREADS for a quick low-frame pass; keep it low for full batches.
set -e
# auto-detect newest installed blender-5.1.* (5.1.1 / 5.1.2); override with BLENDER=/path
BL=${BLENDER:-$(ls -d "$HOME"/Downloads/blender-5.1.*/blender 2>/dev/null | sort -V | tail -1)}
BL=${BL:-$HOME/Downloads/blender-5.1.2-linux-x64/blender}
SAMPLES=${SAMPLES:-48}
RES=${RES:-1080}
THREADS=${THREADS:-2}
NICE=${NICE:-19}
cd "$(dirname "$0")"
for CLIP in "$@"; do
  DIR=renders/clips/$CLIP
  rm -rf "$DIR"; mkdir -p "$DIR"
  echo "=== rendering $CLIP (samples=$SAMPLES res=$RES threads=$THREADS) ==="
  CLIP=$CLIP SAMPLES=$SAMPLES RES=$RES nice -n "$NICE" "$BL" --background --threads "$THREADS" --python build_clip.py
  nice -n "$NICE" ffmpeg -y -framerate 24 -i "$DIR/f%04d.png" -c:v libx264 -pix_fmt yuv420p -crf 18 scene/clip_$CLIP.mp4 2>/dev/null
  echo "wrote scene/clip_$CLIP.mp4"
done
