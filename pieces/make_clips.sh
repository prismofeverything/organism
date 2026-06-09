#!/usr/bin/env bash
# Render one or more storyboard clips to 1080² mp4s in scene/clip_<name>.mp4
#   ./make_clips.sh move eat grow ...
# Env: SAMPLES (default 48), RES (default 1080)
set -e
BL=${BLENDER:-$HOME/Downloads/blender-5.1.1-linux-x64/blender}
SAMPLES=${SAMPLES:-48}
RES=${RES:-1080}
cd "$(dirname "$0")"
for CLIP in "$@"; do
  DIR=renders/clips/$CLIP
  rm -rf "$DIR"; mkdir -p "$DIR"
  echo "=== rendering $CLIP (samples=$SAMPLES res=$RES) ==="
  CLIP=$CLIP SAMPLES=$SAMPLES RES=$RES nice -n 10 "$BL" --background --threads 4 --python build_clip.py
  nice -n 10 ffmpeg -y -framerate 24 -i "$DIR/f%04d.png" -c:v libx264 -pix_fmt yuv420p -crf 18 scene/clip_$CLIP.mp4 2>/dev/null
  echo "wrote scene/clip_$CLIP.mp4"
done
