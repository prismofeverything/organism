#!/usr/bin/env bash
# Board-review stills for the 2-3p 40cm board: a late Zach/Dan/Ryan position (OGF frame 262)
# on each board variant from three angles (storyboards.py b23_*), plus one .blend per variant.
#   ./board23.sh [board.png ...]   (default: the two V1 drops in ~/Downloads)
set -euo pipefail
cd "$(dirname "$0")"
BL=${BLENDER:-$(command -v safe-blender || echo "$HOME/.claude/bin/safe-blender")}
OUT=${OUT:-/mnt/data/archive/organism-renders/board23}; RES=${RES:-1600}
BOARDS=("$@")
[ ${#BOARDS[@]} -eq 0 ] && BOARDS=("$HOME"/Downloads/Organism_Board_2-3_BACKSIDE_40cm_26_V1*.png)
for b in "${BOARDS[@]}"; do
  name=$(basename "$b" .png)
  for clip in b23_top b23_high b23_low; do
    save=""; [ "$clip" = b23_high ] && save="$OUT/$name.blend"
    mkdir -p "$OUT/$name"
    CLIP=$clip FRAME=0 RES=$RES BOARD="$b" BOARDMM=400 SCALE=42.8 FR="$OUT/$name/$clip" \
      SAVEBLEND="$save" "$BL" -b --threads 4 --python build_clip.py >"$OUT/$name/$clip.log" 2>&1
    mv "$OUT/$name/$clip/single_0000.png" "$OUT/$name/$clip.png" && rmdir "$OUT/$name/$clip"
    echo "$OUT/$name/$clip.png"
  done
done
