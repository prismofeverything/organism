#!/usr/bin/env bash
# Sculpt-piece bottom-flatten step (the LAST step of the sculpt-piece path):
#
#   {t}_sculpt.obj            hand sculpt (from ~/blender/organism/era-three/{EAT1,MOVE2,GROW3}.obj)
#   {t}_sculpt_graft_raw.obj  + universal connector grafted on TOP   (build_sculpt_graft.py)  [ORIGINAL / pre-flatten]
#   {t}_sculpt_graft.obj      - 2mm off the BOTTOM for standing stability + print bed adhesion  [FINAL — every loader uses this]
#
# The grafts (raw) are STABLE — do not re-tune. This script only (re)builds the FINAL from the raw
# by cutting 2mm off the bottom. EAT/MOVE/GROW all get 2mm (EAT was already right; MOVE/GROW were
# previously over-cut at 4/5mm — see memory project_bottom_flatten).
set -e
cd "$(dirname "$0")"
BL="${BLENDER:-$HOME/Downloads/blender-5.1.2-linux-x64/blender}"
for t in EAT MOVE GROW; do
  FLAT_IN="out/${t}_sculpt_graft_raw.obj" FLAT_MM="${FLAT_MM:-2}" FLAT_OUT="out/${t}_sculpt_graft.obj" \
    nice -n 19 "$BL" -b --python flatten_bottom.py
done
echo "flattened EAT/MOVE/GROW -> out/*_sculpt_graft.obj (final)"
