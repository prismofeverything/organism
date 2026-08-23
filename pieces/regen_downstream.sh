#!/usr/bin/env bash
# Rebuild EVERY artifact derived from out/*_sculpt_graft.obj, in dependency order.
#
# Run this after ANY change to the sculpt grafts (flatten depth, sliver strip, re-graft).
# Nothing below is wired into the Makefile, so without this the .obj masters drift ahead of
# the STLs, plates and gcode that actually get printed (that drift shipped an over-cut
# MOVE/GROW for ~5 days, Aug 2026).
#
#   1. stl/{EAT,MOVE,GROW}.stl        gather_stls.py
#   2. renders/food/{pieces,print}_plate.stl   build_print_plate.py + strip_slivers + preview
#   3. stl/{eat,move,grow}_{single,plate}.stl + grow_plate3.stl   build_grow_plate.py
#   4. gcode                          reslice.sh (separate, needs PrusaSlicer flatpak)
set -e
cd "$(dirname "$0")"
BL="${BLENDER:-$HOME/Downloads/blender-5.1.2-linux-x64/blender}"
PY="${PY:-../.venv/bin/python}"
BFLAGS=(--background --threads 4 --python)

echo "##### 1/3  stl/ bundle #####"
$PY gather_stls.py

echo; echo "##### 2/3  combined plates #####"
PIECES_ONLY=true nice -n 19 "$BL" "${BFLAGS[@]}" build_print_plate.py >/dev/null
$PY strip_slivers.py renders/food/pieces_plate.stl
PIECES_ONLY=true $PY preview_plate.py >/dev/null
nice -n 19 "$BL" "${BFLAGS[@]}" build_print_plate.py >/dev/null
$PY strip_slivers.py renders/food/print_plate.stl
$PY preview_plate.py >/dev/null

echo; echo "##### 3/3  per-piece singles + plates #####"
plate() { # PIECE COPIES OUTFILE
  PIECE_OBJ="out/$1_sculpt_graft.obj" PIECE_COPIES="$2" PIECE_PLATE_OUT="stl/$3" PLATE_CELL=48 \
    nice -n 19 "$BL" "${BFLAGS[@]}" build_grow_plate.py 2>&1 | grep -E "^wrote"
}
plate EAT  1 eat_single.stl
plate MOVE 1 move_single.stl
plate GROW 1 grow_single.stl
plate EAT  4 eat_plate.stl
plate MOVE 4 move_plate.stl
plate GROW 4 grow_plate.stl
plate GROW 3 grow_plate3.stl

echo; echo "##### done -- now re-slice: ./reslice.sh #####"
