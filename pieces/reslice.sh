#!/usr/bin/env bash
# Re-slice every print gcode from the current STLs, using each gcode's OWN embedded config.
#
# Every PrusaSlicer gcode carries its full config between "; prusaslicer_config = begin/end";
# stripping the leading "; " yields a valid .ini. So the hand-tuned GUI settings are recoverable
# and the gcode is reproducible headlessly -- no need to re-dial anything in the GUI.
#
# Verified faithful: re-slicing UNCHANGED geometry (eat_single.stl) reproduced the original
# gcode exactly -- same 52m 42s, 219 layers, 1665.50mm filament.
#
# PrusaSlicer 2.9.5 is a flatpak here (no prusa-slicer on PATH); --filesystem=home is required
# or it cannot read the STLs. Output is a DIRECTORY so the embedded output_filename_format
# names the file (…_0.2mm_PETG_MK3S_<time>.gcode) consistently.
set -e
cd "$(dirname "$0")"
INI=.reslice
mkdir -p $INI

# (re)extract configs from whatever gcode is present
../.venv/bin/python - <<'PY'
import io, os
def extract(g, out):
    keep=[]; on=False
    for line in io.open(g, encoding="utf-8", errors="replace"):
        if line.startswith("; prusaslicer_config = begin"): on=True; continue
        if line.startswith("; prusaslicer_config = end"): break
        if on and line.startswith("; "): keep.append(line[2:])
    if keep: io.open(out,"w",encoding="utf-8").writelines(keep)
for d in ("stl", "renders/food"):
    for g in sorted(os.listdir(d)):
        if g.endswith(".gcode"):
            base = g.split("_0.2mm")[0]
            if not os.path.exists(f".reslice/{base}.ini"):
                extract(os.path.join(d,g), f".reslice/{base}.ini")
PY

slice_one() { # stl  ini-basename  outdir
  [ -f "$INI/$2.ini" ] || { echo "SKIP $1 (no $INI/$2.ini)"; return; }
  timeout 570 flatpak run --filesystem=home com.prusa3d.PrusaSlicer \
    --load "$INI/$2.ini" --export-gcode --output "$3" "$1" 2>&1 \
    | grep -E "Slicing result|rror" || true
}
slice_one stl/eat_single.stl            eat_single    stl/
slice_one stl/move_single.stl           move_single   stl/
slice_one stl/grow_single.stl           grow_single   stl/
slice_one stl/eat_plate.stl             eat_plate     stl/
slice_one stl/move_plate.stl            move_plate    stl/
slice_one stl/grow_plate.stl            grow_plate    stl/
slice_one stl/grow_plate3.stl           grow_plate3   stl/
slice_one renders/food/print_plate.stl  print_plate   renders/food/
slice_one renders/food/pieces_plate.stl pieces_plate  renders/food/
echo "##### reslice done #####"
