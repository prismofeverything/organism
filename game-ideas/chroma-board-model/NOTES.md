# Chroma 3D-print board model — context & state

Pull this in to resume work on the physical Chroma board. Last updated 2026-06-18.

## What this is
Parametric OpenSCAD model of the Chroma game board: a flat-top hex board, **radius 5
(6 cells/edge) minus a 9-corner trim = 82 hex cells** (81 placeable wells + black center
`[0,0]`). Each cell is a circular recess that holds a stack of up to **3** translucent
chits over a backlight hole. Geometry mirrors the game engine (`../chroma-core.js`).

## Files
- `chroma_board.scad` — the model (all parameters documented inline at the top).
- `chroma_seed_data.scad` — AUTO-GENERATED sector + seed-color data (do not hand-edit).
- `../gen_seed_data.js` — regenerates the above from the engine: `node gen_seed_data.js`.
- `blender_chroma_board.py` — headless backlit viz/render → PNG.
- `chroma_board.stl` / `chroma_cluster.stl` / `chroma_single.stl` — full / 7-hex / 1-cell.
- `preview_full.png` — latest render.

## Build
```
cd ~/organism/game-ideas/chroma-board-model
openscad -o chroma_cluster.stl -D 'RENDER="cluster"' chroma_board.scad   # bed-friendly test
openscad -o chroma_board.stl   -D 'RENDER="full"'    chroma_board.scad   # full board (~10 min)
openscad -o x.stl -D '$fn=48'  -D 'RENDER="full"'    chroma_board.scad   # fast draft
blender -b -P blender_chroma_board.py -- chroma_board.stl preview_full.png
```
RENDER = "full" | "cluster" | "single". Per-cell differencing keeps the interactive
preview fast; the full CGAL export is ~10 min at $fn=96.

## Current geometry (all parametric)
| Spec | Value | Param |
|---|---|---|
| Chit diameter (measured) | 0.75 in / 19.05 mm | `chit_dia` |
| Recess (well) | 22 mm (= hole + 2·ledge) | derived |
| Light hole | 14 mm (+ 1 mm underside countersink) | `light_hole`, `hole_cs` |
| Support ledge | 4 mm | `ledge` |
| Stack depth | 3 chits → well 1.6 mm deep | `stack_n`, `chit_th`, `stack_clear` |
| One laminated chit thickness (est.) | ~0.4 mm | `chit_th` |
| Cell pitch | 26 mm | derived |
| Board thickness | 3.6 mm + 2 mm frame lip = 5.6 mm | — |
| Full board footprint | **261 × 266 mm** (needs ~300 mm bed) | — |
| Lit fraction / chit coverage margin | 54% / +1.05 mm | echoed at compile |

## Features on the board
- **Stress fillets** — rib-to-floor (`fillet_wall`=1.5) + ledge edge (`fillet_hole`=0.8);
  cell cavity is one rotate_extruded profile (countersink+hole+fillets+draft+rim chamfer).
- **Perimeter frame** — raised ring hugging the trimmed silhouette (`frame_w`=3, `frame_lip`=2)
  = stiff edge beam for one-piece handling.
- **Sector dividers** — raised ridges along the 6 wedge boundaries (`divider_w`=2, `divider_h`=1.5),
  follow cell edges, only between different sectors. = physical "thick wedge border" from the app.
- **Seed letters** — each of the 49 seeded cells has its start-color letter (C/M/Y/R/G/B, K at
  center) debossed 0.6 mm on the ledge at 6 o'clock (`seed_marks`). Visible at setup.
- **QoL** — 2° draft, rim chamfer, finger-scoop notch per well.
- `white_patch` exists but is OFF (Muhammad dropped it).

## Key decisions / rationale
- **Light hole kept big on purpose.** Game is light-STARVED: single gels 5–7% transmission,
  3-stacks 0.05–1.4% (at/below the camera black-floor — see `~/chroma-research/2026-06-15_roscolux-picks.html`).
  Bigger hole-to-chip ratio = cleaner color read. Hole can grow to ~15 mm max before the 0.75 in
  chit stops covering it.
- **Stack depth = 3** (engine `DEPTH=3`; the old `DEPTH=4` was a stale bug, now fixed).
- **Structural:** the hex recess walls make this an egg-crate ribbed panel — static handling is
  fine (~15× margin, rough analytical estimate, NOT FEA); real risk is an impact/drop, hence
  PETG/PLA+ + fillets + frame. Print FLAT (wells up): vertical holes need no supports.
- **Color symbols NOT on the board.** The locked concentric color-symbol set (Cyan=center dot,
  Magenta=mid ring, Yellow=outer ring, R=M+Y, G=C+Y, B=C+M, Mud=full bullseye) is center-out and
  conflicts with the center hole; Muhammad chose engraved letters for the board. Symbols stay a
  chit / mass-production concern.

## Open / next steps
1. **Translucent-floor light variant** — print board in white/translucent PLA (walls opaque) so
   the ledge glows too = whole-chip illumination. Biggest remaining light win. OFFERED, not built.
2. **Bed size** — 266 mm needs a ~300 mm-class bed (Prusa XL / large Creality/Elegoo). Tiling
   declined for now; revisit if his printer can't clear it.
3. **Confirm `chit_th`** against real laminated chits once built (pouch mil rating is the variable).
4. Optional: thin floor to 1.2 mm (`floor_th`) for a shorter light tunnel.

See `[[project_chroma]]` memory for the full project history (engine, scoring, gels, hosting).
