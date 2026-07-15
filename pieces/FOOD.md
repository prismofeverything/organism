# FOOD + connector pipeline (round parts)

The **FOOD** token and the **universal connector** are *solids of revolution*: a 2D
meridian (radius, height) spun about the axis. Everything in this corner of the repo is
written in one vocabulary — `sor.py` ("solids of revolution"). This doc is the
recreate-from-scratch + how-to-print guide.

## Principles (`sor.py`)

1. **A part is a MERIDIAN** — an ordered list of `(radius, height)` points, revolved about +z.
2. **Build meridians by chaining tangent-matched primitives** (`Meridian.line_to / parabola_to /
   bezier_to / hermite_to / dish_to`). Each starts where the last ended *and matches its tangent*,
   so continuity is automatic.
3. **Continuity is a checked invariant** — `Meridian.hard_edges()` flags any tangent break >12°.
4. **The connector is one parametric spec** (`graft_connector.py`): a peg (dome+ridge) on top, a
   matching socket below. Fit is a single number — `FOOD_GAP` (+ = clearance/slip, − = interference/snap).
5. **Measure, don't eyeball** — `sor.nest_offset()` / `stack_food.py` (nesting), `manifold_report()`
   (printability), wall thickness (injection-molding).
6. **Two render paths** — `sor.preview()` / `render_food.py` (CPU, matplotlib, fast) for iterating;
   Blender EEVEE (`build_play_real.py`) for the final scene + video.
7. **Build the whole part as ONE meridian → one revolve is watertight.** Booleans (Blender EXACT)
   *shatter* the mesh (renders fine, but non-manifold → not printable). A single revolve never does.
   FOOD does this: `sor.food()` puts the peg dome+ridge and the socket cavity *in the meridian*, so
   `build_food.py` revolves once → **0 non-manifold edges, STL euler 2** (print-ready, no repair).

## The FOOD shape (current)

A shallow parabola flaring **up** out of the connector to a rounded rim — a meniscus. Nests
**on the connector** (the flares clear): ~38% per piece. Ø28.0 × 11.6 mm (socket dome cavity is a
self-supporting cone — prints support-free; see Printing). Knobs in `build_food.py`:
`FOOD_R` (shoulder radius), `FOOD_FLAREUP` (rim rise), `FOOD_WALL` (invariant IM wall, 2.5 mm),
`FOOD_GAP` (connector fit), `FOOD_OUT` (output path).

## Files

| file | role |
|---|---|
| `sor.py` | the shared library: `Meridian` algebra, continuity, `nest_offset`, `preview`, `revolve`, `export` |
| `build_food.py` | the FOOD spec → `sor.food()` → ONE revolve → OBJ + STL (Blender; no booleans) |
| `graft_connector.py` | the universal peg/socket (shared with EAT/MOVE/GROW) |
| `stack_food.py` | mesh-accurate nesting metric (shapely; reads the `.profile.json` sidecar) |
| `render_food.py` | CPU 3D + cross-section preview of a built OBJ |
| `build_play_real.py` | the gameplay scene + animation → PNG frames → mp4 (uses the new food) |

## Recreate on a new machine

In the repo (git has the scripts), you also need these **external** pieces:

1. **Blender 5.1.x** (portable build ok). Set `BLENDER=...` for the Makefile.
2. **The venv** (CPU side, managed with uv): `uv sync` at the repo root (reads `pyproject.toml` + `uv.lock`, creates `.venv`). The Makefile's `PY=../.venv/bin/python` then points at it.
3. **ffmpeg** (video encode): system package.
4. **Prototype art folder** for the video — `~/Downloads/organism/prototype` (the `ART` path in
   `build_play_real.py`) + `pieces/board_hex_2000.png`. **This is the stuff that gets lost** —
   it lives outside the repo. Copy it across, or commit it.
5. **Game data**: `ogf/zach-dan-ryan.json` (already in the repo).

## Run (Makefile in `pieces/`)

```sh
make food      # build both FOOD_nosnap.obj + FOOD_snap.obj (+ .profile.json sidecars)
make measure   # nesting % (stack_food)
make preview   # CPU 3D + cross-section
make video     # full gameplay animation -> scene/zach-dan-ryan-play-newfood.mp4
make stl       # print-ready STL export + manifold report
```

## Printing

`make food` writes `renders/food/FOOD_{nosnap,snap}.stl` (units = **mm**), now **watertight
(0 non-manifold, euler 2)** — slice directly, no repair needed.
- Slice in Cura/PrusaSlicer (FDM) or Lychee/Chitubox (resin).
- Orient **peg up / socket down**. The socket now prints **support-free by design**: the dome cavity
  is a **self-supporting CONE** (`sor._cone`, walls ≥52° from horizontal — replaced the old parabolic
  cavity whose flat apex was a 90° overhang), and the cone doubles as a peg lead-in. The cone is also
  the reason FOOD grew ~0.3 mm taller (a pointed tip sits a touch deeper than the flat-apex dome). The
  **only** remaining overhang is the ~1 mm flat at the top of the **ridge groove** — forced by the
  universal peg's flat-topped ridge (changing it would change every piece's mate), and it bridges
  trivially. So you generally **don't** need to block supports; if your slicer still adds any, block
  just that little groove. Still support the **flared rim** overhang from the build plate. Print
  **both** fits and feel the snap vs slip — the −0.10 mm interference is a starting guess, confirm by hand.
- Snap material wants a little give (PETG / tough resin).

### Full test plate (`make plate`)

`make plate` lays out a ready-to-slice plate — **3 slip + 3 snap food + 1 each piece** — dropped to the
bed in a 3×3 grid (~117 mm square, fits a 220 or 256 mm bed), exported as ONE STL:
`renders/food/print_plate.stl` (+ `print_plate_top.png` layout preview). All 9 bodies watertight.
- The **pieces are tall** (EAT 51, MOVE 55, GROW 38 mm) — supports for the overhangs + a brim for
  adhesion; food (11 mm) is quick. Slice the food row alone first to test fit/stacking fast.
- **MOVE** prints as a real sculpted spiral — its genus-3 mesh is the actual hooks/holes (*not* the
  gap-filled parametric *placeholder*, which would be a genus-0 blob). The pure-parametric *recipe* for
  MOVE is a separate open problem (README `## Status` / `pieces_v2.py`); the built body is fine.
- Counts/spacing live at the top of `build_print_plate.py` (or `PLATE_CELL=..`, `PLATE_OUT=..`).

## Open items

- ~~Watertight FOOD~~ — **done** (principle 7: connector is in the meridian, one revolve, 0 non-manifold).
- **Thin-wall shell** for IM (the body is still solid; ~5 mm mid-flare = minor sink). The single-meridian
  approach makes this tractable — add an offset inner wall to the meridian.
- **Snap fit** is a test-print call; `FOOD_GAP` is the dial (built: +0.15 slip, −0.10 snap).
