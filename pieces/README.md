# Organism piece pipeline

Generates the print-ready meshes for the **EAT**, **MOVE**, **GROW** player
pieces and the **FOOD** stackable disc, with the universal connector grafted
on. The whole pipeline is driven by `make`.

## One-shot

From this directory:

```sh
make print
```

End-to-end: builds the 3 blank bodies, grafts the universal connector onto
each, builds the slip + snap FOOD STLs, and lays them all out on a combined
print plate. Output for the slicer:

```
renders/food/print_plate.stl
```

## Targets

```sh
make             # list targets
make print       # end-to-end -> slicer-ready print_plate.stl
make blanks      # 3 sculpt-ready blank bodies in out/
make grafts      # 3 connector-grafted pieces in out/ (needs blanks)
make food        # slip + snap FOOD STLs in renders/food/
make plate       # combined print plate STL (needs grafts + food)
make all         # rebuild everything (skips video)
make clean       # nuke out/ blanks+grafts and print_plate.stl
make measure     # FOOD nesting % measurement
make preview     # CPU 3D + cross-section render of FOOD
make video       # gameplay animation -> scene/*.mp4
```

Each stage is incremental — running `make print` after `make grafts` only
rebuilds what's stale.

## Where things land

| Path | What |
|---|---|
| `out/{EAT,MOVE,GROW}.obj` | sculpt-ready blank bodies (built by `meshlib/build.py`) |
| `out/{EAT,MOVE,GROW}_graft.obj` | blanks with the universal connector grafted on |
| `renders/food/FOOD_{nosnap,snap}.{obj,stl,profile.json}` | the two FOOD fits |
| `renders/food/print_plate.stl` | **the slicer input**: 3× slip + 3× snap FOOD + 1× each piece |
| `renders/{blanks,grafts}_overview.png` | auto-rendered overview shots (fire-and-forget) |

Both `out/` and `renders/` are gitignored by default.

## Dependencies

- **Blender 5.1.x** for FOOD revolves, the plate layout, and auto-render.
  Default path is `~/Downloads/blender-5.1.2-linux-x64/blender`. Override:

  ```sh
  make print BLENDER=/path/to/blender
  ```

  The Makefile exports `BLENDER` to `meshlib/build.py` so both agree on one
  binary.

- **`uv`** for the core mesh pipeline (blanks + grafts run under the
  repo's `.venv`, no Blender needed). `uv sync` from the repo root creates
  the venv from `pyproject.toml` + `uv.lock`.

## Validate the invariants

`meshlib/invariants.py` runs the eight blank-bar invariants (manifold,
silhouette, no overhang, uniform tris, smooth, additive, symmetry,
profile) against any OBJ. The CLI in `meshlib/build.py` runs them
automatically after each blank/graft build and prints a pass/fail
report. Grafts are checked under a **relaxed bar** (continuous +
prints — belt/pole artifacts allowed). The strict 8-invariant bar is
the **blank** standard.

## Architecture pointers

- **`meshlib/`** — the modern build pipeline. `build.py` is the CLI;
  `connector_field.py` is the universal connector spec (peg dome,
  ridge, socket cavity); `graft_lib.py` is the ring-loft mesh primitive;
  `symmetry.py`, `solid.py`, `collar.py`, `field.py`, `domain.py`,
  `mesh2d.py`, `remesh.py`, `profile.py`, `invariants.py` are the rest.
- **`FOOD.md`** — the FOOD token: solid of revolution with the
  connector built into the meridian (principle 7). `sor.py` is the SoR
  library; `build_food.py` is the Blender entry point.
- **`GRAFTING.md`** — the full graft-attempt journey, the survey of
  blending methods (smin / OpenVDB / biharmonic / loft), and why the
  current generative approach won.
- **`DECISIONS.md`** — design lab notebook (append-only).

## Per-piece notes

| Piece | Fold | z_max | Socket | Method (meshlib/build.py) |
|---|---|---|---|---|
| EAT  | 5 | 48 | no  | star, height-field boss + collar (NESTLE) |
| GROW | 4 | 36 | yes | clover, height-field boss + collar (NESTLE) |
| MOVE | 3 | 60 | yes | spiral, perimeter loft + flare cap (FLARE) |

MOVE was the hard one: its connector seat (Ø14) is wider than the
body's top cross-section, so the cap must flare outward. The height-field
boss that works for EAT/GROW can't represent that overhang, so MOVE uses
a perimeter-loft path that morphs the real hooked cross-section to the
circular seat. Details in `GRAFTING.md`.
