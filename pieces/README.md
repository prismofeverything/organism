# Organism board-game piece generator

Generates four sculpt-ready 3D pieces — **EAT**, **MOVE**, **GROW** (player
pieces) and **FOOD** (stackable disc) — as OBJ meshes from a single SVG of 2D
silhouettes. Each piece is built by a parametric construction designed to
satisfy six topology invariants simultaneously (see
`pieces_v2.py` module docstring and `invariants.py`).

## What you need (clone-from-scratch)

1. **Blender 5.1.x** — the script runs inside Blender's bundled Python and uses
   `bpy` / `bmesh` / `mathutils`. There is **nothing to `pip install`**; all
   dependencies ship with Blender.
   - During development: `~/Downloads/blender-5.1.1-linux-x64/blender` (Linux
     portable build). Any 5.1.x build on any OS works.

2. **The silhouette SVG** at the path in the `SVG` constant near the top of
   `pieces_v2.py`:
   ```
   ~/Downloads/01_organism-elements_wyn_02-01.svg
   ```
   This is the ONLY external input. It contains three filled curves (EAT, MOVE,
   GROW silhouettes); the script imports it, converts curves to meshes, walks
   each outline, and uses them as the piece footprints. FOOD is purely
   parametric (a circle) and needs no SVG input. If your SVG lives elsewhere,
   edit the `SVG` constant.

3. **(Optional) ImageMagick** (`convert`) — only used by ad-hoc diagnostic
   scripts that turn raycast `.pgm` masks into `.png`. The main pipeline does
   not need it.

## Run

```sh
blender --background --python pieces_v2.py
```

(Substitute your Blender binary path.) Everything is written next to the script:

| Output | What it is |
|---|---|
| `EAT.obj` `MOVE.obj` `GROW.obj` `FOOD.obj` | the meshes |
| `connector_meta.json` | per-piece connector placement for `graft_connector.py` |
| `pieces.blend` / `pieces.glb` | combined viewing scenes (drag the GLB into any glTF viewer) |
| `renders/*.png` | workbench renders + topology-audit diagnostics |

## Validate the invariants

```sh
blender --background --python invariants.py -- EAT.obj
```

Runs every registered invariant against the mesh and prints a pass/fail report.
Omit the filename to check the three main pieces (EAT, MOVE, GROW). FOOD is
excluded — it's already designed (shallow parabolic dish + bottom dome) and
isn't a tapering single-apex body, so the six body invariants don't apply.
See `invariants.py` for how to add a new invariant (drop a function into the
`INVARIANTS` registry).

## Status

- **EAT, GROW, FOOD** — satisfy all six invariants.
- **MOVE** — UNSOLVED. Its spiral-with-hooks silhouette is a non-star polygon;
  uniform-scaling extrusion fills the spiral gaps in the top-down view
  (invariant #2). A new construction method is needed — see the "OPEN PROBLEM"
  section in the `pieces_v2.py` module docstring. We will not compromise the
  invariants; MOVE stays in its current placeholder state until the new method
  exists.

## Post-sculpt

After sculpting a body in Blender, re-attach the universal connector with:

```sh
blender --background --python graft_connector.py -- --in sculpted.obj --piece MOVE --out final.obj
```
