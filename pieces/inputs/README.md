# Design inputs (vendored)

The 2D silhouettes are the **only external input** to the piece generator. They
are vendored here so the project builds from a clean clone. Previously the code
read `~/Downloads/01_organism-elements_wyn_02-01.svg` — outside the repo — which
is why a fresh checkout could not regenerate the meshes.

## Files

| File | Role |
|---|---|
| `organism-elements_wyn_02-01.svg` | **Canonical source.** All three element silhouettes in one `viewBox="0 0 200 180"`. Byte-for-byte identical to the original artwork (`~/Downloads/01_organism-elements_wyn_02-01.svg`, by "wyn", 2025-01-07). This is the file `pieces_v2.py` parsed. |
| `eat.svg` `move.svg` `grow.svg` | Per-piece extracts, one `<path>` each. Each verified to **exactly** match the corresponding `<path>` in the canonical file. Originally `eater/mover/grower.svg` from the `elephantlaboratories` repo. Cleaner inputs for the new pipeline (no centroid-sorting needed to tell the three pieces apart). |

## Path → piece mapping (document order in the canonical file)

| index | piece | shape |
|---|---|---|
| `path[0]` | EAT  | 5-fold star |
| `path[1]` | MOVE | 3-fold spiral with inward hook curls (the non-star case) |
| `path[2]` | GROW | interlocking-ring motif |

## Coordinate notes (for whoever writes the SVG→polygon stage)

- `viewBox` is `0 0 200 180`; the SVG **y-axis points down**.
- The three pieces are laid out within the shared canvas (EAT left, MOVE lower,
  GROW right), so each silhouette must be **centered on its centroid-of-area**
  before use (vertex-mean is biased by hook-curl vertex density — see the old
  `extract_silhouettes`).
- Units are arbitrary SVG units; the generator scales each piece to the
  canonical **37 mm** footprint.

## If the artwork changes

Replace `organism-elements_wyn_02-01.svg` here (keep it the source of truth) and
re-derive the per-piece files from it.

## To confirm in the new pipeline

Rotational fold per piece. The old code used EAT=5, MOVE=3, GROW=4. Re-verify
GROW visually — `path[2]` reads like a 3-fold interlocking-ring motif, not 4-fold.
