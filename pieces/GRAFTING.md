# Grafting the connector onto a sculpt — problem, attempts, and a clean restart

The universal connector (peg dome+ridge on top, mirror socket below — `graft_connector.py`, with
*exact* mating dims) must sit on top of each player piece (EAT star, MOVE spiral, GROW clover).
**FOOD** solved this by building the connector INTO its meridian, so one revolve is smooth and
watertight ("principle 7", see `FOOD.md`). The **pieces are sculpts**, not solids of revolution, so
we've been *grafting* the connector on afterward — and that graft is where the lumpiness lives. This
doc records why, surveys the literature, and sets up a clean next attempt.

## The job (constraint hierarchy)

1. **Connector mating surfaces are SACRED.** The peg (Ø3.8 dome × 4.3 mm, Ø8.3–12.8 ridge × 2.75 mm)
   and the mirror socket cavity must stay dimensionally exact or food/pieces stop interlocking. Any
   blend MUST avoid rounding these.
2. **Body preserved** — keep the sculpt (star / spiral / clover) "as intact as possible".
3. **ONE continuous surface in the transition** — G1 (tangent-continuous) from body → connector seat.
   No flat shelf, no sharp rim, no jagged tear, no overhang cliff, no blob.
4. **Watertight + printable** (0 non-manifold ideally), connector seated ON TOP.
5. **Socket should print support-free.** Printed socket-down, the socket cavity overhangs and the
   slicer jams supports into the ridge groove (miserable to remove). The groove ceiling is only ~2 mm
   so it *bridges* fine if supports are blocked — but the connector rework should make the socket
   self-supporting by design: a **45° lead-in chamfer at the mouth** (also eases assembly) + a
   **sloped/steep groove & dome ceiling (≤45° overhang)**, keeping the seating walls/depth so the mate
   is unchanged. Apply to BOTH the food socket (`sor.food`) and the pieces' socket (`graft_connector.add_socket`).

The tension is (3) vs (1)+(2): the connector is a **circle**; the body cross-section where they meet
is **non-circular and varies** (star points, spiral arms). A clean merge must morph circle↔outline
*smoothly* while keeping the connector crisp. That morph is the whole problem.

## What we tried (and why each lumps)

| approach | how | result | render |
|---|---|---|---|
| **Boolean union** (default `graft_connector.py`) | truncate body to a flat Ø14 plateau, union the peg | **sharp shelf + hard rim**; chops the natural crown | `renders/xsec_EAT_connected.png` |
| **Voxel-remesh fuse** (`--fuse`) | flat plateau + peg, then voxel remesh (0.3 mm) + smooth the rim band | rounder, but **smears detail, blobby/torn seam**; EAT 0 / MOVE+GROW 22 non-manifold | `renders/closeup_EAT_fuse_MOVE_fuse.png` |
| **Seamless crown** (`--seamless`, this session) | cut just under the connector, union a revolved domed-crown meridian (`sor`), then remesh | cross-section profile is G1, BUT the crown cylinder **coincides with the star/spiral tips → EXACT boolean tears**; remesh renders it **jagged + (MOVE) a cliff** | `renders/crown_closeup.png` |

Cross-sections (one slice) looked fine; the **3D surface** is where the star↔circle mismatch shows as
lumps/tears. All three are *post-hoc grafts*: a circular feature booleaned/remeshed onto a
non-circular sculpt cannot avoid a seam artifact.

**Bugs fixed en route (keep):** `sor.revolve` did a **multi-object Edit-mode spin** that revolved the
*body* into 1.75 M verts — fixed with a `select_all(DESELECT)` before Edit (`sor.py`). And the
seamless cut-height now cuts where the body ≈ connector width, not at the wide star tips.

## Literature survey — merging two surfaces smoothly

**(A) Smooth-minimum SDF union (`smin`).** Represent body + connector as signed-distance fields and
combine with a polynomial *smooth minimum* instead of `min` → C1/C2 blend "for free" (the
demoscene/CSG trick). *Catch:* a global `smin` rounds BOTH shapes near the seam → softens the
connector. *Fix:* **variable blend radius** `k(x)` — `k→0` over the connector's functional zone,
`k>0` only in the skirt.

**(B) Level-set CSG + masked smoothing (the VFX "merge").** Convert both to OpenVDB narrow-band level
sets, `tools::csgUnion` (robust, no coincidence/tearing), then `LevelSetFilter` (mean-curvature flow /
one-pass fillet) applied **only in the seam band** via a mask, so the connector — and the body away
from the seam — stay put. Polygonize back. Python via `pyopenvdb`. **The most robust,
directly-implementable principled graft-after route.**

**(C) Rolling-ball / variable-radius fillet (CAD).** The exact fillet = the envelope of a ball rolling
in the concave seam keeping two-point tangency. Beautiful on B-reps, hard on triangle soup — but a
good *definition* of the target blend even if we realize it implicitly.

**(D) Variational / biharmonic membrane blend.** Solve a biharmonic (Δ²=0) or membrane surface in the
transition annulus with **Dirichlet** (positions on the connector base ring + a body ring) and
**Neumann** (tangents) boundary conditions → the minimum-bending smooth blend. **We already own this
machinery** — `meshlib/` builds the piece bodies with a cotangent-FEM membrane / height field.

**(E) Loft / Coons–Gregory collar.** Explicitly loft a "collar" from the body's cross-section ring up
to the connector's base circle with tangent continuity at both ends (n-sided Gregory/Coons patch to
morph outline→circle). The most direct control of the star→circle morph.

**(F) Generative integration — don't graft at all (FOOD's principle 7, generalized).** Bake the
connector into the body's *construction* so the blend is inherent. In `meshlib` the body is a 2D
region lifted by a height field over a FEM membrane; add the connector as a **boss in that height
field / SDF**, and the membrane blends it smoothly by construction — never a post-hoc seam. Cleanest,
most "principled", and reuses the pipeline that already passes the six invariants.

## Recommended next attempt (ranked)

1. **(F) Generative** — express the connector as a height-field/SDF boss inside `meshlib`. No seam
   ever, smooth by construction, mirrors what made FOOD clean.
2. **(B) Level-set CSG + masked smoothing** — robust graft-after fallback; preserves the connector
   exactly (mask it), smooths only the skirt, no tearing. `pyopenvdb`.
3. **(D) Biharmonic collar** in the existing FEM if (B) isn't available.

**Through-line principle:** *the connector's mating surfaces are a hard constraint; the blend must be
region-localized — crisp connector, smooth only the skirt.* Every method above works only if the
blend is masked/weighted to leave the connector alone. The global voxel remesh ignored this and that
is exactly why it lumps.

## Next-session prompt

> We're designing the *Organism* game pieces (`pieces/`). FOOD is solved — its connector is built into
> a single solid-of-revolution meridian (`sor.py` / `build_food.py`, watertight, "principle 7"). The
> three player pieces (EAT/MOVE/GROW; sculpted bodies `EAT.obj`/`MOVE.obj`/`GROW.obj`, heights
> 48/60/36 mm) need the same universal connector (`graft_connector.py` dims: Ø3.8 dome×4.3, Ø8.3–12.8
> ridge×2.75, mirror socket) seated on top — but they're organic sculpts (star / spiral / clover), not
> solids of revolution. **Every post-hoc graft we've tried (boolean, voxel-fuse, revolved-crown union)
> leaves a lumpy / torn / cliffed seam** where the circular connector meets the non-circular body. The
> full record + renders + a methods survey are in `pieces/GRAFTING.md`.
>
> Goal: a clean, *principled* merge — body preserved, connector mating surfaces exact, ONE
> G1-continuous surface flowing from the body up to the connector seat, watertight.
>
> Start from `pieces/GRAFTING.md`'s survey. Preferred direction: **(F) integrate the connector into the
> body's generative `meshlib` construction** (a boss in the height field / SDF, blended by the FEM
> membrane — the principle-7 analog); failing that, **(B) OpenVDB level-set CSG + seam-masked
> smoothing** (`pyopenvdb`) so only the skirt blends. While the connector is open, also deliver the
> **self-supporting socket** (constraint #5 — 45° mouth chamfer + ≤45° cavity ceilings, applied to both
> `sor.food` and `graft_connector.add_socket`, seating walls unchanged). First step: prototype on **EAT only**, judged in
> cross-section AND a 3D cavity render (`render_crown_closeup.py`), before touching MOVE/GROW. Keep
> renders light (`nice -n 19 --threads 2`) and ALWAYS print the output path.

## File inventory (this session)

- `graft_connector.py` — added `--seamless` (`add_seamless_crown`): the revolved-crown attempt; kept
  for reference (its boolean tears on the star/spiral tips).
- `sor.py` — `revolve()` multi-object-edit bug fixed (the body-spin).
- `prototype_seamless.py` — meridian prototype overlaid on the current graft (CPU).
- `xsec_piece.py` — cross-section any piece OBJ (CPU, trimesh).
- `render_pieces_3d.py` / `render_crown_closeup.py` — Workbench+cavity 3D renders (cavity = creases show dark).
- `check_manifold.py` / `debug_seamless.py` — Blender bmesh manifold count / stage-by-stage debug.
- Experiment outputs: `renders/{EAT,MOVE,GROW}_seamless.obj`, `renders/{EAT,MOVE,GROW}_fuse.obj`.
- **Production pieces UNCHANGED:** `EAT/MOVE/GROW_connected.obj` are still the default (sharp) graft —
  NOT overwritten. The print plate and the movie still use those.

## Sources

- Inigo Quilez — smooth minimum: https://iquilezles.org/articles/smin/
- OpenVDB `tools` (csgUnion, LevelSetFilter): https://www.openvdb.org/documentation/doxygen/namespaceopenvdb_1_1v13__0_1_1tools.html · https://www.openvdb.org/documentation/doxygen/LevelSetFilter_8h_source.html
- Várady — A survey of blending methods that use parametric surfaces: https://www.academia.edu/3392234/A_survey_of_blending_methods_that_use_parametric_surfaces
- Rolling-ball fillet (patent US5774359A): https://patents.google.com/patent/US5774359A/en · Variable-radius blend: https://link.springer.com/article/10.1007/BF02434038
- Biharmonic fields & mesh completion: https://www.researchgate.net/publication/282600011_Biharmonic_fields_and_mesh_completion · https://arxiv.org/abs/1707.06567
- Laplacian mesh editing (Princeton COS526): https://www.cs.princeton.edu/courses/archive/fall10/cos526/lectures/08-laplacian.pdf · libigl: https://libigl.github.io/tutorial/
- Analytical C² continuous surface blending: https://www.mdpi.com/2227-7390/12/19/3096
- Rhino ShrinkWrap (watertight wrap/union): https://www.rhino3d.com/features/shrinkwrap/
