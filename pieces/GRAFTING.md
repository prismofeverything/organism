# Grafting the connector onto a sculpt — problem, attempts, and a clean restart

> ## ✅ SOLVED (2026-05-24) — MOVE blank is 8/8; all three blanks are 8/8
>
> Everything via `uv run`; the user does all git. Full state in `memory/project_piece_bottoms.md`.
>
> **MOVE IS 8/8.** `uv run python pieces/meshlib/build.py` builds all three to `out/{EAT,GROW,MOVE}.obj`.
> MOVE now routes through **`build_move_symmetric`** (`meshlib/build.py`) — `out/MOVE.obj` IS the 8/8 blank.
> Method (the non-star, cliffed, twisted spiral): a radial-cut wedge can't resolve MOVE's near-vertical
> hub CLIFF, and a global remesh of a symmetric input breaks symmetry at the seams (orbit-snap can't fix
> the count mismatch). The win: (1) `build_piece("MOVE")` global-remesh SOURCE; (2) RE-STACK its
> horizontal slices into one exactly-3-fold "orange slice" — each slice resampled to M=192,
> `symmetrize_ring` (columns 0 & M/3 become an exact-rotation CUT) + `align_ring` (kills spiral twist);
> the cut follows the DRIFTING THROAT one point per z-level → resolves the cliff a radial cut can't
> (throat-anchor by min-radius POINT, not angle); close with wall → quarter-round fillet → miter-inset
> base (`_flat_wedge_from_arc`); (3) `_replicate3d` → full solid; (4) `_sym_remesh` = isotropic remesh
> of **sector 0 only** (`selectedonly=True` LOCKS the cut byte-for-byte) then replicate → uniform AND
> exactly 3-fold. Knobs: `MOVE_M=192`, `MOVE_WALL_Z=1.4`, `MOVE_FILLET=0.7` (arm tips pinch to ~2mm
> necks), z-spacing 0.6 (locked cut edge < 1.5 near apex), slice to `zmax-0.06`, remesh `featuredeg=65`
> (keep the crisp rim, smooth the arm ridge). EAT/GROW unchanged (8/8).
>
> **GENERALIZED ✅ (user's idea):** `build_blank(name)` in `meshlib/build.py` (`uv run python
> pieces/meshlib/build.py --blank`) is ONE shape-agnostic path — silhouette + radial profile + fold →
> 8/8 sculptable blank — that reproduces **all three** at 8/8 (EAT M=160/fillet0.80, MOVE M=303/0.68,
> GROW M=152/0.73), auto-deriving the knobs: `M` from the widest slice's perimeter, `fillet` from the
> rim's narrowest neck (`_min_neck`), and **adaptive z-levels** spaced uniformly in 3D along the cut
> (`_adaptive_zlevels` — dense through the cliff/shoulder/apex, sparse on gentle spans). It's added
> ALONGSIDE the dedicated paths (`build_piece_symmetric` collar, `build_move_symmetric`), which stay the
> default for `build.py` (no `--blank`).
>
> **CONNECTOR GRAFTED onto the new blanks ✅ (2026-05-24):** `build_graft(name)` in `meshlib/build.py`
> (`uv run python pieces/meshlib/build.py graft EAT GROW MOVE`; CLI `build.py blank <name>` /
> `build.py graft <name>`) puts the universal connector on every blank with ONE method: re-stack the
> blank body to a knee and blends the top rings to the Ø14 seat circle, with the cap method chosen by
> regime: **NESTLE** (EAT/GROW, body wider than the seat) uses a **G1 cubic-Bezier collar** per angle
> that leaves the seat HORIZONTAL (tangent to the flat seat ring -> NO shelf/ridge at the base) and
> arrives at the foot on the body's wall slope -> one continuous curve up into the connector; **FLARE**
> (MOVE, body NARROWER than the seat -> the connector necessarily juts out, can't nestle) follows the
> REAL body slices blended (smoothstep) to the seat -> a smooth flare (a per-angle Bezier here scallops
> the straddling cross-section into vertical STREAKS; forcing a horizontal seat tangent balloons the
> overhang into an undercut flange -- both avoided). Detected by `Rb.min() >= R_SEAT`. Then weld the
> structured connector SoR (peg, dims exact, never remeshed), SOLID rounded bottom (no socket; only FOOD
> stacks). Connector ridge bump = the intentional snap feature (kept). Renders: `renders/grafts_*.png`,
> `renders/MOVE_graft_crown.png`. All 3 → watertight, euler 2, single
> component, connector exact (apex=zmax, dome 4.300, seat r=7.00); relaxed graft bar. Supersedes the
> separate `build_graft.py`/`build_move_graft.py`. M must be a multiple of `lcm(fold,32)` for the
> connector belts. Renders: `/tmp/grafts_overview.png`, `/tmp/grafts_cut.png`. Original task notes below:
>
> **Done earlier:** EAT & GROW *blanks* are **8/8** (rounded sculptable bottom rims; `RIM_FILLET`
> in `meshlib/build.py`, fillet + `_stitch_rings` shorter-diagonal in `meshlib/solid.py`). Pieces are
> SOLID-bottomed by design (no socket; only FOOD stacks). The MOVE *connector graft* is done
> (`build_move_graft.py`). **MOVE blank symmetry is SOLVED:** `build_move_graft.py::build_move_blank()`
> (`uv run python pieces/build_move_graft.py blank` → `out/MOVE_blank.obj`) re-stacks the global-remeshed
> `out/MOVE.obj` into BLANK_M=96 symmetric rings → **exactly 3-fold body (0.000 mm), all BODY invariants
> pass.** That was the deep, long-standing blocker.
>
> **Task:** finish the MOVE blank to **8/8** (watertight, euler 2, exact symmetry, all 8 invariants —
> the BLANK bar is the STRICT 8). Only the rounded **non-star BASE** (bottom ~1 mm, z<0.5) remains:
> currently euler ≈ −10 with `no_overhang`/`silhouette`/`uniform_tris` failing there.
> **Exact current bug:** `symmetrize_ring` of the shapely-`buffer(-rf)` inset mis-pairs sectors (the
> buffer changes per-sector arc-length, so index `i+M/3` is no longer the 120° partner) → the inset arms
> distort outward (overhang 3.6 mm). Pick one path:
> - **(A) Finish `build_move_blank`'s base.** Keep the shapely `Polygon(rim).buffer(-rf)` inset (verified
>   correct/inside). Fix the symmetrize to pair by **sector arc-length** — find the 3 throats on the
>   buffered inset and resample each sector to M/3 (so `i+M/3` is a true partner), or buffer+sample ONE
>   wedge and replicate. Then the matched-radial-cut shared-weld base (`_flat_base_sym`, reuses the
>   fillet's `inset_idx`, cuts shared between wedges) should close to watertight.
> - **(B) Cleaner — route MOVE through `build_piece_symmetric`.** Write a non-star wedge mesher (one 120°
>   MOVE wedge meshes cleanly — CONFIRMED: centre→arc, 0 self-intersections, Triangle OK) to replace the
>   star-only `wedge_mesh_star`, so MOVE reuses the whole symmetric pipeline incl. the **already-8/8
>   rounded rim** (`build_solid_split` fillet). Caveat: `build_wedge_top` (collar/CVT) is star-only
>   (`_r_at`) — either generalize it, or use the simpler `build_piece` flow (triangulate one wedge →
>   `replicate2d` → membrane field → lift → `build_solid` with `fillet_r`, NO global remesh).
>
> Then **wire the winner into `meshlib/build.py`** for MOVE (replace the global-remesh path) so
> `out/MOVE.obj` itself is the 8/8 blank. Validate with `invariants.validate`; render the bottom to
> confirm a smooth rounded rim (`/tmp/render_rim.py` pattern; Blender `~/Downloads/blender-5.1.1-linux-x64/blender`,
> `nice -n 19 --threads 2`). Don't regress EAT/GROW (8/8). `out/MOVE.obj` is currently the 6/8
> global-remesh blank and is the restack SOURCE — keep building it first if you restack.

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
5. **Socket should print support-free.** ✅ **DONE for FOOD (2026-05-24)** — and moot for the pieces,
   which are now SOLID-bottomed (no socket; only FOOD stacks). `sor.food`'s parabolic dome cavity (a
   flat-apex 90° overhang) was replaced with a **self-supporting CONE** (`sor._cone`, walls ≥52° from
   horizontal, tangent-above the oversized peg parabola so it still clears the peg) which also serves as
   the peg lead-in. The cone tip sits a touch deeper → FOOD is ~0.3 mm taller (11.6 mm); the ridge
   groove (the snap surface) is untouched so the fit is unchanged. The ONLY remaining overhang is the
   ~1 mm flat at the groove top — forced by the universal peg's flat-topped ridge (a peg change would
   alter every mate), and it bridges trivially. Watertight (0 non-manifold). Renders:
   `renders/food_socket_xsec.png` (cross-section), `renders/food_socket_3d.png`.

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

## Status (2026-05-24): direction (F) WORKS — EAT + GROW grafted

The generative height-field-boss route succeeded, realized NOT in the old sculpt world but in a new
parametric builder `build_graft.py` (+ `meshlib/connector_field.py`), run with **uv** (`uv run python
pieces/build_graft.py EAT GROW`). The connector is a single-valued radial boss (`peg_height`), the body
flows up to a circular seat (r=`R_SEAT`=7) via a star→circle **collar** (`collar_meridian`), and the
socket is the oversized mirror (`socket_height`, `R_SOCK`=6.6) on the bottom — one watertight,
N-fold-symmetric, dimensionally-exact mesh, no boolean / no global remesh. **EAT** (peg only) and
**GROW** (peg + socket) are done. Key reframe: the strict `meshlib/invariants.py` are the **blank /
sculpting** bar; **post-graft** the bar is just *continuous + prints well* (belt/pole triangle artifacts
are acceptable). Full record: `memory/project_connector_graft_approach.md`.

## MOVE diagnosis (2026-05-24): the BLANK is fine — it's the METHOD

**Settled with numbers** (probe + `renders/MOVE_radial_collapse.png`):

- **The MOVE blank is symmetric and watertight.** `out/MOVE.obj` (built by `meshlib/build.py`'s
  non-star `build_piece` path) is watertight, euler 2, z_max 60; a 120° rotation maps it onto itself to
  **mean 0.28 / p95 0.45 / max 0.55 mm** — exactly 3-fold to within the global-remesh's triangle noise.
  So **rotational symmetry (the invariant) HOLDS for MOVE.** Nothing to fix on the blank.
- **MOVE is NOT a "star."** A *star-shaped* (star-domain) silhouette has ONE radius per angle — every
  boundary point visible from the centre, `r(θ)` single-valued. EAT and GROW are stars (ray-crossings
  min 1 / max 1; 0% multi-valued). **MOVE is not:** its 3 arms hook inward, so a radial ray crosses the
  boundary up to **3 times — 73% of all rays are multi-valued** (`r(θ)` triple-valued; throat r=6.2 <
  seat r=7). This is the user's "we have up to three" — exactly right.
- **"Symmetric" ≠ "star-shaped / single-valued radius."** MOVE is rotationally symmetric **and** non-star
  (also chiral — a pinwheel, no mirror symmetry, but that's irrelevant to the graft). **Single-valued
  `r(θ)` was never an invariant** — it's an *accidental* property of EAT/GROW that `build_graft.py`
  silently depends on.

**Where the star assumption lives** (`build_graft.py` + `meshlib/symmetry.py`): the builder parametrizes
the whole body by **radial rings** — `_radial_outline` sorts the outline by θ and `_r_at` does
`np.interp(θ → r)`, ONE r per angle; collar/wall/bottom all sweep `R0 = _r_at(th_s, r_s, TH)`. Feed
MOVE's triple-valued perimeter through that single-valued interp and it **collapses to garbage** — the red
curve in `MOVE_radial_collapse.png` (and the "11 mm rot-sym residual" the builder reports is this collapse
artifact, NOT real asymmetry). That is the ~70%-asymmetric / 11.6 mm-edge MOVE output.

**This is NOT a limit of approach (F).** The body field `z=H(x,y)` is shape-agnostic and already builds
MOVE correctly (that's how `out/MOVE.obj` exists). `build_graft.py` is a **star-only shortcut of (F)** — it
baked the boss into a *radial ring mesh* instead of into the *2D membrane field*. Fix = a body
parametrization that doesn't assume single-valued radius.

### The real sub-problem + focused survey

Strip it down: **connect the MOVE body's NON-STAR cross-section up to the CIRCULAR connector seat — smoothly,
watertight, connector kept exact.** Two parts: (i) connector boss + seat = already solved, shape-independent
solid-of-revolution (`revolution_cap`); (ii) a body+collar that **morphs a non-star, multiply-radial, C3 loop
→ a circle.** (ii) is the crux. Methods:

- **Cross-section lofting / skinning** (surfaces-from-contours): correspond points on the source loop ↔ the
  seat circle, interpolate to sweep a tube. The naïve version (correspond by arc length, interpolate
  *linearly*) **folds over** on a hooked loop — concavities self-intersect mid-morph. Needs a fold-over-free
  morph, hence:
- **Harmonic / Tutte map of the collar ANNULUS.** Map the annulus between the body wall-ring and the seat
  circle to a canonical flat annulus by a Dirichlet (Laplace) solve; the morph is the harmonic interpolation.
  By **Tutte's theorem the map is bijective (no fold-over) when the target boundary is convex** — and a circle
  is convex. This is approach (D) realized in the FEM we already own.
- **As-Rigid-As-Possible (ARAP) shape interpolation** (Alexa–Cohen-Or–Levin, SIGGRAPH 2000): interpolate the
  loop's *interior* with least local distortion → natural, fold-over-free intermediate loops; "rotates" the
  hooks instead of shrinking them. General-purpose alternative to the harmonic map.
- **Implicit / SDF blend** (approaches A/B): don't parametrize at all — union SDFs with a variable-radius
  smooth-min (or OpenVDB `csgUnion` + masked `LevelSetFilter`) and polygonize. Star-ness / valued-ness /
  chirality are all irrelevant.

### The DECISIVE constraint (measured 2026-05-24): the connector is WIDER than MOVE's body → it must FLARE → a single height field can't do it

Sliced each body at its seat height (`/tmp/probe_flare.py`; seat needs body radius ≥ R_SEAT=7 in every
direction so the connector nestles INSIDE the body like a height-field boss):

| piece | seat height | body cross-section r | verdict |
|---|---|---|---|
| EAT  | z=44.2 | **[8.2, 11.7]** ≥ 7 | body SURROUNDS seat → height-field boss works |
| GROW | z=31.7 | **[8.3, 10.5]** ≥ 7 | body SURROUNDS seat → height-field boss works |
| **MOVE** | z=55.2 | **[3.6, 5.5]** < 7 | **seat POKES OUT in every direction → must FLARE** |

This is the deeper reason MOVE is special (deeper than multi-valued `r(θ)`): EAT/GROW throats (9.5, 11.0) >
seat, so the connector tucks into the body and the lobes rise around it — a clean **height-field boss**.
MOVE's throat (6.2, narrowing to ~3.6 up top) < seat everywhere, so a Ø14 seat **juts out past the body** and
the collar must **flare outward into an overhanging mesa**. An overhang is multi-valued in z → **NOT
expressible as a single height field `z=H(x,y)`** (and forcing it via the footprint would fatten/fill MOVE's
pinched throats, destroying the shape). **⇒ Path 1 (membrane height-field boss) is geometrically ruled OUT for
MOVE.** It is the right tool ONLY when the body already surrounds the seat (EAT/GROW). The flare needs a
z-varying cross-section (a loft) or an implicit surface.

### Three concrete paths (re-ranked after the flare finding)

1. **Perimeter loft with a fold-over-free morph (the "non-star perimeter builder"). [recommended primary]** A
   loft is the natural home for the flare: stack cross-sections that **morph from the real MOVE perimeter at
   the bottom to the circular seat at the top** as z rises — that morph *is* the flared mesa, and it handles
   BOTH MOVE blockers at once (the real perimeter preserves the hooks → no multi-valued collapse; the
   z-varying section delivers the overhanging flare). Sample the real perimeter (the 824-pt loop), correspond
   it to the seat circle by arc length **anchored at the 3 symmetry rays** (keeps C3), and interpolate per-ring
   via a **harmonic-annulus** (Tutte-bijective; circle is convex → no fold-over) or **ARAP** morph — NOT linear,
   which folds over on the hooks. Weld the top ring to the structured connector SoR (exact). *Pros:* structured,
   exactly 3-fold, dependency-free, connector exact, flare native. *Cons:* the correspondence + fold-over morph
   is the bulk of the work; deep hooks stress it.
2. **Implicit SDF + masked CSG (robust fallback — GRAFTING's original rank-2).** SDF of `out/MOVE.obj` ∪
   analytic connector SDF with a **masked variable-radius blend** (k→0 in the connector zone, k>0 only in the
   skirt), polygonize (marching cubes / dual contouring). Overhang/flare/any-topology are all free in an
   implicit. *Pros:* bulletproof on the flare and the hooks; the post-graft bar (continuous + prints) is exactly
   what MC yields. *Cons:* `pyopenvdb` (or custom MC+SDF) dependency; exact dims rely on the mask hard-preserving
   the connector; irregular mesh, no exact symmetry, needs cleanup.
3. **Generative boss in the 2D membrane field (F)+(D) — *ruled out for MOVE* (kept for the record).** The
   principled height-field route that made EAT/GROW clean. Cannot represent MOVE's overhanging flare (above).
   Only revisit if MOVE's connector is redesigned to fit inside the body (e.g. a much smaller peg, or the body
   widened at the top) so the seat no longer pokes out.

**Bottom line:** don't touch the MOVE blank. MOVE needs a **loft or an implicit surface**, not a height field,
because its connector is wider than its body at the seat — go **Path 1 (perimeter loft, structured)** for a
clean exact-symmetric mesh, or **Path 2 (masked SDF)** for maximum robustness.

### SOLVED (2026-05-24): MOVE grafted via the perimeter loft (`build_move_graft.py`)

Path 1 (perimeter loft) WORKS. `uv run python pieces/build_move_graft.py` → `out/MOVE_graft.obj`:
**watertight, euler 2, exactly 3-fold** (120° rotation mismatch mean 0.001 / max 0.11 mm, away from the
bottom), connector + socket **dimensionally exact** (built as structured solids of revolution), apex 60.000,
silhouette 1.46%. Renders `cavity_MOVE_graft_{side,cut,crown,bottom}.png` — smooth spiral body → smooth flared
neck → clean connector crown; proper mirror socket underneath. The method:

- **Body** (z=0→`FLARE_Z`=46): re-stack the REAL `out/MOVE.obj` horizontal slices as M=192-point rings
  (arc-length resample + `symmetrize_ring` → exact 3-fold). Preserves the hooks; watertight by loft.
- **Twist fix (key):** MOVE is a spiral, so consecutive slices are rotated — angle-anchoring fails on the
  hooked rings (a ray hits 3×). `align_to` rolls each ring to best-match the one above (a tracking
  correspondence propagated down from the foot) → no loft twist. (Forgetting this terraced the body.)
- **Flare cap** (`FLARE_Z`→seat): blend each ring's radius toward the seat circle by a **smoothstep** weight
  → G1 at BOTH ends for free (zero slope at the foot, horizontal at the seat). The flare zone is 100% smooth
  (0 creases). MOVE's cross-section is star-shaped above z≈40, so this morph can't fold over.
- **Connector + socket**: structured SoR (`revolution_cap` from `build_graft.py`), outer/mouth ring forced to
  full M (`CONN/SOCK_TARGET`) so it shares the cap's seat ring / welds the bottom — no boolean, dims exact.
- **Base bulge** (`apply_base_bulge`): the connector is wider than MOVE's waist at BOTH ends — the socket
  (mouth Ø13.2) doesn't fit the bottom throat (Ø~12.3) either. A smoothstep base bulge fills the throats to
  R_SOCK+wall up to the socket top, tapering out by z=16 — a subtle fuller foot that houses the socket.
- **Bottom annulus** (mouth→rim): Triangle-meshed (the spiral offset + hooked rim make a forced index loft
  twist). Its interior Steiner points aren't 3-fold (the lone symmetry-metric miss, 1.19 mm, bottom-only,
  sits on the board, doesn't affect mating) — accepted under the relaxed bar. Wedge-replicate it later if
  exact bottom symmetry is wanted.

Relaxed-bar check: manifold ✓, silhouette ✓, dims exact ✓, symmetry ✓ (body); the `uniform_tris`/
`no_long_edges`/`no_overhang`(=the intended flare+bulge) fails are the accepted post-graft artifacts. Open
polish (all pieces): the self-supporting socket chamfer (constraint #5); soften the ~51 body creases at the
bulge transition (soft-max); optional exact-symmetric bottom.

## Next-session prompt (MOVE)

> Continue the Organism connector graft — **MOVE** (`pieces/`, everything via `uv run`). The graft is a
> single-valued height-field **boss** + star→circle **collar** (principle 7 generalized); `build_graft.py`
> is the parametrized builder (`SPECS`, `build(name)`, shared `revolution_cap()` for the up-peg and
> down-socket; geometry in `meshlib/connector_field.py`: `peg_height` / `socket_height` /
> `collar_meridian`, `R_SEAT`=7.0, `R_SOCK`=6.6, `SOCKET_GAP`=0.20). **EAT and GROW are DONE**
> (watertight, euler 2, exact symmetry, GROW has the socket). **Post-graft bar is RELAXED** — continuous
> + prints well; do NOT chase the 8 blank invariants. Full record in
> `memory/project_connector_graft_approach.md`.
>
> Task: graft **MOVE** (3-spiral, z_max 60, fold 3, has socket). It is NOT just a flare — `build_graft`
> produces garbage on MOVE (~70% asymmetric, 11.6 mm edges) because MOVE's spiral has inward **hooks** →
> `r(θ)` is **multi-valued** (throat r=6.21 < seat r=7; outline θ non-monotonic), and the builder's
> wall/collar/bottom (`_r_at`) assume a single-valued star. MOVE also needs an **aggressive outward
> flare** (its top is r≈3–5.5, narrower than the Ø14 connector in *every* direction → the cap must bulge
> OUT to the seat). Pick a path: **(A) non-star perimeter builder** — sample the outline by arc length
> *around the perimeter* (preserve hooks), morph perimeter→circular seat (the `pieces_v2.parametric_body_polygon`
> idea ported into meshlib); or **(B) flared cap onto the real mesh** — take `out/MOVE.obj`
> (`uv run python pieces/meshlib/build.py MOVE`), cut the spike, loft the hooked cross-section out to the
> Ø14 seat + peg, add the socket. Keep `M = fold·2^k` (MOVE 192). Judge in cross-section + a Blender
> Workbench cavity render (`render_graft_blender.py`; Blender at `~/Downloads/blender-5.1.1-linux-x64/blender`,
> `nice -n 19 --threads 2`). The user does all git ops.

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
- ARAP shape interpolation (Alexa, Cohen-Or, Levin, SIGGRAPH 2000): https://dl.acm.org/doi/10.1145/344779.344859
- Tutte/harmonic bijective parametrization (convex boundary ⇒ no fold-over): https://cims.nyu.edu/gcl/papers/campen2016bms.pdf · libigl harmonic/Tutte: https://libigl.github.io/tutorial/#harmonic-parametrization
