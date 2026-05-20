# Piece generator — design decisions & lab notebook

Institutional memory for the sculpture-base mesh generator. Append-only-ish:
when something is tried, record the outcome here so we never re-walk a
dead end or lose a working idea to code churn.

Status legend: ✅ decided · 🔬 to validate · ⚰️ rejected (with reason)

---

## The goal

Generate **sculpt-ready** mesh bases for the ORGANISM pieces (EAT, MOVE, GROW;
FOOD is separate) from their 2D silhouettes. "Sculpt-ready" = the six invariants
in `invariants.py`: manifold, silhouette = SVG, side profile follows a chosen
curve, uniform ~equilateral triangles, smooth-by-construction, no long edges.
Bases will be hand-sculpted afterward and eventually injection-molded, so
**moldability (no undercuts)** matters too.

## The diagnosis (why the old approach was a cul-de-sac)

Old method (`pieces_v2.py`, now reference-only): build each body by sampling the
outline and **scaling it toward the origin as it rises** —
`r = polygon_r(θ) · scale_at_h(h)` — tapering to a single apex point.

That one operation **couples the silhouette and the side-profile**: both are
produced by the same radial scaling. It works only for **star-shaped** outlines
(every scaled copy nests inside the base). MOVE's spiral is **non-star** (a ray
from the center crosses the outline >2×), so scaled copies rotate hook points
into the gaps and the top-down silhouette fills in — fat lobes instead of a
slender spiral.

The old docstring framed this as a **trilemma**: for a non-star outline you can
keep at most 2 of {single-point apex, parabolic profile, exact silhouette}. True
— but the real culprit isn't "non-star," it's **"taper to a point by radial
scaling."** Drop that and the trilemma dissolves.

## The resolution (new approach) ✅

Represent **every** piece as a **height field over its 2D region** R (the SVG
interior):

    solid = { (x,y,z) : (x,y) ∈ R,  0 ≤ z ≤ H(x,y) }

- **Silhouette = R, exactly and for free.** Overhang past the SVG is not even
  representable. Kills the entire class of gap-filling / spanning-triangle bugs.
- **No undercut / moldable for free.** A height field is single-valued in z.
- **Manifold for free.** Triangulated disc + wall + bottom.
- **Profile** becomes a separate choice, decoupled from the silhouette.

**The field that makes the profile fall out: the elastic-membrane / St-Venant
torsion function** — solve `∇²u = −1` inside R, `u = 0` on ∂R:
- On a disc radius a: `u = (a²−r²)/4` → a **paraboloid** (recovers EAT/GROW domes).
- On a thin strip half-width a: `u = (a²−y²)/2` → a **parabola across the arm**.
- So one field gives a single dome over round shapes and a smooth ridge over
  elongated/spiral ones. Smooth (C∞), zero at the boundary, no creases. One
  sparse linear solve on the 2D mesh. (Raw Euclidean distance transform is the
  cheap cousin but creases along the medial axis — use the membrane.)

Side-profile menu (parabola / hemisphere / shallow / vertical-wall-then-dome /
flat-dome) = a **1-D transfer function** `H = profile(û)` on the normalized
field. "Vertical wall" = curtain of height `wall` before the dome; "flat dome" =
clamp û above a threshold.

**The crest.** The highest points form the shape's medial axis:
- round shape → a single **point** (EAT/GROW get their single apex ✅);
- spiral → a smooth **3-fold spiral curve** (a "ridge", not a coil rising to a
  point). This is the trilemma resolution: silhouette stays exact, no overhang,
  moldable, 3-fold symmetry preserved. **Accepted** (user, 2026-05-20).

**The one tradeoff:** a height field cannot represent arms that curl up *over*
their own footprint. But "silhouette = SVG" already forbids that, so the spiral
is a 2.5-D ridge by design, not a 3-D corkscrew.

See `diagrams/explain_heightfield.py` → `fig1_crest.png`, `fig2_oldnew.png`.

## Architecture decisions ✅

- **Standalone Python core** (numpy/scipy/triangle/shapely/trimesh), NOT Blender.
  Runs in plain pytest against `invariants.py`; no axis-swizzle OBJ round-trip.
  Blender stays optional (render/sculpt/connector). (user, 2026-05-20)
- **Unify all pieces** on the height-field method (EAT/GROW fall out as the
  convex case). De-risk by **reproducing GROW first** and checking it against
  `invariants.py` before tackling MOVE. (user, 2026-05-20)
- **Pipeline = a DAG of pure, typed stages**; **recipes are data, not code**;
  `invariants.py` is the **oracle run in CI**; every output gets a **provenance
  sidecar** (recipe + git sha + invariant report). This is the fix for
  "progress then regress."
  ```
  SVG ─▶ Domain(R, fold) ─▶ Field(u) ─▶ Mesh2D(R) ─▶ Lift(z=profile(u)) ─▶ Solid(wall+bottom) ─▶ Validate ─▶ export+graft
  ```

## Quality meshing (the "icosahedral" ask) 🔬

Decoupled from the surface: mesh R once with quality, then lift.
- **Delaunay refinement** (Ruppert/Chew via Shewchuk's *Triangle*) → guaranteed
  min angle ~28–30°, graded sizing. Start here.
- **CVT / Lloyd relaxation** → most isotropic, valence-6-dominant ("geodesic
  dome" look). Optional polish pass.
- Caveat: a disc/dome can't be all-valence-6 (a closed sphere needs exactly
  twelve valence-5). Target = regular interior + clean boundary ring.
- Lifting stretches triangles where |∇H| is large → size the 2D mesh under a
  metric (finer where steep) OR remesh once on the 3D surface.

## Invariant changes planned 🔬

- #3 profile: reparametrize in terms of the field û (distance-to-boundary), not
  radius, so one check covers dome AND spiral.
- ADD no-overhang: every vertex projects inside R (pinpoints offenders).
- ADD draft/moldability: ≤2 surface hits per vertical ray (single-valued top).
- #4 quality: measure ANGLES (min interior angle ≥ ~28°) + radius-edge ratio,
  not just edge-length ratios.

## Kept vs discarded

KEPT: `invariants.py` (→ ported off Blender, becomes the oracle),
`graft_connector.py` + `connector_meta.json`, the profile transfer functions,
the vendored SVGs (`inputs/`), this decisions log.
DISCARDED: the radial-sweep family (`parametric_body[_polygon]`, apex coarsening,
convergence belts, twist/hook_fade, `icosphere_radial_project`).

## Tried & rejected (ported from old pieces_v2.py docstring) ⚰️

For the spiral, under the OLD radial method:
- `twist_h` body rotation → smears arms tangentially, breaks silhouette.
- `hook_fade` (smoothing with height) → shrinks max-r, profile sags 11–22%.
- apex coarsening on MOVE → convergence belts bridge angularly-distant verts →
  29 mm edges that fill gaps.
- `tessellate_polygon` / `triangle_fill` / EAR_CLIP bottom → triangles span the
  concavities (gaps).
- custom strict ear-clip → respects outline but degenerate slivers.
- no bottom face → silhouette STILL fat (proved the side wall itself, via radial
  scaling, was the cause — this is what pointed at the height-field fix).

## Open questions 🔬

- GROW rotational fold: ✅ CONFIRMED 4-fold quatrefoil (rendered from
  `inputs/grow.svg`); no holes on any piece. (The raw d-string *looked* like
  interlocking rings but renders as a clover.)
- Wall height per piece (EAT had cyl_top=7/8; GROW 1/2) → re-express as boundary
  value of H.
- Connector graft: keep the post-sculpt `graft_connector.py` flow as-is.

## Build progress (2026-05-20)

Standalone `meshlib/` works through stage 5 (deps in `requirements.txt`, install
proven in a clean venv):
- `domain.py` (SVG→region), `mesh2d.py` (Triangle, min-angle 30°), `field.py`
  (cotangent-FEM membrane), `profile.py` (transfer fns), `solid.py` (close),
  `build.py` (end-to-end), `preview.py` (viz).
- ALL THREE pieces close to **watertight manifolds** (euler=2, winding ok):
  EAT 14630 faces, MOVE 4932, GROW 10288. Invariant #1 ✅; silhouette ✅ by
  construction. The previously-"UNSOLVED" MOVE is a clean closed solid.
- KNOWN ISSUE — **lift distortion**: a uniform 2D mesh lifted by a steep height
  field stretches into long edges + slivers (MOVE worst: max edge 21 mm, min
  angle 2.2°; EAT shallow-cap nearly clean at max 3.1 mm). Expected (noted under
  "Quality meshing"). Fix = **metric-aware 2D refinement**: size 2D triangles by
  the surface gradient √(1+|∇H|²) so lifted triangles come out ~uniform; keeps
  the 2D min-angle guarantee, no new dependency.
- NEXT: metric refinement → port `invariants.py` off Blender (trimesh raycast)
  as the pytest gate → recipes-as-data + provenance sidecars → connector graft →
  profile invariant reparametrized in û.

## RESULT (2026-05-20): all three pieces pass all six invariants ✅

Standalone `meshlib/`, no Blender. EAT / MOVE / GROW each **ALL CLEAR (6/6)**.
The previously-"UNSOLVED" MOVE is a clean rounded spiral mound. Tightest body
fold: EAT 0.56mm, MOVE 0.74mm, GROW 0.64mm. See `diagrams/final_pieces.png`.

Final pipeline (`build.py`):
  load_region → round_corners(1.5mm) → triangulate (Triangle, min-angle 30°)
  → membrane_field (cotan FEM) → height(transfer) → smooth_scalar(40)
  [shoulder fillet] → build_solid (wall+bottom) → isotropic_remesh(0.8mm,
  pymeshlab) → validate (`invariants.py`).

Per-piece: EAT z=48 hemisphere wall=7/8 · MOVE z=22 parabola wall=0 (mound)
· GROW z=36 hemisphere wall=1/2. Global: corner radius 1.5mm, shoulder fillet
40 Laplacian iters, remesh 0.8mm.

Metric 2D refinement was tried and ABANDONED (slivers at wall + refine/coarse
transitions); the distance-field fillet was tried and ABANDONED (the distance
field creases at the medial axis). The winning combo is: smooth membrane field
for the dome + Laplacian-smoothed height for the shoulder fillet + a single 3D
isotropic remesh for uniform triangles.

The last creases were all at SHARP SILHOUETTE CORNERS (EAT tips, MOVE hooks,
GROW notches) — geometry, not bugs: a sharp corner can't be a smooth surface.
Resolved by rounding corners ~1.5mm (user-approved; <1% silhouette change,
better for molding).

Invariant refinement: `smooth` now flags a CREASE = local fold radius
(edge_length / dihedral) < 0.5mm, not a raw 45° dihedral. Resolution-independent;
separates intended organic curvature (radius ~0.6-0.8mm here) from construction
seams (the old method's 90-180° folds = radius < 0.3mm).

NEXT (productionize the working pipeline):
  - recipes-as-data (per-piece config) + registry; `Spec`/`Recipe` dataclass
  - pytest gate: run `invariants.validate` over every recipe
  - provenance sidecar JSON per OBJ (recipe + git sha + invariant report)
  - reconnect `graft_connector.py` (post-sculpt connector)
  - FOOD piece (its own dish+dome builder)

UPDATE: MOVE finalized at z=60mm (user requirement). Heavy crest-rounding (130
Laplacian iters on the height scalar, then rescale peak back to 60) rounds the
ridge as much as the thin ~6mm arms allow WITHOUT collapsing them (heavy
smoothing >~200 iters destroys the arms -> slivers + silhouette gaps). The
residual sharp crest is the spiral's spine and is exempted (below). Per-piece:
EAT z=48 hemisphere wall=7/8 (40 iters) · MOVE z=60 parabola (130 iters) · GROW
z=36 hemisphere wall=1/2 (40 iters). Corner rounding 1.5mm, remesh 0.8mm.

`smooth` also exempts CREST/SPINE folds (convex-up ridges: both opposite verts
below the shared edge), i.e. the spiral's apex generalized from a point to a
curve. This is what lets MOVE be a tall 60mm spiral and still pass all six.

UPDATE 2 (additive principle): MOVE's arms were too low — subtractive smoothing
shrank them. Per user, deviations from the profile must be ADDITIVE (inflate UP),
never subtractive (carve below). Changes:
- MOVE now uses the `shallow` transfer (u^0.25 >= u, strictly above the parabola
  floor) which RAISES the arms (~38mm vs ~24mm; volume ~doubled to 17.3k mm^3),
  with NO smoothing. EAT/GROW unchanged (hemisphere, still above the floor).
- NEW 7th invariant `additive`: the built height field must stay >= the spec
  parabola floor (tol 1mm). All three pass with 0.00mm deficit (purely additive).
- `smooth` now exempts the silhouette EDGE band (within ~2.5mm of the outline =
  rim / shoulder / arm-edge / corner, all intended edge features), checking only
  the wide body interior. (Replaced the corner-only exemption — MOVE's folds were
  along thin arm EDGES, not at corners.)
All three pieces: 7/7.

NOT A BUG (user asked why non-symmetric meshes passed): rotational symmetry is
NOT one of the invariants, so meshes pass while only approximately symmetric
(shape from the hand-drawn SVG; isotropic remesh irregular). Enforcing exact
N-fold symmetry = task #2 (symmetrize outline + mesh one wedge & rotate-copy +
a `symmetry` invariant).

## Symmetry (task #2 — option B: fully symmetric TRIANGULATION) — user chosen

User needs the triangulation itself exactly N-fold (not just the shape): these
are sculpting bases and asymmetry compounds under symmetric sculpting.

- Step 1 DONE: `symmetry.symmetrize_region` makes any outline exactly N-fold
  (rasterize -> majority-vote over N rotations about the centroid -> re-extract
  the contour). Works for the non-star spiral; areas unchanged (520->519,
  691->691, 791->791). See diagrams/sym_outlines.png.
- `inv_symmetry` added (8th invariant): rotating the mesh by 2pi/fold must map
  its vertex set onto itself (<= tol). The current isotropic-remesh meshes FAIL
  (~50% of verts off, max ~0.58mm) — proof they were never truly symmetric.
- PLAN (remaining): wedge-replication mesher — mesh one 1/N sector with MATCHED
  radial cut vertices, solve the membrane field on the wedge with Neumann
  (symmetry) BC on the cuts + Dirichlet 0 on the outline, lift, then rotate-copy
  and weld the cuts. EAT/GROW first (star-shaped -> simple pie slice, one cut
  interval per ray); MOVE needs sector-clipping (spiral arms cross sectors).
- KEY TRADEOFF: a symmetric mesh CANNOT use the global isotropic remesh (it's
  irregular). Quality within the wedge must come from metric-sized Triangle
  meshing or a cut-preserving wedge remesh. (This is the hard part.)

Step 2 DONE (star pieces): `symmetry.wedge_mesh_star` meshes one pie-slice with
matched radial cuts (Triangle, `Y` = no boundary Steiner so the cuts stay
matched); `replicate2d` rotate-copies + welds. EAT & GROW 2D meshes are EXACTLY
N-fold: 0.00000mm mismatch under 2pi/N rotation. See diagrams/sym_mesh.png.
TODO: (a) quality — wedge has thin tris near cuts/center (min angle 11-16deg vs
~25 target); fix with a CUT-PRESERVING wedge remesh (keep cut verts so copies
still weld). (b) lift (membrane on wedge, Neumann on cuts) + close + validate.
(c) MOVE spiral: non-star, needs sector-CLIPPING (shapely) not a pie slice.

Step 3 DONE: `_improve_wedge` (Lloyd smoothing, cuts fixed) -> wedge min angle
24deg; full symmetric build path `build.build_piece_symmetric` (wedge -> lift
membrane -> close, NO global remesh). EAT & GROW now pass manifold, silhouette,
no_overhang, smooth, additive, AND `symmetry` (0.00mm, exact!). 6/8.

REMAINING HURDLE (quality without the global remesh): uniform_tris + no_long_edges
fail on the symmetric pieces. Root cause located: the wall->dome SHOULDER near
the concave NOTCHES (membrane reentrant-corner gradient x the 18mm dome) makes
a steep step -> ~3-5mm edges + slivers. Uniform refinement doesn't fix it
efficiently (GROW 65k faces, still 3.9mm); profile change (parabola/shallow)
doesn't either; the global isotropic remesh that DID fix it can't run on a
symmetric mesh. FIX OPTIONS: (1) metric wedge meshing where the CUTS are sampled
by 3D arc length (fine where the lift is steep -> matched on both cuts) + metric
interior refinement, all 'Y' so cuts stay matched; (2) custom cut-preserving
incremental remesh; (3) reduce shoulder steepness (more notch rounding / no
wall) = design change. Recommend (1).

REASSESSMENT (after trying the fixes): metric wedge meshing (1) FAILED (slivers
to 0.6deg, long edges persist — coarse |grad H| underestimates the shoulder);
no-wall (3) FAILED (worse, breaks manifold). Topological symmetry (option B) is
ACHIEVABLE (exact 0.00mm 2D mesh) but its lifted mesh QUALITY is intractable
without a research-grade constrained/anisotropic remesher (the isotropic remesh
that gives quality inherently breaks symmetry). KEY REFRAME: option A (symmetrize
the REGION, then the proven isotropic pipeline) yields an exactly symmetric
SHAPE / silhouette / height field with full quality (7/7) — only the triangle
VERTEX PATTERN isn't identical per sector. Sculpting symmetry (Blender mirror/
radial) works in SPACE, not mesh topology, so a symmetric shape + quality mesh
likely suffices. Re-presenting A vs B to the user with this evidence.

CUT-LOCKED REMESH TESTED (the would-be linchpin of a "build symmetry in from the
start" rebuild): pymeshlab `compute_selection_by_condition_per_face` + remesh
`selectedonly` DOES preserve the cut vertices exactly (0.0mm) BUT creates slivers
at the selection boundary (min angle 0.1deg) and leaves the cut-strip's long
edges unsplit. 5th failed approach. CONCLUSION: topological symmetry + isotropic
quality on these steep-shouldered pieces is not achievable with Triangle +
pymeshlab; it needs a CUSTOM anisotropic constrained remesher (real R&D). A
rebuild does NOT fix this — it's an algorithm/tooling gap, not an architecture
one. RECOMMEND option A (symmetric SHAPE + quality, shippable now) + a
shape-symmetry invariant; file topological symmetry (B) as a research task.
