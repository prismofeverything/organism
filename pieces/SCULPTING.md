# Sculpting settings & recommendations

Practical setup for sculpting the three player pieces in Blender, then re-attaching
the universal connector. Grounded in the actual meshes in `pieces/out/`.

## Scope

- **Sculpt targets: `EAT`, `MOVE`, `GROW`** — the watertight-manifold blanks at
  `pieces/out/{EAT,MOVE,GROW}.obj` (built by `make blanks`).
- **`FOOD` is not sculpted.** Functional/stackability changes happen in code
  (`build_food.py`) — it already carries its own dome+ridge+socket connector.
- The blanks have **no connector and no socket** — both ends are bare. The universal
  connector is added **after** sculpting by `make grafts` (which calls
  `meshlib/build.py graft …`); that pass also closes the bottom with the rounded
  rim fillet that's already on the blank. See [Connector landing](#4-protect-the-connector-landing).

## The meshes you're starting from

Stats from the current `pieces/out/` blanks (run `python pieces/meshlib/build.py` to refresh):

| piece | verts | faces | bbox (mm) | median edge | min cross-section | watch for |
|---|---|---|---|---|---|---|
| EAT  | 16,912 | 33,820 | 33.1 × 34.7 × **48.0** | 0.69 mm | Ø14.5 at z≈47 (top) | crown is already near the connector seat (Ø14) |
| GROW | 12,290 | 24,576 | 36.8 × 36.8 × **36.0** | 0.72 mm | Ø8.6  at z≈35 (top) | low dome → keep the top plateau flat |
| MOVE | 13,019 | 26,034 | 36.6 × 35.8 × **60.0** | 0.84 mm | Ø4.4  at z≈59 (apex)  | thin spiral apex (~4 mm) — small brushes near the top |

All three are **watertight 2-manifold** (euler 2), ~37 mm footprint, with edges
clustered tightly (~0.3 mm min, ~1.0 mm p95). They are the modern `meshlib`
output — uniform isotropic remeshed, fully N-fold symmetric (EAT 5-fold,
GROW 4-fold, MOVE 3-fold), with a rounded sculptable rim at z=0.

## 0. Scene setup (once per file)

- **Units** — Scene Properties → Units → Metric, **Unit Scale `0.001`**, Length = Millimeters.
  Now 1 Blender unit reads as 1 mm, so every voxel/detail number below is real mm.
- **Apply scale** — Object Mode → `Ctrl+A` → Scale. Uniform sculpt detail depends on object scale = 1.
- Shade Smooth; Edit Mode → `Shift+N` (recalc normals outside). No repair needed going in —
  they're already watertight manifold.

## 1. Dyntopo — the everyday tool

`Ctrl+D` toggles. Open the popover (the chevron next to the toggle):

| Setting | Value | Why |
|---|---|---|
| **Detail Type** | **Constant Detail** | World-space, zoom-independent — `0.7` always means 0.7 mm |
| **Resolution (detail size)** | see table below | The blanks sit at ~0.7–0.85 mm median; pick relative to that |
| **Refine Method** | **Subdivide Collapse** | Adds *and* removes triangles — density stays bounded under prolonged work |
| **Smooth Shading** | on | New tris carry smooth normals — no faceted hits in fresh strokes |
| **Symmetrize / Mirror (Object Props ▸ Mirror)** | X (and Y for fold≥4) | Sculpt mirrored so EAT/GROW/MOVE stay N-fold |

**Detail size guide** (Constant Detail, in mm — the field reads literal mm thanks to Unit Scale 0.001):

| Detail size | Use for | Notes |
|---|---|---|
| **1.5–2.0 mm** | First-pass blocking | Sparser than the mesh → Subdivide Collapse will **decimate** under your stroke, so massive Grab/Clay-Strips moves don't bog down |
| **0.7–0.8 mm** | Match-existing — the default working size | Same density as the blank; new strokes blend invisibly |
| **0.4–0.5 mm** | Detail accents (Crease, Draw Sharp, Pinch) | About the FDM-printable resolution floor; safe for hero details |
| **0.25–0.3 mm** | Resin-only fine detail | Will multiply tris fast — keep strokes small |
| **≤ 0.2 mm** | Don't | Sub-printable on FDM, slow, and dyntopo's non-manifold risk grows with density |

Hit **Detail Flood Fill** to apply the current Constant Detail uniformly across the whole mesh
(useful right after a big remesh to harmonize density). Use the **eyedropper** to sample edge length
where the cursor is — the blanks read **~0.7 mm on EAT/GROW**, **~0.85 mm on MOVE**.

**Cautions specific to these blanks:**

- **MOVE's apex (~4 mm thick spiral arms)** — keep detail size ≤ 0.5 mm and brush radius ≤ 2 mm
  there, or one stroke will punch through the arm or smear adjacent arms together (going non-manifold).
- **Don't push features below 1 mm walls on FDM, 0.6 mm on resin** — they'll vanish in print even if dyntopo
  resolves them.
- Dyntopo can go non-manifold if you push through a thin wall — so **finish each session
  with one Voxel Remesh** (next section) before exporting.

## 2. Voxel Remesh — the cleanup/blocking pass

Sculpt header → Remesh (`Ctrl+R`). Always returns watertight 2-manifold, print-safe.

| When | Voxel size | Notes |
|---|---|---|
| Block out (Day-0 big-form pass) | **0.5–0.6 mm** | Coarser than the blank — homogenizes density quickly |
| Mid-detail re-base | **0.30–0.40 mm** | Re-voxel here whenever the mesh has stretched/distorted under Grab |
| Final clean before export | **0.25–0.30 mm** | Guarantees a clean manifold for the connector graft boolean (next section) |
| **MOVE specifically** | **0.25 mm** at the apex | The spiral arms get eroded by voxel ≥ ⅓ of the thinnest feature (⅓·4 mm = 1.3 mm, but stay well under to keep the arm crisp) |

Enable **Fix Poles** + **Smooth Normals**. Match voxel size to your **thinnest feature**
(keep voxel ≤ ⅓ of it) — MOVE's apex demands 0.25 mm; EAT/GROW are chunkier, 0.4 mm is fine.

## 3. Brushes — size & strength

**First, lock brush radius to mm** — by default it's in *View* units (screen pixels), so
zooming in/out changes the brush's world size. Fix once per brush, or once globally:

- **Per-brush:** *Sidebar (`N`) → Tool → Brush Settings → Radius* → click the unit dropdown
  beside the Radius slider → **Scene**. (Sets `brush.use_locked_size = 'SCENE'`.)
- **All brushes at once:** *Properties ▸ Active Tool ▸ Options ▸ Unified Radius* — flip on,
  then set the unit to Scene once. All brushes now share that radius.
- `F` drags radius live in the viewport · `Shift+F` drags strength.

You may want detail brushes (Crease, Draw Sharp, Pinch, Clay Strips) on **Scene** so the mm
target below is real, and Grab/Smooth on **View** so they always feel "viewport-sized" —
mix per brush. Strength is unitless 0–1.

| Brush | Radius (mm) | Strength | Dyntopo | What it's for |
|---|---|---|---|---|
| **Clay Strips** | **4–8** | **0.5** | on @ 0.7 mm | Primary blocking + buildup — flat-trailed strokes |
| **Clay** | 5–10 | 0.6 | on @ 0.7 mm | Softer, rounder fill than Clay Strips |
| **Grab** | **10–20** | 1.0 | **off** | Pull whole limbs/lobes — no new topology, preserves density |
| **Snake Hook** | 6–12 | 1.0 | on @ 1.5 mm | Extrude tentacles, horns — dyntopo coarse so new tris stay fat |
| **Elastic Deform** | 12–25 | 0.7 | off | Volume-preserving big translations (no spike artifacts) |
| **Inflate** | 6–12 | **0.3–0.4** | on @ 0.7 mm | Thicken — keep strength low or you'll bloat lobes round |
| **Crease** | **2–5** | 0.6 | on @ 0.4 mm | Recessed crease; hold `Ctrl` for raised outset |
| **Draw Sharp** | **1–3** | 0.7–0.9 | on @ 0.4 mm | Crisp single-pixel ridges (pair with Pinch) |
| **Pinch** | 2–5 | 0.3–0.5 | on @ 0.4 mm | Sharpen an existing edge — light strength to avoid pinching off |
| **Flatten / Fill / Scrape** | 6–12 | 0.4–0.6 | on @ 0.7 mm | Planar transitions — `Plane Offset 0`, `Angle 0` |
| **Smooth** (hold `Shift`) | 5–10 | 0.5 | n/a | Constantly — relaxes topology under any other brush |
| **Mask** (`Ctrl`-drag) | 5–10 | 1.0 | n/a | Protect the connector landing (next section) |

**Rules of thumb:**

- Brush radius ≈ **2–4× the dyntopo detail size**. Smaller and you're sampling one vertex per
  stroke; bigger and you skip detail.
- Cap brush radius at **¼ of the piece's footprint** (~9 mm) for body work, and at **half the
  local cross-section width** (so on MOVE near the apex, radius ≤ 2 mm).
- Strength `0.5` is the right default — only Grab/Snake Hook/Elastic Deform want full strength,
  and only Inflate/Pinch want lower.

## 4. Protect the connector landing

The connector is grafted **after** sculpting (`make grafts`), so the body is yours — but the
graft pass needs predictable real-estate at the top to land on:

- **Top:** the graft re-stacks the body up to a knee at `GRAFT_KNEE · seat_z` (≈ 37 mm on EAT,
  28 mm on GROW, 47 mm on MOVE) and then blends inward (EAT/GROW) or outward (MOVE) to a Ø14
  seat circle at `seat_z = zmax − 4.3`. **Keep the top ~5 mm zone roughly axisymmetric** — no
  big tilts, no leaning towers. Mask the top **Ø16 mm disc** to be safe.
- **Bottom:** all three pieces are now **solid-bottomed** (the graft pass closes the bottom
  with a vertical wall → quarter-round fillet → flat base — no socket carve anywhere).
  So you don't need to reserve a "landing disc" on the bottom. Just **don't punch through
  the z=0…1.1 mm fillet rim** with a brush stroke, or you'll leave a non-manifold edge that
  the graft will inherit.
- **EAT specifically:** its crown is already at Ø14.5 mm at z≈47, almost exactly the connector
  seat. Don't pinch it inward, or the graft's nestle-cap will have nothing to blend into.

Connector spec (from `meshlib/connector_field.py` — keep in sync):

| param | value |
|---|---|
| dome (`DOME_R × DOME_HEIGHT`) | Ø3.8 mm × 4.3 mm tall (parabolic) |
| ridge (`RIDGE_OD / RIDGE_ID × RIDGE_HEIGHT`) | OD 12.8 / ID 8.3 mm × 2.75 mm tall, peak width 2.0 |
| seat radius (`R_SEAT = RIDGE_OR + SEAT_MARGIN 0.6`) | **Ø14.0 mm** |
| socket clearance (FOOD only) | 0.20 mm (CLEARANCE 0.15 + IM 0.05) |
| dome apex z (final piece) | EAT 48 · MOVE 60 · GROW 36  (= each blank's `zmax`) |

## 5. Mesh integrity & cleanup

- The graft is an **exact boolean → requires watertight 2-manifold input.** Holes,
  self-intersections, or spikes make `meshlib/build.py graft` fail.
- **Safe finish per session:** Voxel Remesh @ 0.25–0.30 mm → export → run `make grafts`.
- Verify with the **3D-Print Toolbox** addon (enable in *Preferences → Add-ons*): *Check All* →
  non-manifold / holes / wall thickness / overhang; *Make Manifold* fixes most issues.
- Keep walls **≥ ~1 mm (FDM) / ~0.6 mm (resin)** so detail survives printing.
- Run `python pieces/meshlib/invariants.py` (or `make grafts` which calls it) to re-validate
  manifoldness, symmetry, profile, and edge-uniformity after sculpting.

## 6. Workflow per piece

1. Import `pieces/out/<PIECE>.obj`; do the [scene setup](#0-scene-setup-once-per-file).
2. Mask the **top Ø16 mm disc** and the **bottom rim z ≤ 1.1 mm** (with `Ctrl`-drag Mask
   or a Face Set).
3. **Block out:** Voxel Remesh @ 0.5 mm; Clay Strips r=6 s=0.5, Grab r=15 s=1.0 (dyntopo off).
4. **Mid-detail:** dyntopo Constant Detail @ 0.7 mm; Clay / Inflate / Flatten.
5. **Detail accents:** dyntopo @ 0.4 mm; Crease r=3, Draw Sharp r=2, Pinch r=3 (all s≈0.7).
6. **Final cleanup:** Voxel Remesh @ 0.25–0.30 mm. Keep the top axisymmetric.
   Export as `pieces/out/<PIECE>.obj` (overwriting the blank).
7. **Graft:** `make grafts` (or `python pieces/meshlib/build.py graft EAT MOVE GROW`).
   That re-stacks the body, blends inward/outward to the Ø14 seat, welds the structured
   connector solid-of-revolution, and closes the bottom. Result lands at
   `pieces/out/<PIECE>_graft.obj`. Invariants run automatically.
8. **Print plate:** `make plate` (after grafts + food) → `renders/food/print_plate.stl`.

## Related

- `README.md` — pipeline overview and how the blanks are generated
- `DECISIONS.md` — design rationale (append-only lab notebook)
- `meshlib/build.py` — `build_blank` / `build_graft` — entry points for both stages
- `meshlib/connector_field.py` — canonical connector dims (peg + seat)
- `meshlib/invariants.py` — the 9 invariants the build pipeline enforces
