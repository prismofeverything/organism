# Sculpting settings & recommendations

Practical setup for sculpting the three player pieces in Blender, then re-attaching
the universal connector. Grounded in the actual meshes in `pieces/`.

## Scope

- **Sculpt targets: `EAT`, `MOVE`, `GROW`** — the watertight-manifold blanks in `pieces/`.
- **`FOOD` is not sculpted.** The only changes to FOOD are functional/stackability tweaks
  done in code (it already carries its own dome+ridge+socket connector).
- The blanks have **no connector**. The universal connector is grafted **after** sculpting
  with `graft_connector.py` (see [Connector landing](#protect-the-connector-landing)).

## The meshes you're starting from

| piece | verts | faces | bbox (mm) | mean edge | bottom socket? | watch for |
|---|---|---|---|---|---|---|
| EAT  | 14,077 | 28,150 | 33.1 × 34.6 × **48.0** | 0.78 mm | no  | top Ø16 landing only |
| GROW | 10,362 | 20,720 | 36.8 × 36.8 × **36.0** | 0.77 mm | yes | top + bottom landings |
| MOVE | 13,411 | 26,818 | 36.6 × 35.8 × **60.0** | 0.82 mm | yes | thin legs/arches → finer voxel |

All three are **watertight 2-manifold**, ~37 mm footprint, ~0.7–0.8 mm average edge length.
(`MOVE` is still the placeholder body — see README "Status" — but the settings below apply.)

## 0. Scene setup (once per file)

- **Units** — Scene Properties → Units → Metric, **Unit Scale `0.001`**, Length = Millimeters.
  Now 1 Blender unit reads as 1 mm, so every voxel/detail number below is real mm.
- **Apply scale** — Object Mode → `Ctrl+A` → Scale. Uniform sculpt detail depends on object scale = 1.
- Shade Smooth; Edit Mode → `Shift+N` (recalc normals outside). No repair needed going in —
  they're already watertight manifold.

## 1. Topology: voxel remesh as the workhorse, dyntopo for accents

For solid pieces headed to a printer, make **Voxel Remesh** the base (Sculpt header → Remesh, or `Ctrl+R`):

- Set in real mm, always returns watertight manifold, print-safe.
- **Voxel size:** blocking **0.4–0.5 mm**; detail pass **0.25–0.3 mm**. Re-voxel when the mesh stretches.
  Enable *Fix Poles* + *Smooth*.
- Match voxel size to your **thinnest feature** — keep voxel ≤ ⅓ of it. **MOVE**'s legs/arches are
  the thin bits → use 0.25–0.3 mm there or they erode; EAT/GROW are chunkier, 0.4–0.5 mm is fine.

**Dyntopo** — best for on-the-fly *local* detail, not as the base. `Ctrl+D` to toggle, then in the popover:

- **Detail Type: Constant Detail** — world-space, zoom-independent. Essential so detail size means
  something at print scale. *Avoid Relative Detail* for dimensional work.
- **Resolution:** use the **eyedropper** to sample the mesh (≈0.75 mm now), then raise it until
  edges are ~0.4 mm. (The % units aren't intuitive — sample, don't guess.)
- **Refine Method: Subdivide Collapse** (adds *and* removes detail — keeps density sane).
  **Smooth Shading: on.** **Detail Flood Fill** applies the detail once across the whole mesh.
- Caveats: dyntopo gives uneven density and can go **non-manifold** if you push through a thin wall.
  So shape big forms with it **off** (Grab / large strokes), flip it **on** for fine detail, and
  **finish with one Voxel Remesh** to guarantee a clean manifold.
- Don't out-detail the printer: FDM (0.4 mm nozzle) won't hold features under ~0.8 mm; resin holds
  ~0.1–0.2 mm. Set detail size to match your process.

## 2. Brushes

- **Clay Strips** — primary blocking / buildup · **Clay** — softer fill
- **Grab / Snake Hook / Elastic Deform** — big masses, pull out limbs/protrusions (dyntopo off)
- **Inflate** — add volume · **Crease / Draw Sharp** — crisp ridges & creases · **Pinch** — sharpen edges
- **Flatten / Scrape / Fill** — planar transitions · **Smooth** (hold `Shift`) — constantly
- **Mask** (`Ctrl`-drag) + **Face Sets** — isolate / protect regions (next section)

## 3. Protect the connector landing

The connector is grafted **after** sculpting, so the body is yours — but keep its landing zones clean:

- **Top:** keep a flat, level disc **~Ø16 mm** at the top center. The peg (Ø6 dome + Ø12 ridge)
  unions at the mesh's max-Z; mask/Face-Set that disc so brushes can't tilt or bump it. A sloped or
  pointed top makes the peg land crooked.
- **Bottom (MOVE & GROW only):** keep z=0 flat and solid across the center **~Ø12–13 mm** so the
  socket carve (Ø6 dome + clearance) doesn't punch through. **EAT has no socket** (food never sits
  under EAT) — its underside is unconstrained.

Connector spec (from `pieces_v2.py` / `graft_connector.py`, keep in sync):

| param | value |
|---|---|
| dome | Ø6.0 mm × 3.0 mm tall (parabolic) |
| ridge | OD 12.0 / ID 9.0 mm × 0.6 mm tall, peak width 1.0 |
| plateau | Ø16.0 mm |
| clearance (socket) | 0.10 mm |
| peg top z | EAT 48 (no socket) · MOVE 60 (socket) · GROW 36 (socket) |

## 4. Mesh integrity & cleanup

- The graft is an **EXACT boolean → requires watertight 2-manifold input.** Holes,
  self-intersections, or spikes make it fail.
- **Safe finish:** final **Voxel Remesh** (guaranteed manifold; keep the top plateau flat) → export → graft.
- Verify with the **3D-Print Toolbox** addon (enable in Preferences → Add-ons): *Check All* →
  non-manifold / holes / wall thickness / overhang; *Make Manifold* fixes most.
- Keep walls **≥ ~1 mm (FDM) / ~0.6 mm (resin)** so detail and the socket carve survive printing.
- There is **no headless cleanup script yet**. Candidate: `pieces/cleanup_mesh.py` (pymeshlab —
  weld duplicates, drop non-manifold/unreferenced geometry, close holes, optional isotropic remesh)
  so an OBJ can be cleaned from the CLI without opening Blender.

## 5. Workflow per piece

1. Import `pieces/<PIECE>.obj`; do the [scene setup](#0-scene-setup-once-per-file).
2. Mask the connector landings (top Ø16; bottom Ø12–13 for MOVE/GROW).
3. Voxel-remesh (0.4–0.5 mm) and block out — Clay Strips, Grab.
4. Detail pass — dyntopo Constant Detail ~0.4 mm, or voxel 0.25–0.3 mm; Crease / Draw Sharp / Pinch.
5. **Final Voxel Remesh** (clean manifold), keeping the top plateau flat. Export OBJ.
6. Re-attach the connector:
   ```sh
   blender --background --python graft_connector.py -- --in <sculpt>.obj --piece EAT --out <final>.obj
   ```
   (`--piece` one of EAT / MOVE / GROW — controls the bottom socket.)
7. Optional: `invariants.py` to re-validate; 3D-Print Toolbox to confirm watertight + wall thickness.

## Related

- `README.md` — pipeline overview and how the blanks are generated
- `DECISIONS.md` — design rationale, including the post-sculpt connector graft flow
- `graft_connector.py` — re-attaches the connector after sculpting
- `connector_meta.json` — per-piece peg/socket placement
