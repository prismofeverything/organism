// =====================================================================
// Chroma — parametric 3D-print test board  (OpenSCAD)
// =====================================================================
// Hex board, radius 5 (6 cells/edge) MINUS the 9-corner trim  -> 82 hexes
// (81 placeable wells + the black center [0,0]).  Geometry mirrors the
// live engine chroma-core.js (axial q,r; flat-top hexes).
//
// Per Muhammad (2026-06-14):
//   * each hex has a CIRCULAR recess, 1 in (25.4 mm) dia  -> chit well
//   * a 0.5 in (12.7 mm) through-hole at the well bottom   -> backlight
//   * walls per PLA recommendation                         -> 2 mm default
//   * stack depth = 3
//
// QoL features folded in from the BGG / 3D-print recessed-board research
// (all parametric, toggle as needed):
//   * rim chamfer for token pickup
//   * finger-scoop notch in each rim (press-to-lift)
//   * "wells-up" orientation = support-free vertical light hole
//   * RENDER mode: print a small cluster/single first (board > most beds)
//   * optional engraved q,r coordinate labels
//   * optional white-balance reference patch on the border (camera scoring)
//
// Build STL:
//   openscad -o chroma_cluster.stl -D 'RENDER="cluster"' chroma_board.scad
//   openscad -o chroma_board.stl   -D 'RENDER="full"'    chroma_board.scad
// =====================================================================

// Per-cell sector + seed-color data, auto-generated from chroma-core.js
// (regenerate: node ../gen_seed_data.js). Defines SECTOR_DATA and SEED_DATA.
include <chroma_seed_data.scad>

/* [Render] */
// "full" = whole 82-hex board (BIG ~290 mm, will exceed most beds — tile it)
// "cluster" = center + 6 neighbours (7 hexes, bed-friendly print test)
// "single" = one cell
RENDER = "cluster";

/* [Board geometry] */
RADIUS = 5;            // engine radius (5 = 6 cells per edge)

/* [Cell — physical spec] */
chit_dia     = 19.05;  // 0.75 in  ACTUAL chit diameter (measured)
fit_clear    = 0.5;    // min diametral drop-in clearance for the well (FDM ~0.4–0.5)
// The game is light-STARVED (pipeline transmission data, 2026-06-17): single saturated
// gels pass only 5–7%, 3-stacks 0.05–1.4%, at/below the camera black-floor. So we drive
// for MORE light. ledge + hole now set the cell size: ledge=4mm support + a 14mm hole ->
// recess = hole+2*ledge = 22mm (governs over chit+fit since 22 > 19.55). The 0.75in chit
// still fully covers the 14mm hole even at max lateral play (margin ~1mm). hole_cs flares
// the hole's UNDERSIDE to gather more light from the panel without touching the ledge.
ledge        = 4.0;    // support ledge under the chit (chit rests on this annulus)
light_hole   = 14.0;   // backlight through-hole (up from 12.7 -> +21% throughput)
hole_cs      = 1.0;    // countersink on hole underside (light-gather); 0 = straight hole
wall         = 2.0;    // min material between adjacent wells (PLA 1.5–2.0)
floor_th     = 2.0;    // solid floor under the recess (hole passes through it)

/* [Chit stack -> well depth] */
// The chit is a Rosco gel laminated in a GLOSS pouch, cut after laminating
// (voice-pipeline prototyping thread, 2026-06-15). Estimated final thickness of
// ONE laminated chit:  gel ~0.13mm (≈5 mil polyester) + 5-mil/side gloss pouch
// ~0.25mm  ≈ 0.38mm  -> rounded to 0.4mm. (5-mil pouch = the "credit-card-ish"
// stiffness pick; a 3-mil pouch → ~0.28mm, a 10-mil → ~0.65mm.)
chit_th      = 0.4;    // est. final thickness of one laminated chit (mm)
stack_clear  = 0.4;    // headroom above a full 3-stack so the top chit tucks under the rim
stack_n      = 3;      // max chits per cell (engine depth = 3)

/* [QoL] */
rim_chamfer  = 0.6;    // chamfer at the top of each well (easy pickup); 0 = off
scoop        = true;   // finger-scoop notch in the rim (press-to-lift a chit)
scoop_w      = 9;      // scoop opening width
draft        = 2;      // wall draft angle in deg (eases chit release; ~2 typical)
labels       = false;  // engrave axial q,r in each cell floor
label_h      = 4;      // label text height
white_patch  = false;  // white-balance reference patch on the border (camera scoring)
patch_sz     = 20;     // reference-patch square size

/* [Strength — fillets + perimeter frame] */
// Stress-relief fillets at the cavity's internal corners (the hole/ledge edge and the
// rib-to-floor junction) remove the sharp stress risers that crack brittle prints.
fillet_wall  = 1.5;    // concave fillet where the recess wall meets the floor (rib base)
fillet_hole  = 0.8;    // roundover of the ledge inner edge (hole top) — also smoother for chits
// A raised perimeter frame rings the whole board OUTSIDE the cells = a stiff edge beam
// for handling a one-piece print. Hug s the trimmed silhouette via a 2D offset.
frame        = true;   // add the perimeter frame
frame_w      = 3.0;    // frame wall thickness
frame_lip    = 2.0;    // how far the frame rises above the top surface

/* [Region dividers + seed marks] */
// Raised ridges along the 6 sector (wedge) boundaries — the physical version of
// the digital "thick wedge border." They follow cell edges (sit in the inter-cell
// gaps), only between cells of DIFFERENT sectors.
dividers     = true;   // add the wedge-boundary ridges
divider_w    = 2.0;    // ridge width (fits the 2mm inter-cell wall gap)
divider_h    = 1.5;    // ridge height above the board top
// Engrave each SEEDED space's start-color letter (C/M/Y/R/G/B, K at center) into
// its ledge at 6 o'clock — visible at setup, covered once the seed chit is placed.
seed_marks   = true;   // engrave start-color letters
seed_letter_h= 3.0;    // letter cap height
seed_engrave = 0.6;    // engrave (deboss) depth

// MATERIAL: print in PETG or a toughened PLA (PLA+/PLA Pro), NOT brittle plain PLA —
// the board is a large thin part and plain PLA is impact-fragile. Print FLAT (wells up):
// vertical light holes need no supports and the plate's strong direction resists bending.

// $fn drives smoothness of the round wells/holes AND CGAL export time. 96 is
// smooth but the full 82-cell F6 export takes ~8 min; drop to 48 for a fast
// draft STL (-D '$fn=48'), keep 96 for the final print export.
$fn = 96;

// ---- derived ----
// recess is the LARGER of: enough to drop the chit in, OR enough for hole + 2*ledge
recess_dia   = max(chit_dia + fit_clear, light_hole + 2*ledge);   // ≈22mm here
recess_depth = stack_n*chit_th + stack_clear;  // well depth = stack of 3 + headroom (≈1.6mm)
// chit coverage over the hole at MAX lateral play (>0 means it always covers):
cover_margin = chit_dia/2 - light_hole/2 - (recess_dia - chit_dia)/2;
flats  = recess_dia + 2*wall;     // hex across-flats == cell pitch
R      = flats / sqrt(3);         // hex circumradius (center -> vertex)
H      = recess_depth + floor_th; // total board thickness
eps    = 0.01;

// flat-top axial (q,r) -> planar (x,y)   [Red Blob Games]
function px(q,r) = 1.5 * R * q;
function py(q,r) = flats * (r + q/2);

// the 9 trimmed corner cells (120°-symmetric), from chroma-board-proto.js
removed = [[5,0],[4,1],[5,-1],[-5,5],[-5,4],[-4,5],[0,-5],[1,-5],[-1,-4]];
function is_removed(q,r) =
    len([for (c=removed) if (c[0]==q && c[1]==r) 1]) > 0;

// cell set for the chosen render mode
cluster = [[0,0],[1,0],[-1,0],[0,1],[0,-1],[1,-1],[-1,1]];
function cells() =
    RENDER=="single"  ? [[0,0]] :
    RENDER=="cluster" ? cluster :
    [ for (q=[-RADIUS:RADIUS], r=[-RADIUS:RADIUS])
        if (abs(q+r) <= RADIUS && !is_removed(q,r)) [q,r] ];

// ---- one hex prism (flat-top: default $fn=6 cylinder already flat-top) ----
module hex_prism(h) { cylinder(h=h, r=R, $fn=6); }

// arc of points (r,z) from angle a0->a1 about (cx,cz), n segments
function arc(cx,cz,rad,a0,a1,n) =
    [for (i=[0:n]) [cx + rad*cos(a0+(a1-a0)*i/n), cz + rad*sin(a0+(a1-a0)*i/n)]];

// ---- cavity for one cell as ONE rotate_extruded profile (countersink + hole +
// filleted ledge edge + filleted rib base + drafted wall + rim chamfer). Single
// profile lets every internal corner be rounded; one revolve is cheaper than the
// old stack of cylinder cuts. The scoop stays a separate cut (not axisymmetric).
module cell_cut() {
    hr  = light_hole/2;
    rr  = recess_dia/2;
    rrt = rr + tan(draft)*recess_depth;          // recess top radius (draft)
    Zf  = floor_th;
    rfh = fillet_hole;
    rfw = fillet_wall;
    prof = concat(
        [[0, -eps], [hr+hole_cs, -eps]],         // bottom + countersink mouth
        hole_cs > 0 ? [[hr, hole_cs]] : [],      // countersink top
        [[hr, Zf-rfh]],                          // up the hole to the ledge roundover
        arc(hr+rfh, Zf-rfh, rfh, 180, 90, 6),    // roundover: hole-top -> ledge (convex)
        [[rr-rfw, Zf]],                          // across the ledge top to rib-base fillet
        arc(rr-rfw, Zf+rfw, rfw, -90, 0, 6),     // concave fillet: ledge -> recess wall
        [[rrt, H], [rrt+rim_chamfer, H+eps], [0, H+eps]]   // wall, rim chamfer, top
    );
    rotate_extrude() polygon(prof);
    // finger-scoop notch on the +x rim: cut from the recess floor up through the
    // rim so a fingernail can slide under the stack (depth-aware).
    if (scoop)
        translate([recess_dia/2, 0, H-recess_depth])
            cylinder(h=recess_depth+rim_chamfer+eps, d=scoop_w);
}

// ---- engraved axial label in the cell floor (optional) ----
module cell_label(q,r) {
    translate([0,0,floor_th-0.5])
        linear_extrude(0.6+eps)
            text(str(q,",",r), size=label_h, halign="center",
                 valign="center", font="Liberation Sans");
}

// ---- one finished cell (prism with its own well/hole/scoop/label) ----
// Differencing per-cell instead of one monolithic union-then-difference keeps
// the CSG tree small -> CGAL render is ~orders faster on the 82-cell board.
// sector / seed lookups from the embedded data (-1 / "" if not a playable cell)
function sector_of(q,r) =
    let (m = [for (s=SECTOR_DATA) if (s[0]==q && s[1]==r) s[2]]) len(m) ? m[0] : -1;
function seed_of(q,r) =
    let (m = [for (s=SEED_DATA) if (s[0]==q && s[1]==r) s[2]]) len(m) ? m[0] : "";

module cell(q,r) {
    difference() {
        hex_prism(H);
        cell_cut();
        if (labels) cell_label(q,r);
        // start-color letter engraved on the ledge at 6 o'clock (seeded cells only)
        if (seed_marks && seed_of(q,r) != "")
            translate([0, -(light_hole/2 + (recess_dia-light_hole)/4), floor_th-seed_engrave])
                linear_extrude(seed_engrave+eps)
                    text(seed_of(q,r), size=seed_letter_h, halign="center",
                         valign="center", font="Liberation Sans:style=Bold");
    }
}

// ---- 2D footprint of all cells (union of flat-top hexes) ----
module footprint_2d() {
    for (c=cells()) translate([px(c[0],c[1]), py(c[0],c[1])]) circle(r=R, $fn=6);
}

// ---- raised perimeter frame: a ring hugging the board silhouette, OUTSIDE the
// cells, rising frame_lip above the top -> a stiff edge beam for a one-piece print.
module perimeter_frame() {
    linear_extrude(H + frame_lip)
        difference() { offset(frame_w) footprint_2d(); footprint_2d(); }
}

// ---- raised ridges along the 6 sector (wedge) boundaries ----
// For each playable cell, raise the shared edge with any neighbour in a DIFFERENT
// sector. Ridge = a thin bar centred on the shared edge, perpendicular to the line
// between the two cell centres, length = hex side (R), sitting on the wall top.
NB = [[1,0],[1,-1],[0,-1],[-1,0],[-1,1],[0,1]];   // axial neighbours
module sector_dividers() {
    for (c=cells()) {
        s0 = sector_of(c[0],c[1]);
        for (d=NB) {
            nq = c[0]+d[0]; nr = c[1]+d[1];
            s1 = sector_of(nq,nr);
            if (s1 >= 0 && s1 != s0) {
                mx = (px(c[0],c[1])+px(nq,nr))/2;
                my = (py(c[0],c[1])+py(nq,nr))/2;
                ang = atan2(py(nq,nr)-py(c[0],c[1]), px(nq,nr)-px(c[0],c[1]));
                translate([mx,my,H-eps]) rotate([0,0,ang])
                    translate([-divider_w/2, -R/2, 0])
                        cube([divider_w, R, divider_h+eps]);
            }
        }
    }
}

// ---- assemble ----
module board() {
    cs = cells();
    for (c=cs) translate([px(c[0],c[1]), py(c[0],c[1]), 0]) cell(c[0],c[1]);
    if (frame) perimeter_frame();
    if (dividers) sector_dividers();
    // white-balance reference patch on the border (camera scoring). Anchored to
    // an existing right-edge cell ([5,-2] on the radius-5 trimmed board) via a
    // short bridge neck so it prints as ONE solid piece -- the y=0 corner where
    // it used to sit was removed by the 9-corner trim. Recessed pocket holds the
    // neutral reference inlay; pocket walls shield it from stray backlight.
    if (white_patch && RENDER=="full") {
        ox = px(5,-2); oy = py(5,-2);
        x0 = ox + R - 6;                 // start 6mm inside the cell vertex -> weld
        translate([x0, oy - patch_sz/2, 0]) difference() {
            cube([12 + patch_sz, patch_sz, H]);                 // 6 weld + 6 bridge + tab
            translate([12 + wall, wall, floor_th])              // pocket only in the tab
                cube([patch_sz-2*wall, patch_sz-2*wall, H-floor_th+eps]);
        }
    }
}

board();

// echo a few key dimensions to the console at compile time
echo(str("Chroma board | mode=", RENDER, " | cells=", len(cells()),
         " | pitch=", flats, "mm | thickness=", H,
         "mm | recess=", recess_dia, "mm | hole=", light_hole,
         "mm | ledge=", (recess_dia-light_hole)/2,
         "mm | chit-play=", (recess_dia-chit_dia)/2,
         "mm | cover-margin=", cover_margin,
         "mm | lit-fraction=", 100*pow(light_hole/chit_dia,2), "%"));
