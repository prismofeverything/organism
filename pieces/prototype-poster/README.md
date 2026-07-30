# ORGANISM — Poster Print Plan (Office Depot)

An alternate layout that packs everything onto **Office Depot poster sizes** (they only offer 16×20, 18×24,
24×36). Everything is circles/sheets, so you just cut them out. Files: `out/*.pdf` — send at **100% scale,
actual size, no scaling**. Regenerate with `build_posters.py`.

## Why 24×36
The two **21" (540mm) boards are the constraint** — a 21" circle only fits on 24×36, and two of them can't
share a sheet. So the whole order is 24×36 (one size to order, most efficient for the small stuff too).

## The 4 posters

**ADHESIVE (peel-and-stick → mount on 2mm chipboard, then cut):**
| # | File | Holds |
|---|---|---|
| 1 | `ADHESIVE_24x36_1.pdf` | **Pentagon board** (side A) + one token/platform sheet in the strip |
| 2 | `ADHESIVE_24x36_2.pdf` | **Hex board** (side B) + one token/platform sheet |
| 3 | `ADHESIVE_24x36_3.pdf` | third token/platform sheet + **power/score board** |

**STANDARD (cut-out hand cards):**
| # | File | Holds |
|---|---|---|
| 4 | `STANDARD_24x36_1.pdf` | **5 player aids** + **26 mutation cards** |

The **player platforms/fields** live on the three token sheets → they're on the **adhesive** posters, so they
mount on chipboard and end up stiff (poker-chip feel), same as the boards.

The board is **double-sided**: mount poster 1 (pent) and poster 2 (hex) **back-to-back on one chipboard**.

## The rulebook is NOT a poster — order it as a booklet
It's a bound **24-page, 223×223mm (8.77") square** booklet: `../prototype/print_ready/04_RULEBOOK/
rulebook_18pp.pdf`. 24 pages is a multiple of 4, so **saddle-stitch (saddle-stapled)** is the right
binding. Upload that PDF, pick saddle-stitch. The only wrinkle is the **square** size — if OD's booklet
tool only lists standard sizes, ask for **8.5×8.5 square** (I can hand you a version scaled to 216mm, a
~3% shrink you can't see) or the nearest size + custom trim. Cover: heavier stock (80–100lb cover); text
pages 80–100lb book/text.

## Paper — what to pick at the counter
Office Depot standard posters come in **matte, satin, gloss, high-gloss, canvas**; adhesive posters have the
peel-and-stick back. For flat-color art that's cut and handled:

- **Adhesive posters (1–3):** ask for **matte** (or satin) finish. Matte = no glare on the play surface, hides
  fingerprints. **Skip gloss/high-gloss** (glare + fingerprints on a board you stare at) and canvas (texture).
- **Standard poster (4):** **satin** (slight sheen, nice on cards) or **matte**. Poster paper is thinner than
  cardstock, so these cut-outs will be a bit floppy — for stiffer cards either **back them with chipboard**
  too, or print just poster 4 on **11×17 cardstock** at their print center instead (stiffer, same art).
- Don't bother with lamination unless you want extra durability; the boards get chipboard-mounted anyway.

## Cut & assemble
1. Adhesive posters → peel onto 2mm chipboard, burnish flat, then cut each circle/token with a craft knife or
   circle cutter. Boards: cut the 21" circle; mount pent+hex back-to-back; score/fold into the 2×2 quad-fold.
2. Standard poster → cut the aid and mutation circles; laminate if you want.
3. Cut-guides are printed as thin grey outlines; the tiny corner labels get trimmed away.

## If you'd rather fewer sheets
The two boards are unavoidable (2 sheets). The only way to 3 total is to drop the third token sheet onto the
standard poster — but that makes those platforms thin instead of chipboard-stiff, which cuts against putting
platforms on adhesive. Recommend keeping the 4.
