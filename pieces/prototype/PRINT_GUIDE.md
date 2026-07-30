# ORGANISM — Prototype Print Guide

How to turn the print masters into a legit, good-looking physical prototype you cut & assemble yourself.

**Golden rule:** every file in `print_ready/` has its **true physical size baked into the PDF**. Tell the
print shop **"print at 100% — actual size — do NOT scale/fit to page."** That's the whole trick to getting
dimensions right.

Source masters (all ~300 DPI, unchanged since March, verified == your `current version` download):
`~/Downloads/organism/prototype/`. Regenerate anytime with `pieces/prototype/build_prototype.py`.

---

## 1. What goes on what (the short answer)

| Component | File (`print_ready/…`) | Qty | Size | Material | Mount? |
|---|---|---|---|---|---|
| **Main board (DOUBLE-SIDED)** | pent = `…_PENT_540mm_FULL.pdf`, hex = `…_HEX_540mm_FULL.pdf` (or each's `_tiles_11x17/`) | 1 board, 2 faces | Ø540 mm (21") | print **both faces** → mount **back-to-back on one 2 mm chipboard** | **YES** |
| **Power/score board** | `01_MOUNT_chipboard/power_score_board_165mm.pdf` | 1 | 165 mm (6.5") | poster/adhesive paper → **2 mm chipboard** | **YES** |
| Player aids | `02_CARDSTOCK/player_aid_190mm_print5.pdf` | 5 | **190 mm (7.5")** | **cardstock** (110 lb cover) | no |
| Element/platform tokens | `02_CARDSTOCK/tokens_{Green_Red,Purple_Blue,Yellow_Dark}_11x17.pdf` | 3 sheets | Ø37 mm tokens | **cardstock** | no |
| Mutation cards | `02_CARDSTOCK/mutation_cards_26_round92mm.pdf` | 26 (13 pp) | **Ø92 mm round** | **cardstock** | no |
| Box wrap | `03_BOX/box_wrap_457mm.pdf` (+ `_VECTOR.pdf`) | 1 | 457 mm net | poster/adhesive paper → box | wrap |
| Rulebook | `04_RULEBOOK/rulebook_18pp.pdf` | 1 | 223 mm booklet | **saddle-stitch booklet** | no |

**The main board is ONE double-sided board — pentagon on one face, hex on the other.** Print both and mount
them **back-to-back on a single chipboard** (see §4). So **2 boards get mounted** (the double-sided main +
the power board); everything else is cardstock. No stickers required for gameplay — adhesive paper is just the
*cleanest way to mount* the boards.

### Where the sizes come from (so they're right, not guessed)
Every size above is anchored, not assumed: **boards** = `27_*_54cm` filename + `BOARD=540` in
`pieces/build_setup_scene.py`; **player aid 190mm & power board 165mm** = that same real-mm scene
(`plane("PlayerAid",190,190)`, `plane("PowerBoard",165,165)`); **rulebook 223mm & box wrap 457mm** = the
trim size embedded in the vector PDFs (`02_Manual_18.pdf`, `Packaging_Print_01.pdf`); **mutation cards** =
measured round art (~1400px) → Ø92mm at 386 dpi, consistent with the scene's `92`. (No *filled* manufacturer
spec was found — `Component Specifications.xlsx` in Downloads is the blank Panda template. If you locate the
filled sheet, it wins; these are the best on-disk sources.)

---

## 2. Quality — killing the "grainy / streaky" look

That look is almost never the file — it's the **machine + paper**. This art is **flat vector color** (no
gradients), which prints beautifully *if*:

1. **Coated stock, not uncoated.** Uncoated paper soaks up ink unevenly → dull + grainy. Ask for **silk/matte-
   coated** (or gloss). Coated = saturated, solid, even color. This is the single biggest lever.
2. **A real print shop machine**, not a tired office copier. For flat color, a **production color laser**
   (Xerox/Canon/Konica) lays down dead-flat solids; a good **inkjet/giclée** also works. Either is fine —
   avoid the beat-up self-serve copier with toner streaks.
3. **Send PDF, not JPEG.** Our PDFs wrap lossless PNGs — no compression speckle. (PNG masters are in
   `~/Downloads/organism/prototype/` if a shop asks for images instead.)
4. Say the magic words at the counter: **"100% scale, highest quality / photo setting, coated stock."**
5. **Matte finish** hides any faint banding and glare (recommended for boards you'll photograph); gloss pops
   more but shows fingerprints. Your call.

If a proof still looks streaky, it's their machine — try a different shop or ask them to run it on the
production printer, not the copier.

---

## 3. Where to print

**Fastest / DIY (what you asked about — FedEx/UPS/Staples):**
- **Cardstock items** (tokens, aids, mutations) + **rulebook booklet** → **FedEx Office / Staples / Office
  Depot**. Ask for **110 lb (300 gsm) cover, coated**; booklet = "saddle-stitch, 8.5×11 folded to 5.5×8.5"
  (or whatever the manual imposes to).
- **Main board** → FedEx **large-format / poster** dept prints the single 540 mm PDF on **matte poster paper**
  (they go to 36" wide), OR run the **4 tile PDFs** on 11×17 cardstock and seam them. Large-format single =
  cleaner; tiles = works on any printer and matches the fold panels.
- **Chipboard**: buy separately — 2 mm **greyboard / davey board / bookbinding board** at an art store (Blick,
  Michaels) or online. This is what you glue the printed board to.

**Premium / "real game" feel (mail-order, more $ + lead time):**
- **The Game Crafter** — purpose-built for this: mounted boards, **punch-out token boards**, round cards,
  custom boxes. Upload art, they make components. Best "legit" result with least hand-cutting.
- **MakePlayingCards (MPC)** — great custom **round cards** + tokens + decks (linen finish).
- **PrintNinja / QinPrinting** — bulk/offset if you ever go past one copy.

For one solid prototype you cut yourself, **FedEx (cardstock + booklet + large-format board) + art-store
chipboard** is the move.

---

## 4. Mounting the boards (chipboard) — pentagon, hex, and power

**Cleanest method — adhesive-backed print:**
1. Print the board on **self-adhesive matte poster paper** (large-format shops offer this; ask for
   "adhesive-backed" / "peel-and-stick").
2. Cut a piece of **2 mm chipboard** slightly larger than the board.
3. Peel a few inches, line up an edge, and roll it down slowly with a squeegee/credit-card to avoid bubbles.
4. Trim to the artwork edge with a sharp craft knife + metal ruler (multiple light passes, fresh blade).

**Alternative — spray mount:** print on normal matte paper, coat the chipboard with **3M Super 77**, apply,
burnish, trim. Cheaper, slightly more finicky (work fast, it grabs).

**Do this for BOTH main boards** (pentagon + hex) and the power board — three mounted boards total.

**Main board fold:** each board quad-folds into four ~270 mm quadrants — exactly the 4 tile PDFs
(`main_board_PENT_tiles_11x17/` and `main_board_HEX_tiles_11x17/`). Mount each quadrant on its own chipboard
panel, lay them out with a ~3 mm gap, and **hinge the backs with bookbinding/linen tape** → a real folding
board. (Or keep it one rigid piece if you printed the single large-format version.)

---

## 5. Tokens, cards, aids (cardstock)

- **Tokens** (`tokens_*_11x17.pdf`): print on cardstock, cut the **Ø37 mm** circles with a **circle cutter**
  or a 1.5" punch. Want a chunky poker-chip feel? Glue the sheet to **1.5 mm chipboard** first, then cut.
- **Mutation cards** (`mutation_cards_26_round92mm.pdf`): 13 pages, 2 round cards each → cut **Ø92 mm** circles.
  Sleeve or corner-round if you like.
- **Player aids** (`player_aid_190mm_print5.pdf`): print **5×** at **190 mm (7.5")**, cut round. Laminate optional.

Cardstock weight: **110 lb cover (300 gsm)** is the sweet spot — stiff, cuts clean, feels like a real
component. Coated for color pop.

---

## 6. Box

- Print `box_wrap_457mm.pdf` (the full net: center = top, 4 flaps = sides) on **adhesive poster paper**, wrap it
  onto a rigid **chipboard box** (build one, or buy a blank set-up box near the box size and wrap it).
- `box_top_VECTOR.pdf` is just the top face if you want the lid separately.
- Prefer the `_VECTOR.pdf` versions if your shop accepts vector PDFs — crisper text/edges than the raster wrap.

---

## 7. One-trip checklist

**To the print shop (all at 100% scale, coated stock):**
- [ ] `main_board_PENT_540mm_FULL.pdf` **and** `main_board_HEX_540mm_FULL.pdf` — large-format, **matte adhesive poster paper**  *(or each board's 4 tiles on 11×17 cardstock)*
- [ ] `power_score_board_165mm.pdf` — adhesive poster paper
- [ ] `tokens_{Green_Red,Purple_Blue,Yellow_Dark}_11x17.pdf` — 110 lb coated cardstock, 11×17
- [ ] `mutation_cards_26_round92mm.pdf` — 110 lb coated cardstock
- [ ] `player_aid_190mm_print5.pdf` — ×5, cardstock
- [ ] `box_wrap_457mm.pdf` — adhesive poster paper
- [ ] `rulebook_18pp.pdf` — saddle-stitch booklet

**From the art store:**
- [ ] 2 mm chipboard/greyboard (boards) · 1.5 mm (optional thick tokens)
- [ ] bookbinding/linen tape (board hinge) · 3M Super 77 (if not using adhesive paper)
- [ ] fresh craft-knife blades · metal ruler · circle cutter · cutting mat · bone folder (box)
