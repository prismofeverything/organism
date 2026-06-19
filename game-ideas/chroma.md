# Chroma — Rules

Abstract area-influence game: stack translucent colored chits on a backlit board,
harvest a new chit of the color each stack now glows, and score the chits you held
back against the largest color regions you helped build.

**2–6 players · ~30–40 min · backlit board + translucent chits.** This top section is
the **canonical, current ruleset**. Everything below the "Design history" divider is the
chronological design log — useful background, but *non-canonical*; where the log and these
rules disagree, **these rules win**.

## How to play (the core loop)

On your turn you place one chit onto a stack; the stack re-blends to a new color, and you
draw a chit of that color. Chits left in your hand at game end score against the biggest
same-color regions on the board. The tension: you spend chits to build the regions that set
your score, so every chit is *hold-for-points* vs *spend-to-build*.

## Components

- **Board:** a backlit hexagon of hexagonal recessed spaces. Current default is an
  **edge-6 board of 91 spaces** — 90 playable + 1 neutral center — partitioned into
  **6 equal wedges** of 15 spaces each. (Board size by player count is still being tuned —
  see "Open for your call" below.)
- **Center hub:** the single black center space is a **neutral, non-playable pivot**. The 6
  color-wheel cells around it *are* playable.
- **Depth:** every space holds up to **3 chits** (depth = 3). A space filled to 3 is
  **locked** — no further placement.
- **Chits:** 6 colors — three primaries **Cyan, Magenta, Yellow** and their pairwise mixes
  **Blue (C+M), Red (M+Y), Green (C+Y)**. No clear/wild/blackout chit. (This is the digital
  CMY palette; the physical chit palette is pending the calibration swatch shoot.)
- **Supply bag:** **30 chits per color (180 total), drawn without replacement.** Seeds and
  starting hands come out of the bag; it is never refilled.
- **Starting hand:** **6 chits**, hidden, drawn from the bag.
- **Seeding:** the board starts with **49 spaces pre-seeded** (1 chit each, depth 1) along
  the inscribed-triangle lines and color-wheel cells; the other 42 start empty/white. Seeded
  spaces are fully playable (you stack on the seed; the seed counts as 1 of the 3 depth).

## Mixing

Chits mix **subtractively, like paint** (settled by physical experiment, 2026-06-07):
Cyan+Magenta→Blue, Magenta+Yellow→Red, Cyan+Yellow→Green; a stack that blends too dark/grey
is **mud**. A space's color is the live blended disc of its whole stack. The exact stack→color
results come from a **calibrated lookup table**, not a clean formula (the digital mock uses a
provisional table pending the swatch shoot).

## A turn (simultaneous, rotating wedges)

All players act **simultaneously each turn**, each confined to the one wedge they may touch
that turn. Wedge access **rotates +1 each turn**, so everyone cycles the whole board. Then:

1. **Place** exactly one chit from your hand onto a legal space in your current wedge
   (mandatory — see passing rules). A legal space is any non-locked space in your wedge.
2. **Re-blend:** the stack takes on its new blended color.
3. **Draw (resolved simultaneously):** if the new blend is one of the 6 colors, you draw 1
   chit of that color. **Draws resolve together:** if the bag can't supply *every* player who
   wants a given color this turn, **no one** gets it that turn (it stays in the bag). If the
   blend is **mud**, you draw nothing.
4. **Optional swap (simultaneous):** you may make **one** color-wheel swap — discard 2 of the
   *same* color for 1 of its wheel-**opposite**, or 2 *different* (two-apart) colors for the 1
   color **between** them. Net −1 chit. The gained chit obeys the same simultaneous
   conflict-denial rule as a draw.

### Two placement restrictions

- **No same color as your last turn.** You may not place a chit of the color you placed on
  *your* previous turn. (Hard rule: if your whole hand is that color you have no legal
  placement and must skip — you may still swap to rebuild and re-enter.)
- **No consecutive passing.** You may not voluntarily pass two of your own turns in a row. The
  only legal back-to-back pass is a **forced** one (you hold 0 chits, or the no-same-color rule
  leaves you no legal placement). With ≥1 chit and a legal placement, a second straight pass is
  illegal — you must place.

A player who runs to an **empty hand** is *not* eliminated and triggers no endgame: they simply
keep passing and are skipped. (Deliberately un-rewarded so nobody games it.)

## Endgame — the dry clock

A turn is **"dry"** when you draw nothing: the blend was **mud**, OR the result color's pile is
**empty**, OR the draw was **denied by conflict**. Each dry turn costs you the chit you placed
(hand −1) and advances a shared clock. When **any player reaches their 3rd dry turn**, one final
simultaneous turn is played, then the game scores. (An emptied bag is a backup end condition that
rarely binds.) Mud-avoidance is the core skill that lengthens the game.

## Scoring (rule "op2b")

At game end, only **chits still in hand** score (board chits just set region values). For each
color, find its **largest contiguous same-color region** on the board (hex 6-neighbor
adjacency; mud and empty/white spaces break contiguity and don't score).

- A color's largest-region size scores for the **single player holding the most chits of it.**
  A sole leader takes the full region; on a tie for most, each tied player scores that color's
  **second-largest** region instead. A color nobody holds scores for nobody.
- **Mud** is a single bonus "region" scored by the player with the **fewest chits in hand**
  (sole-fewest → all mud cells; tie → the largest contiguous mud region each). It breaks ties
  between close players rather than dominating.
- **Tiebreaker:** if players tie on total score, the winner is the one who scored the
  **most distinct region types** (colors with value > 0, plus mud if scored).

## Quick reference

| Knob | Value |
|---|---|
| Players / time | 2–6 / ~30–40 min |
| Board (current default) | hex edge-6, 91 spaces (90 playable), 6 wedges × 15 |
| Depth (stack height) | **3** — full stack locks |
| Palette | CMY + RGB: Cyan, Magenta, Yellow, Red, Green, Blue (no clear chit) |
| Supply | 30 per color, 180 total, no replacement |
| Starting hand | 6, hidden |
| Seeding | 49 seeded / 42 empty |
| Turn | simultaneous; rotating wedge (+1/turn); place 1 → draw result → optional swap |
| Adjacency / scoring | hex 6-neighbor; op2b (most-holder takes largest region) |
| Endgame | dry clock — 3rd dry turn (any player) → 1 final turn |

## Open for your call (not yet canonical — awaiting Mohammad)

These weren't settled enough to bake in as rules; the design log treats them as live:

- **Board size by player count.** The mock runs one edge-6 (91-space) board for all counts;
  §24–26 found it's too roomy for depth-3 stacking to bite, and you leaned toward removing
  spaces. A trimmed-corner board (9 blanks → 81 spaces) exists behind a flag but isn't adopted.
  No per-count size table is settled.
- **Player→wedge mapping for 4 and 5 players** (2/3/6 space evenly on 6 wedges; 4 and 5 don't).
- **Physical chit palette (RYB vs CMY).** Digital default is CMY; the physical material is still
  pending the calibration swatch shoot, and available translucent sets nudge toward RYB.
- **Mud-bonus formula** is marked provisional ("expected to change after testing").

---

# Chroma — design notes (history, non-canonical)

> Everything below is the chronological design journal that produced the rules above. It is
> kept for context and rationale. It records decisions **as they were made**, so earlier
> entries are superseded by later ones and by the canonical rules at the top — do not read any
> single entry below as the current rule.

*Working title. Abstract puzzle / area-influence game about stacking translucent
colored chits on a backlit board and harvesting the blended light.*

Status: brainstorming. Owner: Mohammad. Started 2026-06-07.

---

## 1. The pitch (one sentence)

Play a translucent chit onto a backlit stack, harvest a new chit of the color the
stack now glows, and slowly transform both your hand and the board — then score the
chits you held back against the biggest contiguous color regions you helped build.

## 2. Core loop

1. Choose a chit from hand.
2. Place it on a stack (a recessed space holding 0..depth chits).
3. The stack now glows a blended color (the light transmitted through the whole stack).
4. Draw a chit of that blended color from the resource pool into your hand.
5. Board composition shifts → everyone's future outputs shift.

**Tension:** chits in hand at game end are worth points, but you spend chits from
hand to build the board regions that *set* those points' value. Hold vs. spend.

## 3. Scoring

- At game end, each color has a value = size of the **largest contiguous group of
  that color currently on the board**.
- Each chit left in a player's hand scores its color's value.
- Endgame board state captured by **phone photo → app classifies each space's color
  → computes per-color largest region → multiplies by hand counts**.

---

## 4. Mixing model — SETTLED BY EXPERIMENT (2026-06-07) ✓

Mohammad built a physical single-stack rig (backlight + real translucent chits) and
photographed it. Photos in `chroma-photos/` (zip: `choma-color-photos.zip`).

**Result: real stacked chits mix like PAINT (broadband subtractive), NOT like idealized
dichroic filters.**
- Red + blue, fully stacked → **purple** (not black). [PXL_…823]
- Red / blue partial overlap → red · purple · blue Venn. [PXL_…832]
- Blue + green overlap → **teal/cyan**. [PXL_…845]
- Singles (blue / red / purple) glow clean and distinct. [PXL_…747/754/801]

### Why (so we reason correctly from here on)
My earlier "red filter + blue filter = black" assumed *ideal narrow-band* filters that
pass one channel and block all else. Real colored cellophane/vellum/tissue are
**broadband partial absorbers** — each passes a wide soft hump of spectrum. Stacking
multiplies two broad transmission curves, and red×blue still leaves a violet band →
purple. The non-ideality is the feature; cheap real materials behave like a painter's
wheel. So the intuitive "red+blue=purple" feel, *real* optics on the board, and
camera-readability all come for free — there is **no fork**.

### Implications
1. **Calibrate, don't derive.** Mixing is a continuous space. The draw-table
   (stack → which color you pick up) and the phone classifier must both be built from
   photographs of the *actual* chits, not from a clean algebra. → see Next Steps.
2. **Depth still degrades, gently.** Many paint-like layers drift toward muddy
   brown/grey rather than pure black — a softer "board ages over the game" arc. The
   degradation budget survives, so depth (Q1) still matters.
3. **Palette is chosen for closure + separation** (see Q2): pick colors so that the
   mixes you want to be *productive* land on another palette color, while *clashing*
   mixes go muddy ("dead"), and so base colors AND their common mixes are camera-distinct.
4. **Recess = uniform disc.** Full overlap in the recess yields one clean blended color
   per space (photo 4), much easier for the camera than loose overlaps (photo 5). Keep it.

### Notes from the photos to design around
- Chits read **pale / light-leaky** (purple → lavender, red → orange-red). Beautiful
  glow, but: add a **white-reference patch** to the board for camera white-balance, and
  test whether saturation/hue survives at depth 3–4 (likely the practical depth ceiling).
- Prototype digitally with a **measured lookup table** (from the calibration shoot),
  not an idealized formula.

---

## 5. Open questions & options

### Q1 — Recess depth — RESOLVED: depth = 3 (see canonical rules)
**Depth is no longer an open question.** Playtests (§16) showed stacks rarely reached 4, so
depth was set to **3** and is uniform across all spaces. The "4 vs 5" debate and the
per-space-varying-depth option were both dropped. Historical framing below.

Depth = how many layers before a stack saturates to mud; it's the "how hard is it to keep a
color clean" dial and also sets game length (capacity = turns). The original open options were
depth 4 (faster saturation, shorter game) vs depth 5 (more room to repaint, longer arc), or
depth varying by space — all superseded by the depth-3 decision.

### Q2 — Color set (now driven by the paint-wheel reality)
Pick 6 base colors so that (a) common mixes land on another palette member ("productive")
while clashing mixes go muddy ("dead"), and (b) bases + their common mixes stay
camera-distinct. Two candidate frames to test in the calibration shoot:
- **Artist wheel {Red, Orange, Yellow, Green, Blue, Purple}:** matches what cheap
  cellophane comes in and the user's tested chits (red/blue/green/purple already mix
  intuitively). Neighbor mixes = the color between them; opposites = mud.
- **Print-style {Cyan, Magenta, Yellow} + their mixes {Red, Green, Blue}:** CMY are the
  "playable primaries," pairwise mixes give RGB, all three → muddy/black. Cleaner closure
  but may not match available physical chits.
Plus **Clear** (gentle/no-op filter). **No special/blackout chit — cut 2026-06-07.**
Watch luminance gaps (yellow bright, blue/purple dark) and add a **white-reference patch**
for the camera. DECISION DEFERRED to calibration data.

### Q3 — Placement legality (mix & match)
- **(a) Unrestricted:** any chit on any non-full stack.
- **(b) No-redundant:** illegal to play a chit that doesn't change the stack color
  (bans "R-on-R" free farming; forces meaningful change).
- **(c) Neighborhood requirement:** the played color (or resulting color) must already
  be present in an adjacent space. Pushes clustering → directly feeds contiguous scoring.
- **(d) Draw-gated:** legal only if the resulting color is still available in the pool.
- **(e) Lock-when-full:** a depth-max stack is sealed; full black stacks become walls.

### Q4 — Special / blackout chit: CUT (2026-06-07)
Dropped entirely. System stays pure: 6 colors + Clear. Considered a constructive "wild"
but cut to keep it clean; revisit only if playtests want a release valve.

### Q5 — Camera scoring requirements
Feasible with controlled backlight. Needs: corner fiducials (perspective), a white-
reference patch (white balance), known grid geometry, 8-way hue/value classifier. The
backlight is the feature that makes this reliable (kills ambient color cast).

### Q6 — Board geometry & size
- **Square grid:** easiest camera + adjacency (4-neighbor). 7×7=49.
- **Hex grid:** 6-neighbor adjacency = richer regions, thematically nods to 6 colors.
  Hexagon of side 4 = 37 spaces.
- Scale with player count via modular tiles (add a ring/tile per extra player).
- Capacity = spaces × depth; tune so the board fills ~60–80% by endgame, not 100%.

### Q-RESOLVED (2026-06-07)
- **Forced placement:** on your turn you must place exactly one chit — no more, no less.
- **Space color = the live blended disc** of the whole stack.
- **Brown/muddy is not a drawable color:** when a stack blends to mud, you take NO chit
  and it has NO score value. Mud still matters — it reshapes region boundaries (a wall).

### Q7 — Resource pool
Limited (scarcity, draws can fail → tension) vs. effectively infinite (smoother).
If limited: what happens when you can't draw the result color? (skip / wild / forced black?)

### Q8 — Player count & time (2–6, 30–40 min)
6p in 40 min ≈ very tight turns. Consider **simultaneous selection** or a short hard
turn timer. Decide endgame trigger: fixed rounds / pool exhausted / board X% full / a
player empties hand.

---

## 6. My open follow-up questions (for Mohammad)
1. Do you want the *intuitive* "red+blue=purple" feel, or the *physically real* AND
   feel? (This is the fork that everything else hangs on.)
2. Is the board's degradation-toward-black a feature you like (dramatic arc) or a bug
   to design around?
3. For "a space's color" in region scoring — is it the live blended top color, or some
   stable attribute? Does a stack going black erase it from every color's region?
4. Can a player place onto *any* stack, or only ones they have some claim to? Is there
   ANY ownership/territory layer, or is the board fully communal?
5. What stops a player from just hoarding their whole hand and never building? (Hand
   size cap? Forced placement each turn? Draw only happens via placement — so hoarding
   = never drawing = fine, but then what pressures them to spend?)
6. Is there hidden information (concealed hands) or fully open? Open hands make it a
   pure abstract; hidden hands add bluff/denial.
7. Win by most points only, or any tiebreak texture (e.g., most distinct colors)?

---

## 7. Next steps

1. **Calibration shoot (highest value).** Lock a candidate 6-color palette, then on the
   real backlight photograph: every single chit, every pairwise stack, and a few depth-3/4
   stacks. This produces (a) the measured mixing/draw table and (b) the data to test how
   camera-separable the colors + mixes are. Everything else depends on this.
2. **Decide depth (Q1)** using the shoot — find where stacks turn unreadably muddy; set
   max depth one below that.
3. **Digital prototype.** Build a mock that uses the *measured* lookup table (not a clean
   formula) so we can feel the core loop, then bolt on placement-rule variants (Q3) to
   playtest which one makes the contiguous-region scoring sing.
4. **Camera-scoring spike.** Small script: photo → fiducial/perspective correction →
   white-balance off reference patch → classify each space → largest-region-per-color →
   score. Prove feasibility on a hand-built endgame board.

### Purchasing plan (decided 2026-06-07)
Material = **theatrical/photographic lighting gels**, cut into uniform squares (~2.5 cm;
~50 chits per 8×10″ sheet). Plan: **cheap 6-color pack now + a swatchbook to fine-tune
the palette later.**
- **Must include true Cyan AND Magenta** (not orange/pink/purple) — many "6-packs" are a
  photographer's palette and skip cyan; those don't fit the CMY framework.
- **Buy on Amazon/Prime** (free shipping). Specialty lighting dealers (B&H, filmandvideo-
  lighting) charge shipping that can be 3× a $6 swatchbook. B&H free only over $49.
- Brands to check for the 6-pack: Selens, EXMAX, LimoStudio, Neewer (confirm in-stock +
  C/M present — earlier Neewer/Jeasun ASINs were dead). Avoid Paul C. Buff set (no cyan).
- Also buy: an **LED A4 tracing pad** (even neutral high-CRI backlight) for glow + camera.
- NEXT after it arrives: calibration shoot → then return to the rules set.
- **Swatchbook PURCHASED 2026-06-07.** (6-pack play material still TBD.)

---

## 8. Emergent properties of the locked-in physics (reason from these)

These aren't decisions — they fall out of "subtractive mixing + add-only stacks." They
should steer the open rule choices below.

1. **Add-only, order-independent, no take-backs.** Mixing commutes, so a stack is just a
   *multiset* of chits (position never matters). Chits are only ever added — never removed.
   Stacks only darken/shift; **mud is permanent**. No recovery from a mistake or a griefed
   space. Clean and tight, but unforgiving — confirm that's the intended texture.
2. **Locking = protection (argues for "full stack locks").** If a depth-full stack can't
   be played on, the way to *defend* a color region is to fill its stacks to max depth
   while they're still your color — opponents then can't mud them. The game becomes a race
   to **build-and-lock your regions before rivals clash them.** Strong, elegant; leans
   toward adopting rule (e).
3. **Clear = renewable harvest (collides with "no-redundant" rule).** Playing a Clear
   doesn't change a stack's color, so you *draw that color again without degrading the
   stack* — a sustainable harvest. BUT the "no-redundant placement" rule (b) would ban
   exactly this. So Clear's identity and rule (b) are mutually exclusive — pick one.
4. **Your hand chases what you build.** Forced place→draw-the-result means your hand fills
   with the colors you're building. To *hoard* a color for scoring you must stop building
   it (and lock its region to protect it). This self-balances against runaway specializing.
5. **The board filling up is a natural game clock.** Mandatory 1 placement/turn + lock-on-
   full ⇒ board fills monotonically. "End when board is full" is the cleanest endgame
   trigger. Size the board for ~120–150 total placements ≈ 30–40 min, and **scale board
   size with player count** to hold that target.

---

## 9. Rule-design checklist (open questions to resolve)

### Q-RESOLVED (2026-06-07, batch 2)
- **Hidden hands.** You see opponents place & draw, but not their held colors.
- **Mostly constructive** interaction (denial exists but isn't the main lever; board is
  more shared canvas than battlefield; scales to 5–6p).
- **Scoring math: decide via playtest** — mock must compute BOTH multiplicative
  (count × largest region) and additive/tiered, to compare feel.
- **Hex grid (6-neighbor).**

Consequences of batch 2:
- *Hidden but inferable:* draws are public (stack result is visible), so a sharp opponent
  can track roughly what you're holding. Hidden = scoring **intentions**, not raw info.
  → don't also hide draw counts; the inference game is a feature.
- *Constructive tone reopens the Blackout chit:* a one-shot "mud any stack" nuke is a pure
  attack and clashes with the constructive feel. Candidates: reframe as a constructive
  **wild** (counts as any one color, to complete/lock your own region) or a personal
  **lock**, or cut it. NEEDS A DECISION (see C1).
- *One board can serve all player counts on TIME:* with "end when board full," total
  placements = board capacity, independent of player count, so game length is fixed by
  board size (~148 placements at hex side-4 × depth-4 ≈ 30–40 min). Scale board size with
  player count not for time but for **per-player agency** (~30–40 placements each):
  2p ≈ side-3 (19), 3–4p ≈ side-4 (37), 5–6p ≈ side-5 (61, maybe depth-3).

**A. Setup**
- A1 Board start: empty (all clear/white) vs **seeded** (each space 1 random chit) for
  instant geography? (lean seeded)
- A2 Starting hand: size + composition (random / fixed-identical / drafted)?
- A3 Pool model: deterministic supply you take the result color from (matches the loop).
  How many of each color? Communal market vs per-color piles?
- A4 Hand-size limit? (place 1 / draw ≤1 keeps hand ~constant; mud shrinks it.)

**B. Placement legality (THE core rule — playtest variants)**
- B1 Which of: (a) unrestricted, (b) no-redundant, (c) neighborhood-required,
  (d) draw-gated, (e) lock-when-full? (e looks near-mandatory; pick among a/b/c/d on top.)
- B2 May you place on a mud stack (pure dumping)? On a locked/full stack (no)?
- B3 Communal board vs any ownership/territory layer? (lean communal)
- B4 Is deliberately making mud (denial) legal? (presumably yes — key tactic)

**C. Special chit — RESOLVED: cut entirely.**

**D. Scoring**
- D1 Adjacency: orthogonal (square) vs 6-neighbor (hex)? (ties to F? no — to geometry)
- D2 Score math: **multiplicative** (hand count × largest region) vs additive? (mult =
  swingy, rewards specialization; matches your phrasing)
- D3 Does white/clear score, or is it the neutral non-color? (lean: not scored)
- D4 Hold a color with no region on board ⇒ value 0 (confirm).
- D5 Board chits never score for anyone; only hand chits do (confirmed). Region value =
  count of spaces of that color (not chits).

**E. Endgame & pacing**
- E1 Trigger: board full (lean) vs fixed rounds vs pool exhausted?
- E2 Turn structure: sequential (clean, may drag at 6p) vs simultaneous-select (fast, needs
  conflict resolution)?
- E3 Board geometry + size table per player count (scale to ~120–150 placements).

**F. Information & interaction**
- F1 Hands **hidden** (bluff/denial, less AP) vs **open** (pure perfect-info abstract)?
- F2 Interaction level: cutthroat denial (mud rivals, steal regions) vs more
  multiplayer-solitaire? Sets the game's whole personality.

**G. Edge cases**
- G1 Pool empty for the result color ⇒ draw nothing / wild / substitute?
- G2 Board full (can't place) on your turn ⇒ end / skip?
- G3 Empty hand on your turn ⇒ eliminated / triggers end? — **RESOLVED 2026-06-14:
  deliberate DEAD END.** A player at 0 chits is NOT eliminated and triggers NO end;
  they simply keep passing and are skipped, and the existing dry-clock / empty-bag
  end conditions close the game on their own. No random-chit handout, no elimination.
  This is intentional and left un-rewarded so nobody is incentivized to force a low/
  empty hand to pass every turn — see the no-consecutive-pass rule below.
- G4 Always draw if a real color results (no opt-out)? (lean yes)

---

## 10. v0.1 working ruleset (DRAFT — disposable, 2026-06-07)

Synthesises every decision so far; for the still-open knobs I picked a default so we have
something playable to mock. **Everything here is up for revision.**

**Components**
- Hexagonal board of hex recessed spaces; size scales with player count (2p ~19 / 3–4p ~37
  / 5–6p ~61 spaces). Each space holds up to DEPTH chits (default 4).
- Backlit (LED pad). Hex chits in 6 colors {Red, Green, Blue, Cyan, Magenta, Yellow} +
  Clear. No special chit. Shared supply piles, one per color.

**Board design (Mohammad's, 2026-06-07) — recorded**
- Macro board = hexagon; chits = hexagons (shape rhymes with the 6 colors).
- Inscribed equilateral triangle: 3 sides = the 3 primaries; 3 vertices = the secondary
  composite of the two sides meeting there → all 6 colors diagrammed.
- Center = black dot ringed by the 6 colors as a wheel (the "all → black" sink).
- All playing spaces start WHITE/open; any chit may be placed on a white space.
- ⚠️ OPEN — is this printed geography purely **decorative + mixing legend**, or does it
  carry **gameplay function** (scoring zones / placement bonuses tied to where a color
  sits on the board)? Big branch — needs Mohammad before the mock locks scoring.

**Setup**
- All spaces empty/white. Each player draws a hidden starting hand of H chits (H TBD,
  ~5–7). Supply piles set out.

**Turn (sequential, mandatory)**
1. Place exactly one chit from hand onto any legal space.
2. The stack re-blends (subtractive, per the calibrated table).
3. If the new blend is one of the 6 colors → draw 1 chit of that color from supply into
   hand. If it's mud → draw nothing. (Placing Clear leaves the color unchanged → you draw
   that same color again = a renewable harvest.)

**Placement legality (v0.1 default; variants to playtest later)**
- Legal = any space not full. A DEPTH-full stack is LOCKED (no more plays) → locking is
  how you protect a region.
- Placing on a mud stack is allowed (stays mud, draw nothing) = dumping.
- Variants to test: (b) no-redundant [NB: bans the Clear-harvest]; (c) neighborhood — the
  color you make must already exist in an adjacent space.

**Endgame** — board fills / no legal placements remain (natural fill-up clock).

**Scoring (mock computes BOTH for comparison)**
- Hex 6-neighbor adjacency. A color's region = largest contiguous set of spaces whose
  current blend is that color. Mud and white break contiguity and don't score.
- Multiplicative: each held chit scores its color's largest-region size.
- Additive/tiered: region size → per-chit value tier; held chits add up gently.
- Only HAND chits score; board chits only set region values.

**Open knobs still to tune**
- DEPTH 4 vs 5 (set after calibration shows where stacks go unreadably muddy).
- Starting hand size H; supply counts; is Clear drawable or hand-only?
- Placement variant (a/b/c); scoring math (mult vs additive); board-art function.

---

## 11. Board mockup + palette implication (2026-06-07)

- Rendered `chroma-board-mockup.png` via `render_board.py`. Hex-of-hexes, **edge = 6
  cells = 91 spaces**. Pointy-top cells, flat-top board. 6 wheel-colored corners,
  inscribed triangle (vertices = secondaries Purple/Orange/Green; sides tinted primaries
  Red/Yellow/Blue), central black hub + color-wheel ring. Mohammad counting white-space
  regions before answering the board-art-function question (§10).
- The inscribed triangle splits the board into **4 regions**: 1 central triangle + 3 outer
  corner-triangles (each holding a primary corner). Edge=6 may change after counting.
- **PALETTE IMPLICATION:** the triangle's "3 primaries on sides + 3 secondary composites
  at vertices" is the **RYB artist wheel {Red, Yellow, Blue / Orange, Green, Purple}**,
  not {R,G,B,C,M,Y}. Matches the physical experiment (red+blue=purple, blue+green=teal).
  Leans Q2 toward the artist wheel — confirm at calibration.

---

## 12. Rotating-regions mechanic + seeding (2026-06-07)

**Big mechanic decision (Mohammad):** simultaneous turns + rotating exclusive regions.
- Each turn every player places 1 chit at the same time, each confined to a region only
  they can touch that turn. Region access **rotates** each turn so all players cycle the
  board. Motivation: kill downtime + defuse cutthroat stress.
- Precedent: Photosynthesis (rotating sun = rotating directional access on a hex board).

**Consequences:**
- No continuous territory ownership → regions are built collaboratively; "build-and-lock"
  becomes a *race across rotations*, not private camping. Real competition shifts to the
  **hidden hand** (bet on which colors dominate the shared board). Softer, less cutthroat.
- Near-zero same-turn conflict (exclusive regions) + near-zero downtime (simultaneous).
- Only interaction vector = setting up / sabotaging the region the NEXT player rotates into.

**The "middle problem" + solution:** 6 wedges partition the rim cleanly but collide at
center. FIX: make the **hub (black center + its 6 wheel cells) a neutral, non-playable
monument** — rotation only covers the outer board, where wedges are clean. Cost: those 7
central cells (incl. the wheel) are no longer playable.

**Seeding (Mohammad's design):** pre-place 1 chit (depth-1) in: black center; the 6 wheel
cells; every cell each triangle line passes through (in that line's primary color →
**1–2 tiling**); triangle-vertex corners get their secondary color. Render
`render_board.py` v2: edge-6 board ⇒ **49 seeded / 42 white** (triangle diagonals are long).
- **DEPTH = 4** (leaning). Seeded spots start with their seed in, so they "fill deeper,
  sooner." OPEN: do seeded spots get capacity 4 total (seed = 1 of 4) or seed **+4 = 5**
  ("virtual 5 deep")?

**Open rotation questions:**
- Hub neutral/non-playable — confirm?
- Seeding density — full-edge lines (49 seeded) vs shorter segments (more white canvas)?
- Wedge count = 6; how do 2–5 players map onto 6 wedges (spaced/opposite)? rotation
  direction? endgame = fixed #rotations vs board-full?
- Wedge geometry: fixed wedges with rotating *access*, or the wedge shapes rotate?
- Uneven wedges in v2 render (white per wedge 9/5/9/5/9/5) → real partition should balance
  (pinwheel) — cosmetic for now.

### Hub question RESOLVED (2026-06-07)
Pinwheel partition is already clean: **15 cells/wedge**, and the 6 cells touching the
black center each belong to a different wedge (one tip apiece) → **wheel cells stay
playable**. Only the single black center is the neutral pivot. No ambiguous middle.
Also confirmed: **seeded cells are playable** (stack on the seed); **seed depth = seed+4
(=5 total)** for colored spaces, white spaces = 4; **full triangle lines (49 seeded)**.
Note: seeding alternates **6/10 vs 10/6** seeded:white across wedges ("heavy"/"light"),
but each player meets 3 heavy + 3 light per full rotation, so it balances per player.

### Next rotation questions (open)
- Endgame: fixed # of rotations (lean) vs board-full. Capacity is large (~364 open slots),
  so a rotation/turn count is the natural clock, NOT fill.
- Player→wedge mapping: even spacing works for 2 (opp), 3 (every other), 6 (all); 4 & 5
  can't space evenly on 6 → some adjacency. Rotate all by +1 each turn.
- Hand size, supply counts (tunable later).

---

## 13. Digital mock v0.1 (2026-06-07)

Built `chroma-mock.html` (+ `chroma-core.js` pure logic, `chroma-sim.js`-style node tests).
- 2 players: you (P1, cyan wedge) vs **bot (P2, opposite orange wedge)**. Pick a chit →
  click a glowing legal cell in your wedge → both place, draw the color their stack becomes,
  access rotates +1 wedge. Soft end after 4 rotations (24 turns).
- Reuses the real geometry/seeding (91 cells, 49 seeded, 15/wedge), depth (white 4 / seeded 5),
  hidden bot hand, live largest-region readout, both scoring methods (mult + tier) shown.
- **Mixing is PROVISIONAL** (per-channel multiply of pale base RGBs; R+B→P, Y+B→G validated)
  — swap in the calibrated table after the swatchbook shoot.
- Open it with no server: `xdg-open chroma-mock.html` (core loads via relative path).
- Early feel knobs to watch: mud is rare at shallow depth (only ~2/24 in sim); rotations
  target, hand size, and scoring math all trivially tunable in `chroma-mock.html` constants.

---

## 14. Mock v0.2 + palette decision (2026-06-07)

Independent agent audit of the mixing (re-derived in Python, 0 computation bugs): **RYB is
a counterintuitive thicket** (R+G→Yellow, O+P→Red, Y+G→mud; 34 dark-but-colored surprises);
**CMY passes all 7 subtractive-closure rules** (C+M=Blue, M+Y=Red, C+Y=Green, all→black,
prim+opposite→mud). → **Default palette switched to CMY+RGB** (also best camera separation);
RYB kept as a toggle but flagged provisional/messy.

Changes in `chroma-mock.html` / `chroma-core.js`:
- **CMY default**, RYB toggle. Mud gate now **relative-luminance** (REL_LUM_MIN 0.14), not
  peak-channel → fixes "expected mud, drew a color." Mud now ~9/24 turns (was ~2).
- **Depth uniform 4** (seeded cells: seed = 1 of 4). 90×4 = 360 capacity.
- **Finite supply bag: 60/color, NO replacement.** Seeds + starting hands drawn from it;
  draws deplete it; empty color → no draw. Conserves to 360. (CMY seeding is asymmetric:
  C/M/Y ~13–15 each on edges, B/R/G ~2 each → bag starts C45/M47/Y46 vs B/R/G 58 — primaries
  scarcer. Consequence to watch.)
- **Black wedge borders** (yours solid, bot dashed) + white-dot legal markers — no colored
  highlights (board too saturated). Prominent **Turn / Rotation / Bag** tracker.
- Log now names the **initial space color** ("played Magenta on a Cyan space → Blue, drew Blue").
- **In-app draw verifier** (recompute + compare) and standalone `chroma-verify.js`.

**Open design note — "dump least-present chit" felt dominant (Mohammad).** v0.2 adds three
counter-pressures: (1) mud now costs you a hand chit (no draw) → placing to AVOID mud matters;
(2) you can place to FARM the color you're secretly hoarding (pick the cell whose result =
your bet); (3) finite bag → your bet color can run dry. Needs re-test. The random bot won't
reveal strategic depth — consider a smarter bot or hotseat 2p to truly test dominance.

---

## 15. Mock v0.3 — mud clock + playtest confirmations (2026-06-07)

**Confirmed working (no change):** board size feels right (Mohammad: the real lever is
#spaces × hand size, minus duplicates/non-viables); rotation adds good tension; scoring
model is liked.

**Transparent/clear chit: CUT for good** (not needed in play). System = 6 colors only.

**Endgame = MUD CLOCK (replaces fixed rotations).** Mud appears only as stacks mature, so it
times the game naturally. Rule: a placement resulting in mud = no draw (hand −1) AND +1 to
that player's mud count; when any player hits **3 muds**, ONE final simultaneous turn plays,
then score. Sim (random bots): ends ~turn 19 / ~3 rotations. A mud-avoiding player extends it.
Reframes the "awkward, shrinks my hand" feeling as the intended countdown.

**Anti-gaming fallback — CONTOUR RULE (captured, not yet enabled).** Risk: a player rushes a
single stack into mud to yank the clock. Mitigation idea (Mohammad): a stack can't be >1 (or
2) taller than its neighbors, forcing even building. Note: rotation already slows single-stack
rushing (you only reach a given wedge once per 6 turns), so test the mud clock first; enable
the contour rule only if rushing proves real. One-flag add when needed.

Also need a real **draw/tie rule** eventually (mud clock could end on a tie).

---

## 16. Mock v0.4 (2026-06-07)

Playtest-driven tuning:
- **Depth 3** (stacks rarely reached 4). **Bag halved to 30/color** (180 total) — reduces
  variance; practically 60 was way too many pieces.
- **Dry-turn clock unified:** a mud result OR a draw into an empty pile both = no draw,
  hand −1, +1 to that player's "dry" counter; 3rd dry turn (any player) → one final turn.
- **Bag-empty = backup end condition** (rarely binds; dry clock ends games first).
- **Bot hand revealed at game-over**; **log moved under the board**; mud counters relabeled
  "dry". Transparent chit stays cut.
- Sim (random bots): ends ~1–2 rotations via dry clock; bag ~100 left (untouched). Careful
  mud-avoiding play will run longer — mud-avoidance is now the core skill.

**Strategic insight (Mohammad):** playing on BLANK spaces early to expand a single color and
re-draw it (farming) is a strong opening to set the board — players aren't doing it enough.
Confirms the intended engine: claim blank canvas early, mature/clash later. (Good candidate
behavior for a smarter bot.)

**Confirmed working:** mud as scoring penalty (fewer end chits) + as clock; rotation prevents
single-stack rushing for free.

Still TODO: real draw/tie rule; optional contour rule (only if rushing appears); smarter bot
to stress-test whether "dump least-present chit" is still dominant.

---

## 17. Mock v0.5 — 3 players + smart bots + reference table (2026-06-07)

- **3 players** (you + B1 + B2) on evenly-spaced wedges (bases 0/2/4), rotating together.
  N_PLAYERS is a one-line constant. Wedge labels (YOU/B1/B2) drawn on the board; yours SOLID
  black border, bots DASHED.
- **Smart bots:** commit to current majority color as a hidden target; per turn evaluate every
  (legal cell × hand chit) and score: draw-target +8, grows-target-region +2, spend-target-chit
  −4, claim-blank +1, would-be-dry −6, mud −100. ⇒ they farm their target via non-target chits,
  avoid mud, open on blanks. All hands + targets revealed at game-over; winner declared (by mult).
- **Reference table** in UI: rows = current space color (+blank), cols = placed chit, cell =
  resulting color or · (mud). Computed from the live palette (assumes a pure space; deep mixed
  stacks can differ slightly).
- Sim (3 smart bots): ends ~turn 31 / rot 5 via dry clock; bag ~26 left. **Mud-avoidance extends
  the game massively vs random** — confirms it's the core skill and skilled play = full arc.
- Watch: positional variance (first-actor / heavy-seed wedge can rack up dry plays and lag) —
  check fairness across seats.

Validated design loop now: claim blanks early → grow + farm your color → avoid mud → bet your
hand on the biggest region; mud/empty draws are the self-pacing clock.

---

## 18. Remote phone testing setup (2026-06-07)

Chroma is served over the Tailscale tailnet for phone playtesting, decoupled from the
voice-pipeline webhook (so iterating can't break the pipeline).

- **Static server:** `systemctl --user` unit `~/.config/systemd/user/chroma-mock.service`
  runs `python3 -m http.server 8770 --bind 0.0.0.0 --directory /home/m/organism/game-ideas`.
  Enabled + active; Restart=always. Serves the dir LIVE → edit chroma-mock.html / core.js,
  just refresh the phone (no redeploy).
- **URL (tailnet):** `http://100.115.113.111:8770/chroma-mock.html` (tokenless; tailnet-only).
- **Phone shortcut:** added "Chroma" (browser launcher) to `~/voice-pipeline/gen_shortcuts.py`
  (Games category) → regenerated `~/voice-pipeline/shortcuts.json`. The webhook serves the
  updated file live at `:8765/shortcuts?t=<token>`; re-import in the HTTP Shortcuts app once.
- **Phone needs Tailscale ON** (Pixel 8a `100.66.222.39` is often toggled off).
- **Reboot survival:** user service starts on login; for headless/no-login survival run
  `sudo loginctl enable-linger m` (needs Mohammad's sudo).
- Manage: `systemctl --user {status|restart|stop} chroma-mock.service`.

---

## 19. Physical prototype: Icehouse / Looney Pyramids (2026-06-08)

Mohammad is testing **Icehouse pieces** (Looney Pyramids) in the physical mock — translucent
stacking pyramids, immediately available, good over a backlight.

Caveats to read the test correctly:
- **They NEST, not layer** — light path = overlapping angled walls, so mixes read more
  saturated/uneven than flat gels. View straight DOWN through a recess at the backlight for
  the closest "blended disc."
- **Three sizes (1/2/3 pips)** = a free extra dimension. Use ONE size for a clean color test
  (or later make size = filter strength, a possible mechanic).
- **Palette catch:** standard translucent sets are ROYGBV (artist wheel). **Cyan + magenta
  only come in the Xeno translucent set** — so Icehouse availability nudges back toward RYB
  unless Xeno sets are sourced. (We concluded CMY mixes cleaner — confirm what colors are on
  hand before trusting the mix results.)
- Pyramids are tippy; the final piece may still be flat hexes even if Icehouse wins the feel
  test. Icehouse = fast mixing/stacking prototype, not necessarily the production form.

## Remote-test addendum (2026-06-08)
- Added **"Chroma (LAN)"** shortcut → `http://192.168.254.38:8770/chroma-mock.html` for
  same-WiFi testing without Tailscale (LAN IP can change on DHCP).
- Tailscale "connected, not synced" diagnosis: laptop healthy; phone relayed/stale due to
  **Android battery optimization throttling Tailscale** → set Tailscale to Unrestricted
  battery, or foreground the app before launching. Keep the laptop awake during testing.

## Phone import — DONE (2026-06-08)
Chroma imported to the phone via ADB. Method: reconnected wireless ADB by mDNS discovery
(`adb mdns services` → `192.168.254.19:<port>`), served `chroma-shortcuts.json` on the open
static server (:8770), fired the app's import deep link `http-shortcuts://import?url=...`,
tapped OK. Verified Games category = Eridu · Chroma · Chroma (LAN). Also staged to
/sdcard/Download + Dropbox bridge (~/Dropbox/shortcuts/). See [[reference_phone_shortcut_push]].
OPEN: enable HTTP Shortcuts "Automatic Import" (needs frequency choice) → point at webhook
URL for full push→autofill loop.

---

## 20. Draw-conflict rule + GA personality bench (2026-06-08)

Mohammad-approved fixes + a genetic-algorithm pass modelled on the Eridu evolution bench.

**Engine fixes (`chroma-mock.html`, `chroma-core.js` unchanged):**
- **Real double-check wired in.** `place()`→`applyPlacement()` now calls `C.resultOf(stack)`
  and compares `.color` (per-channel multiply) vs `.check` (independent absorption/log-space
  re-derivation). Was previously comparing classify to itself — a tautology. Sim confirms
  **0 mismatches over 50+ games** (the two code paths agree on the current CMY mixing).
- **NEW draw rule (Mohammad).** A turn is now resolved in two phases: everyone places, THEN
  draws resolve together. For each result color, if the bag can't supply ALL the players who
  want it this turn, **no one** gets it (the chit stays in the bag, picked up later only when
  uncontested). Stacks are visible so a denial is never a surprise. This removes the old
  seat-order bag-raiding bias entirely (resolution is order-independent).
- **Dry clock split into three sub-types** (all still feed the 3-to-end clock): `mud`
  (blended to mud), `empty` (real color, pile exhausted, uncontested), `conflict` (real color,
  more claimants than supply). Logged distinctly. Volume corpus: dry events are ~49% empty,
  ~28% conflict, ~23% mud; ~1.5 conflict-denials per game.
- **Replayable logs.** Seeded RNG + per-game record (seed, palette, every play's
  prev/result/draw/dry); **Download-log** button + autosave POST to `serve_nocache.py`
  (`/save-log` → `chroma-logs/`). Browser localStorage fallback if served by plain http.server.

**GA bench (`chroma-sim.js`, headless, reuses `chroma-core.js`):** weight-vector
"personalities" (genome = the traits Mohammad listed — C·M·Y vs R·G·B focus, color-lock,
mud-rush, blank/cap/edge-center prefs, plus island-bridging + grow-region + draw/hoard/dry
backbone). Eridu-style tournament: Elo + win-share, niche fitness w/ diversity, crossover +
mutation, archetype-seeded population. Ran **~17k games** total (GA 3.2k, archetype arena 6.6k,
painter arena 5.3k, volume 1.5k fully-logged → `chroma-logs/volume-1500.jsonl`).

**Verdict on "is there a mindless optimal?" (`chroma-analyze.js` → `analysis.json`):**
- ✓ The obvious mindless lines LOSE: naive "dump least-present chit" ranks 11/12 (wr .217);
  deliberate mud-rush is dead last (wr .116). Mohammad's earlier worry is answered.
- ⚠ BUT a near-total dominance hierarchy, **0 rock-paper-scissors cycles**. The strongest
  single lever is **mud/dry-avoidance + hand-hoarding** (Survivor wr .597, Cézanne tops the
  painter set too). Winner held the most chits in **86%** of decided games — the multiplicative
  scoring (held chits × largest region) intrinsically rewards a big surviving hand, so "don't
  go dry, hold chits" is a strong, somewhat-mindless backbone. Pure thematic single-trait
  builds (color-only, bridge-only) get crushed when they neglect survival.
- ✓ Depth is in the COMBINATION: the GA's best evolved genomes (balanced/bridge/cmy blends)
  only *tie* tuned survival — they fold survival in WITH a color/region plan. So the skill
  ceiling is real, but the skill FLOOR (survive + hoard) is too rewarding on its own.
- **Tuning levers to break the survival monopoly (for Mohammad to pick):** (a) cap hand size
  so hoarding has a ceiling; (b) make region SIZE (not hand count) the dominant scoring term;
  (c) reward distinct-color spread / bridging explicitly; (d) make `empty`/`conflict` dries
  NOT advance the end clock (only `mud` does) so stalling can't be the whole game.

**Base-12 painter personalities (`chroma-personalities.json`) — for future plays.** Each is a
competent build (survival backbone so none is a pure punching bag) differentiated by one
signature trait, named for a painter whose style matches: Mondrian (primaries), Matisse
(secondaries), Klein (color-lock), Monet (blank-canvas farming), Seurat (cap/seal), Klimt
(edge), Kandinsky (center), Miró (island-bridging), Vermeer (hoard), Goya (mud/tempo —
weakest, .135), Cézanne (survival), Picasso (balanced). 5,280-game round-robin win-rates
.135–.468 (validation block in the JSON). Goya stays lowest because mud-pressure is a
genuinely weak strategy here (consistent across both arenas) — reported, not hidden.

## 21. Size-based scoring + color-wheel hand-swaps (2026-06-08, Mohammad-approved)

Mohammad picked **lever (b)** from §20 and added a new hand-manipulation mechanic. Both
shipped to `chroma-core.js`, `chroma-sim.js`, and the live mock (`chroma-mock.html`).

**Scoring change (lever b — region SIZE, no longer multiplicative).** `scoreHand` is now:
**Σ over every color you still hold ≥1 chit of, of that color's largest-region size.** Holding
more than one chit of a color is NOT multiplicative — one is enough to "claim" the region;
extra copies buy nothing except breadth insurance. (`mult`/`add` field names kept for
back-compat with the Elo/ranking/mock code; `mult` is now the size score, `add` a tier
tiebreak over the same held-color set.) Lever (a) hand-cap was **deliberately not added** —
Mohammad's point: hand size can never exceed the opening 6, so there is no hoarding runaway to
cap. This directly attacks the §20 "survive + hoard" monopoly: you now win by *building* the
biggest body in colors you can keep one ticket to, not by sitting on a fat hand.

**Color-wheel hand-swaps (new mechanic).** After place+draw resolve, each player may
OPTIONALLY and simultaneously make ONE swap, at a net cost of one hand chit:
- discard **2 of the SAME** chit → gain **1 of its wheel OPPOSITE** (180°, `wheelOpposite`);
- discard **2 DIFFERENT** chits → gain **1 of the color BETWEEN** them (`wheelBetween`; only
  defined for the 2-steps-apart case — two primaries → the secondary between, two secondaries
  → the primary between; adjacent / opposite pairs have no clean midpoint → not offered).
The two discards **leave play** (do NOT return to the bag) — a very tiny extra depletion, as
Mohammad intended. The gained chit is drawn from the bag and is subject to the same
simultaneous **conflict-denial** rule as normal draws (short supply this turn → denied to all
claimants). Three uses Mohammad called out, all now supported: shift a poorly-positioned hand,
build into a target color from another angle, deplete chits slightly faster.

**AI: alternate path to a target color.** `decide()` now computes the swap *ingredients* for a
bot's target color — its wheel-opposite (×2 → target) and its two wheel-neighbours (their
"between" = target) — and is gently reluctant to place those ingredient chits away when a swap
is in its plan, so it can recognise it has a second route to the color and hold for it.
`decideSwap`/`bestSwap` only fire a swap when it ADDS a new color's region to coverage without
dropping a held color to zero (surplus duplicates only) — the correct play under size-scoring.

**Genetic traits.** Two new genes — `earlySwap` and `lateSwap` — blended by game phase
(`swapDrive = early·(1−t) + late·t`). New archetypes `EarlyShifter` / `LateSwapper`; the
Vermeer painter is re-cast as the patient late-swap build.

**New benches (baseline-first, ~20.4k games; verifier 0 mismatches over 50 games):**
- **Controlled swap A/B vs the Cézanne baseline** (`swaptest.json`, 3k paired seeds/variant,
  variant in seat 1 vs two plain baselines, neutral = .333): baseline .325 → **lateSwap .387
  (Δ +0.062)**, earlySwap .311 (Δ −0.014), both .382 (Δ +0.057). **Mohammad's intuition is
  confirmed: the swap is powerful specifically LATE; early-swapping is mildly counter-productive,
  and "both" ≈ late alone.** ~0.96 swaps/game when the gene is hot.
- **Painter round-robin under the new scoring** (`chroma-personalities.json`, 5,280 games): the
  survival monopoly is BROKEN. Old order was Cézanne .468 … Goya .135; new order is **Monet
  .399** (blank-canvas/region *building* now leads), Vermeer .382, **Cézanne .373 (fell #1→#3)**,
  Matisse .367, … **Goya .239 (tail nearly doubled)**. The spread compressed from .33 to .16 —
  building a big region now beats merely surviving with a fat hand.
- **Archetype arena** (`arena.json`, 10,920 games): EarlyShifter .458 and Survivor .453 top,
  MudRusher .165 / Monochrome .219 bottom. CAVEAT: EarlyShifter tops the *arena* on its strong
  survival+draw backbone, NOT its swap gene — the controlled paired-seed A/B above is the
  trustworthy isolate of swap value (and it says early-swap ≈ neutral-to-negative). Arena win
  confounds the gene with the whole genome.
- **Volume** (`volume-1500.jsonl`, 1,500 fully-logged): avg 19.1 turns; dry mix 4176 empty /
  2525 conflict / 1659 mud; **swaps rare at 0.29/game** (313 opposite, 123 between, **0
  denied**) across mixed archetypes — matching Mohammad's "most players avoid until late, once
  or twice." The 436 swapped-away chits over 1,500 games = the predicted tiny depletion bump.

**Open item (UX) — RESOLVED 2026-06-08:** the human turn now gets an optional **swap? / skip**
prompt (`chroma-mock.html`). It appears ONLY when the player has a *valid* legal swap available
(2-same → wheel-opposite, or 2-different 2-apart → wheel-between, with the gained color in the
bag); otherwise the turn auto-skips the swap. Each available swap is its own button plus a Skip
button; the choice resolves simultaneously with the bots' swaps under the shared-bag conflict
rule. Bots still auto-apply their best-coverage swap via `planSwap`.

**Lesson for Eridu's GA / future games (Mohammad's request) — PORTED 2026-06-08:** a phase-gated
trait's marginal value is best measured by a **controlled paired-seed A/B that isolates the single
gene** against a fixed baseline, NOT by free-for-all arena win-rate, where a strong backbone
confounds the read (here: early-swap looked top-tier in the arena but is actually neutral/negative
once isolated). This harness is now live in Eridu as `eridu.isolate` (`src/clj/eridu/isolate.clj`):
seeded, deterministic paired games (variant gene in seat 0 vs plain baselines, same seed cancels
board variance), Δ win-share per gene with 95% CIs and significance stars. Run:
`lein run -m eridu.isolate [N-seeds] [players] [champion]`. First read (N=200, neutral 3-player,
default baseline) lands in `output/bench/isolate-results.{edn,json}`. **Also fixed the same day:**
the live Eridu evolution loop (`evolve.clj`) was stripping `:personality` weights on every save and
re-attaching *random* personalities on resume — silently discarding all accumulated learning. It
now persists and restores the full evolved weight vectors (and the champion's), so the GA actually
carries its baseline forward run-to-run. (`bench.clj`'s population path already persisted weights.)

---

## 22. Calibration photo intake + PROVISIONAL schema check (2026-06-09)

**Filed.** New backlit **overlap-grid** photo intaken from the voice pipeline →
`chroma-photos/2026-06-09_overlap-grid_PXL_20260610_032012213.jpg` (orig
`PXL_20260610_032012213.jpg`, rode in with the 2026-06-09 voice drop). Mostly **single
chits** plus a couple of overlaps; centimetre ruler down the left edge for scale.

**⚠️ CAVEAT — do NOT let this become canonical:** the center overlap is an **ORANGE chit over
light-blue**, NOT red. So the intended **red+blue → purple** test is **INVALID in this shot** —
exclude that data point from any mix-table / classifier calibration. (The auto-sampler below
also auto-dropped that blob: its blended mean is a low-saturation muddy brown, correctly read as
"not a clean single disc.")

**Dependency:** a proper physical **color swatch arrives later this week**. The full
overlap-grid calibration is **pending that swatch** — treat it as the official next step, NOT
settled by this photo. (Mohammad, on approving this intake: *"No changes applied yet, the swatch
will be our official next step."*)

**PROVISIONAL preliminary test vs the CURRENT schema (Mohammad-approved 2026-06-09).** He asked
for a preliminary read against the current color schema only. Note: `chroma-analyze.js` does NOT
do this (it scans the game-balance sim logs, not photos), so I wrote `chroma-photo-sample.py` —
segments each disc (HSV mask + connected components, PIL/scipy), takes its mean sRGB, and matches
it to the nearest chip in the current schema (`chroma-core.js` `chip:{}`, both RYB & CMY).
Output: `chroma-photos/2026-06-09_overlap-grid_PROVISIONAL-sample.json`. **9 clean single discs**
(overlap excluded). **UNVERIFIED — applies NO change to the canonical schema/mixing model.**

| # | sampled hex | nearest RYB | nearest CMY |
|---|---|---|---|
| 1 | `#1085e7` | Blue Δ44 | Blue Δ44 |
| 2 | `#3846b6` | Blue Δ49 | Blue Δ49 |
| 3 | `#9d6ebd` | Purple Δ60 | Magenta Δ77 |
| 4 | `#0d8e9f` | Blue Δ67 | Blue Δ67 |
| 5 | `#9b244d` | Red Δ77 | Red Δ77 |
| 6 | `#467491` | Blue Δ67 | Blue Δ67 |
| 7 | `#1abe95` | Green Δ77 | **Cyan Δ66** |
| 8 | `#663a44` | Purple Δ110 | Green Δ123 |
| 9 | `#d03950` | **Red Δ28** | **Red Δ28** |

**Provisional reading (hold loosely — could be white-balance, not pigment):**
- **All Δ are large** (best is the red/pink #9, Δ28; everything else ≥44). Real backlit chits sit
  well off the schema chip hexes — expected pre-swatch, but quantified now.
- **4 of 9 discs nearest-match "Blue"** (#1,2,4,6 — light-blue, indigo, teal, slate). That's a
  crowded blue/cyan corner: either the photo carries a **cool white-balance cast**, or the schema
  under-resolves the cyan↔blue↔teal band. The teal #4/#0d8e9f and emerald #7 (nearest **Cyan**
  under CMY) hint the **CMY palette fits the cool chits a touch better** than RYB — but this is a
  single uncalibrated phone shot, so it's a hypothesis for the swatch to settle, not a finding.
- **Burgundy #8 (`#663a44`)** is effectively unmatched (Δ≥110) — a dim/muddy chit, low-priority.

**Icehouse update (extends §19):** Mohammad has now **bought Icehouse/Looney pieces** to test
Chroma as a physical **stack**; awaiting arrival. Read them per §19 caveats — they **nest, not
layer** (view straight down through the recess), use ONE pip-size for a clean color test, and
**Cyan+Magenta only exist in the Xeno translucent set** (standard sets are ROYGBV → nudges RYB
unless Xeno is on hand). Confirm which colors actually arrive before trusting any mix read.

*No calendar reminder set for the swatch test (date still vague — "later this week"); say the word
and I'll drop one once there's a day.*

---

## 23. Scoring rule op2b MERGED to the live engine (2026-06-14)

The §20 "survive+hoard / broad-hand" worry was explored on 2026-06-09 in a 4-branch
scoring experiment (`chroma-scoring-experiment.md`, `chroma-branch-{base,op1,op2a,op2b}`,
100 games each). **op2b was recommended but never merged into `chroma-core.js` and never
recorded here** — so every live game and the bench kept running `base` (each player
holding ≥1 chit of a color scores that color's full largest region; a contested color is
counted once *per holder*). Caught 2026-06-14 when a 3p game scored Red for all three
players though only B2 held the most (3 vs 2 vs 1).

**Now merged.** `chroma-core.js` gains `largestTwo` + a game-wide `scoreGame(players, stacks)`
implementing **op2b**: a color's region scores ONLY for the player holding the MOST chits
of it; a sole leader takes the full largest region; on a tie for most, each tied player
takes the **second-largest** region of that color; a color no one holds scores for no one.
This is necessarily a cross-player pass (the old per-hand `scoreHand` could not express it
and is kept only for reference/tests). Both the live mock and the bench call `C.scoreGame`,
so they stay unified ([[feedback_one_engine_path_bots_humans]]). Engine/play is unchanged
(smoke moves/turns/dry byte-identical); only end-game scoring changed. Regression test:
`chroma-engine-test.js` §H (the exact You/B1/B2 scenario) + §I (op2b ≤ base always, and
genuinely differs). 326/326 pass.

**Two open follow-ups (unchanged from the 2026-06-09 recommendation):**
- **Bots are NOT yet re-evolved under op2b.** Scoring is read only at game-end and never
  feeds `decide`, so the GA/arena/painter rankings (and `swaptest.json`, regenerated under
  op2b on 2026-06-14) now reflect op2b *scoring of base-optimized play*. Re-evolving a
  population under op2b is the proper next step to confirm the fewer-colors incentive holds
  when strategies adapt.
- **Mud scoring** is still unaddressed (op2b doesn't score mud).

---

## 24. Mud-bonus + no-same-color rules, and the 3-stack/blocking analysis (2026-06-14)

Two rule changes (both in the shared `chroma-core.js`, so mock + bench stay unified;
both also covered by `chroma-engine-test.js` §K/§J, 333/333 green):

1. **Mud bonus (PROVISIONAL).** `scoreGame` now adds, per player, `(START_HAND −
   handLen) × (mud cells on board)`. Rewards spending your hand DOWN when the board is
   muddy, so mud and swap — which both shrink your hand and previously only eroded your
   score — finally pay. Mud = playable cells that blended to mud; the fixed black center
   is excluded. `scoreGame` rows now expose `regionScore`, `mudBonus`, `boardMud`. Formula
   expected to change after testing.
2. **No-same-color-as-last-turn.** `enumerateMoves` excludes the color you placed on your
   previous turn (`p.lastPlaced`, set in `applyPlacement`). EXCEPTION: if that would leave
   no legal chit (your hand is entirely that color), the restriction lifts — the rule alone
   can never brick a player into a forced pass. Applies to bots automatically; the mock greys
   the forbidden hand chip and gates cell-highlight/humanMove. Forces hand diversification,
   reduces draw-type variance, aims to encourage clever color stacking.

**3-chit stack / blocking analysis** (`chroma-stack-analysis.js`, 1000 all-bot 3p games
under the new rules):
- Games end at **avg 27.6 turns / ~83 placements**, **always via the dry clock** (never
  bag-empty), with the board only **~48% full**.
- End-state cell heights: 0.4% empty · 60% h=1 · 33% h=2 · **only 6.2% full (h=3)**
  (~5.6 full stacks/game; 95.6% of games have ≥1 full stack).
- **Blocking ≈ 0**: across 82,776 player-turns, ZERO cases of a player holding chits but
  having no placeable cell in their rotated wedge. With ~15 cells/wedge and <1 full
  cell/wedge, a wedge never saturates before the dry clock ends the game.

**Implication for the board-size question:** the board (90 playable cells, 270 capacity)
is far too large for the dry-clock game length — space is never scarce, full stacks are
incidental, and blocking never happens, so depth-3 stacking carries no tactical pressure.
Mohammad's instinct to **remove spaces is exactly the right lever.** To make stacking +
blocking bite, capacity should approach the ~83 placements/game — i.e. roughly a THIRD of
the current playable cells (~28–35, ≈ hex edge-4). NEXT (when desired): prototype a smaller
board (parameterize edge size / drop the outer ring + re-derive seeding & wedges) and re-run
this analysis to find the size where blocking is a meaningful but not punishing pressure.

---

## 25. Rule refinements + board-removal balance check (2026-06-14)

Follow-ups to §24 (all in shared core; tests 334/334):
- **Mud bonus now counts the K center** (no exclusion) — there's always a baseline
  +1 mud, so an empty hand always earns ≥ (6−handLen)·1.
- **No-same-color is now a HARD rule (no monochrome fallback).** If your whole hand is
  last turn's color you have NO legal placement and MUST skip placement+draw; `lastPlaced`
  is unchanged by a skip, so you stay barred until you place a different color. You can
  still SWAP after skipping to rebuild and re-enter — `decideSwap` gained a RESCUE branch
  that fires (bypassing the usual thresholds) whenever a player is rule-blocked, swapping
  to introduce a different color. Forced-pass rate: ~8.5% of player-turns without rescue →
  **~3.65% with rescue + the swap phase**. Mock shows a "click Pass, then you may swap" hint.

**Board-removal balance (re: removing 9 cells across 3 corners).** Geometry probe: 6 wedges
× exactly 15 cells; each corner tip is in its OWN wedge (sectors 0–5) and a 3-cell corner
cluster lies entirely within that one wedge — so 3 corners = −3 cells from each of 3 wedges
(15→12), the other three stay 15. Because access ROTATES (every player cycles all 6 wedges
each 6-turn rotation, games run ~4–5 rotations), per-player total access stays balanced
regardless of which corners go. **Removing 3 ALTERNATING corners (every other: sectors
{0,2,4} or {1,3,5}) is strictly best**: it preserves the board's 3-fold (120°) symmetry,
which matches the 3-player start (bases 0,2,4), making access symmetric every single turn —
not just over a rotation. Non-alternating (two adjacent) corners keep long-run fairness but
break turn-by-turn symmetry and the visual/seed symmetry. CAVEAT: 9 of 90 cells is far too
few to create the stacking/blocking pressure of §24 (fill ~49%→~53%, blocking still ≈0); it's
a shape/feel trim, not the space-scarcity lever (that needs ~a third of cells removed).

---

## 26. Trimmed-corner board prototype + friction measurement (2026-06-14)

Added a clean `removed`-cells mechanism to `chroma-core.js` (`setRemoved`/`isRemoved`;
empty set = full board, no behavior change — all 334 tests still pass). Prototyped removing
**9 BLANK ring-5 cells at 3 alternating corners** (sectors 0/2/4 = right · upper-left ·
lower-left, matching Mohammad's markup), generated 120°-symmetrically so it's perfectly
balanced for the 3 players (`chroma-board-proto.js`). Live in the mock behind `TRIM_CORNERS`
(renders the 9 as dark holes). Mohammad's insight confirmed: all 9 are blanks, so the cut
hits the OPEN CANVAS (42→33 blank cells, −21%), not already-stacked cells.

**Friction comparison (2000 all-bot 3p games, current ruleset):**

| metric | full board | trimmed | 
|---|---:|---:|
| playable cells / blanks | 90 / 42 | 81 / 33 |
| end fill | 53.4% | 57.9% |
| avg legal cells per decision | 14.7 | 13.1 |
| tight turns (≤2 legal cells) | 0.0% | 0.0% |
| blocked (0 legal cells) | 0.0% | 0.0% |
| **forced pass (no-same-color, no legal chit)** | **3.7%** | **7.5%** |
| full (depth-3) stacks / game | 8.5 | 10.8 |

**Reading:** the trim is gentle on placement geometry (decision space still ~13 cells, never
tight, zero cell-blocking) and adds only a little stacking (full stacks +27%, fill +4.5pp) —
"a little more friction," as intended, NOT painfully tight. BUT its biggest single effect is
**doubling the forced-pass rate (3.7%→7.5%)**: fewer blanks → less fresh-color farming →
hands homogenize toward last-turn's color → more rule-skips. That friction compounds the
no-same-color rule Mohammad is already wary of. Also note: because the board stays so roomy
(~13–15 legal cells/turn even trimmed), geometric trimming is a WEAK lever for stack/block
pressure — the chit rule dominates the real constraint. Recommendation: keep the 9-blank trim
for shape/feel + a mild bump, but watch the combined forced-pass feel in playtest; if it reads
as too many dead turns, drop to 6 blanks (2 per corner). Do NOT go to a drastically smaller
board on top of the chit rule — that would over-constrain.

---

## 27. Mud scoring v2 — "sixth region" owned by the fewest-hand player (2026-06-14)

Replaced the §24/§25 `(6−handLen)×mud` per-player bonus (which §26-era playtest showed
could win on an empty hand, ~50% of scoring) with: mud is a single scorable quantity owned
by the player with the FEWEST chits in hand. Sole fewest → scores TOTAL mud cells (all mud
anywhere, contiguous or not, incl. the K center). Tie for fewest → each tied player scores
only the LARGEST contiguous mud region. No other player scores mud. (`chroma-core.js`
`mudRegions` + rewritten `scoreGame`; fields `mudScore`/`boardMud`/`mudLargest`; tests §K.)

Effect (100 games × counts 2–6): mud's share of per-player score fell ~50% → 5–10%; the
winner now gets 0.0–0.3 mud points (2%→0% of score as count rises); "won on mud" 0–2%.
Mud now BREAKS TIES between close region players instead of dominating, exactly as intended,
and is small at every player count.

OPEN (design brainstorm, not built): a NON-points incentive for occasional mud *placement*
— a "side benefit" that facilitates a lateral pivot (~1 player per 4p game makes 1–2 mud).
Candidate directions captured: (A) making mud clears your no-same-color lock and/or grants a
free swap — a release valve that interlocks with the diversification rule; (B) mud-as-region-
cutter (already mechanical: mud breaks contiguity, so making mud splits a leader's region —
a denial pivot); (D) making mud grants a "draw any color" token to reshape your hand toward a
new target. Awaiting Mohammad's pick before implementing.

---

## 28. Mud-upgraded swap — the lateral pivot (option A, 2026-06-14)

Chosen incentive for mud *placement* (over B = region-cutting, already mechanical; and D =
draw-any-color token, rejected for adding clauses to otherwise smooth flow). On a turn you
MAKE MUD, your swap is upgraded to consolidate the cost:
- **opposite:** discard **1** of a color → its wheel-opposite (was: discard 2-same).
- **adjacent:** discard **1** of a color → **either** wheel-neighbor of it, your choice (was:
  discard 2-different-2-apart → the color between).

Both are net-0 on hand size (discard 1, gain 1), so a mud+swap turn costs only the −1 of the
mud placement itself. The tension Mohammad designed: do it ~3 times and you end at ~3 chits,
so they'd better be the right 3 — and you need that mud to score (fewest-hand → mud region).
It interlocks with the no-same-color rule: making mud becomes the deliberate release valve to
reshape a stuck/committed hand.

Implementation (shared core; mock + bench): `mudded` flag computed per turn from placements
that blended to mud, threaded into `availableSwaps`/`bestSwap`/`decideSwap` (mudded swaps
discard 1; bots take any coverage-positive upgraded swap, bypassing the usual hand-size/drive
thresholds since it's nearly free). `resolveSwaps` now tracks `players[].discarded` (1 or 2),
and the conservation invariant counts actual discards (was `2×swaps`) — verified 0 violations
over 1000 games incl. mudded swaps. Tests §L. Mock greys/upgrades the swap menu and logs the
upgrade. Mudded swaps fire in ~14% of all-bot games (rare — default bots avoid mud; will rise
with human play or once bots are re-evolved to seek mud). 343/343 tests pass.

ASIDE (noted for the record): when I brainstormed options I listed A, B, D and skipped "C" —
a symbol-interference slip (`C` is bound to the core module and the Cyan chit throughout, so
the token read as "already taken"). Caught by Mohammad.

---

## 29. Explicit score breakdown in the UI (2026-06-14)

Scoring was correct but illegible: the game-over panel showed `mult/add`, where `add` was a
vestigial tier-tiebreak from the multiplicative era — confusing and uninformative. Fixed:
`scoreGame` now returns `breakdown:{colors:[{color,value,tie}], mud:{value,tie}|null}`, and the
mock renders an explicit per-player line at game over: each scored color (swatch + value, with
`tie→2nd` flagged in gold), plus the mud component noted `(fewest hand)` or `(tie→largest)`.
The `/add` number was removed from the display. (Verified against a real 3p game: You 19 =
Cyan 14 + Blue 1 tie→2nd + Mud 4 fewest-hand; B1 18 = Magenta 12 + Green 5 + Blue 1 tie; B2 15
= Yellow 13 + Red 1 + Blue 1 tie — all reconcile.) 343/343 tests pass.

OPEN: no final tiebreaker when two players tie on TOTAL score (mock picks first as winner). The
old `add` nominally served this; needs a real rule (e.g. most distinct colors / fewest chits) or
an explicit shared-win — awaiting Mohammad.

---

## 30. Tiebreaker + placement-style gene + re-evolution (2026-06-14)

1. **Final tiebreaker (Mohammad):** when players tie on total score, the winner is the one
   who scored the MOST different region types (colors with value>0, + mud if scored).
   `scoreGame` exposes `scoredTypes`; the mock winner line and the sim `rankAndShare` use it
   (replacing the vestigial `add`); the mock notes "(tiebreak: most region types — N)" when it fires.
2. **New genetic trait `cycleLock` [0,2]:** high = tight A/B/A/B cycling (replay the color you
   placed 2 turns ago — the one the no-same-color rule permits); ~0 = broaden, using other
   colors to time which color lands on which wedge. Implemented via `lastPlaced2` tracking in
   `applyPlacement` + a decide term; neutral at 0 so committed archetypes are unchanged. Tests §M.
3. **Re-evolution kicked off** (`node chroma-sim.js ga`, pop 48 × 22 gens) under the FULL current
   ruleset — op2b scoring, mud v2, no-same-color, mud-upgraded swap, scoredTypes tiebreak, and
   the cycleLock gene — since the bench drives the shared core. Champion to be wired into the
   mock's bot seats (replacing the hand-tuned defaultGenome). NOTE: this GA is self-play
   (intra-population tournament); per [[feedback_self_play_overfit]] a frozen reference panel
   would harden it against monoculture — deferred unless the champion looks degenerate.

347/347 engine tests pass.

## 31. Re-evolution results (2026-06-14)

3,168 games, 22 gens. Champion **g16_0** (elo 1666) is a coherent, NON-degenerate build:
mudRush 1.6 + low mudAversion 0.75 + low dryAversion 0.2 + blankPriority 1.8 + cmyFocus 1.7 +
targetDraw 1.5 + spendTargetPen 2.0. **The new mud rules worked**: a mud-EMBRACING strategy
evolved to the top (vs §20 where mud-rush was dead last under the old scoring) — the fewest-hand
mud-score + mud-upgraded swap made deliberate mud viable. Runners-up: g15_1 (region-grower /
heavy hoarder + dryAversion), g10_0 (blank-canvas farmer).

**cycleLock verdict: broadening beats cycling.** Across the top 16, cycleLock mean 0.15 (mostly
0, max ~1). The GA preferred ~0 → bots use varied colors to time which lands on which wedge
(A/B/A/C), NOT tight A/B/A/B cycling. Directly answers Mohammad's question.

Champion vs 2 old default bots: **0.385 seat win-share** (neutral 0.333) — a genuine but modest
edge ("slightly smarter," as requested). Wired 3 varied evolved builds (g16_0 / g15_1 / g10_0)
into the mock's bot seats, cycling, so the human faces different strategies.

CAVEAT (self-play): Elo gains were modest and late-gen champion win-rate hovered ~0.5 — the
population converged (likely toward mud-awareness), so the mudRush edge is partly intra-pop.
Per [[feedback_self_play_overfit]], a frozen reference panel would give an honest absolute
fitness; deferred (champion is non-degenerate and good enough for the playtest). 347/347 tests pass.

## 32. No-consecutive-pass rule (2026-06-14) — closes the G3 "pass-to-win" exploit

**THE RULE.** A player may **not pass on two of their own turns in a row.** The only
legal back-to-back pass is a **forced** one — you hold **0 chits** in hand (or, by the
no-same-color-as-last-turn rule §[forced-skip], you have no legal placement at all).
With **1+ chits in hand AND a legal placement available, a second straight pass is
illegal — you MUST place.** This shuts the exploit where a player who is ahead on
majority-color scoring just keeps passing to freeze the board and protect their lead.

**Enforced in the ENGINE, not just on paper.** `chroma-core.js` gained `canPass(G, pi)`:
returns `false` only when the player passed last turn AND holds ≥1 chit AND has at least
one legal placement (`enumerateMoves` non-empty); a forced empty-hand or color-locked
skip still returns `true`. Each turn, `step()` (bench) and the mock's `finalizeSwaps()`
record `player.passedLast = (placed nothing this turn)`. The mock's **Pass button is
greyed/rejected** whenever `!canPass` — and `pass()` hard-rejects the action with a
"can't pass twice in a row while holding chits — you must place" message, with the phase
banner spelling out the obligation. Legacy saved games (no `passedLast` field) read as
`false` = "may pass", so loading an old log is unaffected.

Bots are players too, but `decide()` only ever returns a pass when there is **no legal
move** (it always plays when it can), so the rule never has to override a bot — the
`passedLast` tracking is pure bookkeeping for them; the human Pass button is what the
rule actually gates.

**The 0-chit case stays a deliberate DEAD END (G3, above).** A player at empty hand may
keep passing and is simply skipped; the dry-clock / empty-bag end conditions close the
game on their own. No random-chit handout, no elimination, no end-trigger. It can't
happen by accident, it isn't strategically strong (forcing an empty hand burns ~6 turns
of swapping/mud-making that pushes toward closure anyway), and leaving it un-rewarded is
the point — we don't want to incentivize people forcing a low hand to pass every turn.
Recorded here so future-us remembers it's intentional, not an oversight.

Verified: 9/9 targeted `canPass` unit cases (fresh game, passed-last+chits+move ⇒
blocked, empty-hand forced pass allowed, color-lockout forced skip allowed, placement
resets the streak, `step()` maintains the flag) + the full **347/347 engine suite still
passes** (incl. the headless mock-drive in §G).
