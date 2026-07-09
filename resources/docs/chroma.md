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

- **Board:** a backlit hexagon of hexagonal recessed spaces: an **edge-6 hex with 18 cells
  cut away = 73 spaces** — 72 playable + 1 neutral center. The cuts (adopted 2026-07-02,
  120°-symmetric): at each of the 3 white corners, a 6-cell notch — the vertex, its 2 rim
  neighbors, and the 3-cell inner bulge behind them. Each corner's former 7-white line
  becomes **two discontinuous white pairs** hugging the rim; whites drop 42 → **24**.
  *(Confirmed by Mohammad 2026-07-02: he'd pictured 4 singly-spaced whites rather than two
  pairs of 2, but the pairs build is accepted as canon.)*
  Wedges alternate **9 / 15** playable cells (tight wedge, loose wedge), which evens out as
  wedge access rotates. Canonical in `chroma-core.js` `DEFAULT_TRIM`.
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
- **Hand visibility (rule, 2026-07-02):** only the **starting 6 are hidden**. Every chit
  gained afterward — draws and swap gains — is **placed face-up in front of you, visible to
  all**. When you spend a color you hold in both pools, **you choose** whether to spend the
  open copy or a hidden starting chit (spending hidden implicitly reveals you had one). So a
  player decides between relying on the shadow of the starting hand or transforming into a
  fully open position; in practice only 1–2 chits stay unknown late-game.
- **Seeding:** the board starts with **48 playable spaces pre-seeded** (1 chit each, depth 1)
  along the inscribed-triangle lines and color-wheel cells; the other 24 start empty/white.
  Seeded spaces are fully playable (you stack on the seed; the seed counts as 1 of the 3 depth).

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
| Board (canonical 2026-07-02) | hex edge-6 minus 18 (3 corner notches + inner bulges) = 73 spaces (72 playable), wedges alternate 9/15 |
| Depth (stack height) | **3** — full stack locks |
| Palette | CMY + RGB: Cyan, Magenta, Yellow, Red, Green, Blue (no clear chit) |
| Supply | 30 per color, 180 total, no replacement |
| Starting hand | 6, hidden; **all later gains are public** (spend-choice: open vs hidden copy) |
| Seeding | 48 seeded / 24 empty |
| Turn | simultaneous; rotating wedge (+1/turn); place 1 → draw result → optional swap |
| Adjacency / scoring | hex 6-neighbor; op2b (most-holder takes largest region) |
| Endgame | dry clock — 3rd dry turn (any player) → 1 final turn |

## Open for your call (not yet canonical — awaiting Mohammad)

These weren't settled enough to bake in as rules; the design log treats them as live:

- **Board size by player count.** RESOLVED for the default (2026-07-02): the 73-space
  deep-notch board above is canonical for all counts (§24–26's "too roomy" finding + the
  interaction-density finding drove it). A per-count size table is still open if playtests
  want one.
- **Player→wedge mapping for 4 and 5 players** (2/3/6 space evenly on 6 wedges; 4 and 5 don't).
- **Physical chit palette (RYB vs CMY).** Digital default is CMY; the physical material is still
  pending the calibration swatch shoot, and available translucent sets nudge toward RYB.
- **Mud-bonus formula** is marked provisional ("expected to change after testing").

---

# Chroma — design notes (condensed history, non-canonical)

> Chronological decision log that produced the canonical rules above. **Condensed
> 2026-06-20** from the full blow-by-blow journal (recoverable in git history) down to
> decisions + rationale + artifact pointers. Section numbers/titles are preserved so older
> references (e.g. §22, §23) still resolve. Where any entry disagrees with the canonical
> rules at the top, **the canonical rules win.** Owner: Mohammad. Started 2026-06-07.

## 1–3. Pitch, core loop, scoring (2026-06-07)
The seed concept: place a translucent chit on a backlit stack, the stack re-blends, draw a
chit of the new color; chits held at game end score against the largest contiguous same-color
regions. Hand chits = "hold for points vs spend to build." Original scoring was multiplicative
(hand count × largest region); later replaced (see §21, §23).

## 4. Mixing model — SETTLED BY EXPERIMENT (2026-06-07) ✓
Physical backlit rig + photos (`chroma-photos/`, `choma-color-photos.zip`) proved real stacked
chits mix like **paint (broadband subtractive), not ideal filters**: red+blue→purple,
blue+green→teal, over-stacking→muddy brown (not black). **Consequence: calibrate, don't derive**
— the draw table and the camera classifier must be built from photos of the real chits, not a
formula. Depth still degrades gently (mud). Add a white-reference patch for camera white-balance.

## 5–10. Early rule resolutions (2026-06-07)
- **Depth = 3** (Q1), uniform; full stack **locks** (a defense/protection mechanic). Per-space
  variable depth dropped.
- **Palette:** debated RYB artist-wheel vs CMY; see §14 for the CMY decision.
- **Special/blackout/clear chit: CUT** entirely — system is 6 colors only.
- **Forced placement** (exactly 1 chit/turn); space color = the live blended disc; **mud is not
  drawable** (no chit, no score value, acts as a wall).
- Hidden hands; mostly-constructive interaction; **hex 6-neighbor** adjacency.
- Emergent physics to design around: add-only/order-independent (a stack is a multiset), mud is
  permanent, hand chases what you build, board-fill is a natural clock.
- §10 = first disposable v0.1 ruleset draft (superseded). Open question flagged: is the printed
  board geometry purely decorative/legend, or does it carry gameplay function? (Stayed decorative.)

## 11–12. Board geometry + rotating-regions mechanic (2026-06-07)
- Board mockup via `render_board.py`: **hex-of-hexes, edge-6 = 91 spaces**, inscribed triangle
  (primaries on sides, secondaries at vertices), central black hub + color-wheel ring.
- **Big mechanic decision (Mohammad): simultaneous turns + rotating exclusive wedges** (à la
  Photosynthesis) to kill downtime and defuse cutthroat play. **6 wedges × 15 cells**; the single
  black center is the neutral non-playable pivot (the 6 wheel cells stay playable, one per wedge).
- **Seeding:** pre-place 1 chit along triangle lines + wheel cells → **49 seeded / 42 white**.

## 13–17. Digital mock v0.1→v0.5 + palette/endgame decisions (2026-06-07)
Built `chroma-mock.html` + pure-logic `chroma-core.js` + headless `chroma-sim.js`.
- **§14 PALETTE DECISION: CMY+RGB** (Cyan/Magenta/Yellow + Blue/Red/Green). An independent audit
  found RYB a counterintuitive thicket (34 dark-but-colored surprises) while **CMY passes all 7
  subtractive-closure rules** and separates best for the camera. RYB kept as a provisional toggle.
- **§15 ENDGAME = MUD/DRY CLOCK** (replaced fixed rotations): a turn that draws nothing is "dry";
  **3rd dry turn (any player) → one final turn → score.** Mud-avoidance is the core skill.
- **§16:** depth→3, bag→30/color (180), dry clock unified (mud OR empty pile), bag-empty = backup
  end. Strategic insight: farm blank spaces early to set the board.
- **§17:** 3-player support (N_PLAYERS constant), first smart bots (weighted target-color play),
  in-UI mix reference table. Mud-avoidance massively extends games vs random → confirms skill arc.

## 18. Remote phone testing (2026-06-07)
Served over Tailscale for phone playtest via `systemctl --user chroma-mock.service` →
`serve_nocache.py` (no-cache static server on :8770). URL `http://100.115.113.111:8770/
chroma-mock.html` (tailnet). Phone shortcut added; needs Tailscale ON; `loginctl enable-linger m`
for headless survival. See [[reference_phone_shortcut_push]]. *(Paths updated 2026-06-20: files now
under `game-ideas/chroma/`.)*

## 19, 22. Physical prototype + calibration intake (2026-06-08/09)
- **Icehouse / Looney Pyramids** bought to test physical stacking. Caveat: they **nest, not
  layer** (view straight down); use one pip-size for a clean color read; **Cyan+Magenta only in
  the Xeno set** (standard sets are ROYGBV → nudges back to RYB unless Xeno is sourced).
- §22: a backlit overlap-grid photo was intaken but the central overlap was orange-over-blue (not
  red+blue) → **invalid for the red+blue→purple test**; excluded. A provisional 9-disc sample
  (`chroma-photo-sample.py`) showed all chits sit far off the schema chips (CMY fits the cool
  chits slightly better) — **a hypothesis for the swatch, applied no change.** **Mixing stays
  PROVISIONAL**; the official next step is the calibration **swatch shoot** (swatchbook purchased
  2026-06-07; play-material 6-pack TBD). No date set.

## 20. Draw-conflict rule + GA personality bench (2026-06-08)
- **NEW draw rule (Mohammad):** place-then-draw in two phases; if the bag can't supply ALL
  players who want a color this turn, **no one** gets it (order-independent; kills seat-order
  bag-raiding). Dry sub-types: `mud` / `empty` / `conflict`. Replayable seeded logs + `/save-log`
  autosave to `chroma-logs/`.
- **GA bench** (~17k games): the obvious mindless lines LOSE (naive dump 11/12, mud-rush last),
  BUT under the then-multiplicative scoring the dominant lever was **survive + hoard a big hand**
  (winner held most chits in 86% of games) with near-zero strategy cycles. Tuning levers proposed;
  Mohammad picked lever (b) — see §21. Produced 12 named "painter" personalities
  (`chroma-personalities.json`, Mondrian…Picasso) for future plays.

## 21. Size-based scoring + color-wheel hand-swaps (2026-06-08, approved)
- **SCORING CHANGE (lever b): region SIZE, not multiplicative.** Score = Σ over each color you
  still hold ≥1 chit of, of that color's largest-region size. One chit "claims" a region; extra
  copies only buy breadth. Hand-cap (lever a) deliberately not added (hand can't exceed the
  opening 6). Breaks the survive+hoard monopoly → you win by **building** the biggest regions.
- **Color-wheel hand-swaps (new mechanic):** after place+draw, one optional swap, net −1 chit:
  2-same → wheel-**opposite**; 2-different-2-apart → the color **between**. Discards leave play.
  Bench confirmed the swap is powerful **specifically late** (lateSwap Δ+0.062 vs neutral;
  early-swap ≈ neutral/negative). **Lesson ported to Eridu** as the paired-seed gene-isolation
  bench `eridu.isolate` (arena win-rate confounds a gene with the genome). See
  [[feedback_self_play_overfit]].

## 23. Scoring rule op2b MERGED to the live engine (2026-06-14)
**Live scoring = op2b** (was silently still running `base`, caught when a 3p game scored a
contested color for all three holders). op2b: a color's largest region scores ONLY for the player
holding the MOST chits of it; sole leader → full region; tie → each tied player scores the
**2nd-largest** region; unheld color scores for no one. Implemented in `chroma-core.js`
`scoreGame`/`largestTwo`; both mock + bench call it ([[feedback_one_engine_path_bots_humans]]).
The 4-branch comparison that chose op2b (`chroma-scoring-experiment.md`) has been retired now that
op2b is canonical.

## 24–26. Mud bonus, no-same-color rule, board-size analysis, corner trim (2026-06-14)
- **No-same-color-as-last-turn:** can't place the color you placed on your previous turn. Became a
  **HARD rule** (§25): an all-forbidden hand → forced skip (no place/draw); recover via swap
  (`decideSwap` rescue branch). Aims to force hand diversification.
- **Mud bonus** introduced (`(6−handLen)×mud`) then **replaced** in §27.
- **Board-size finding (`chroma-stack-analysis.js`):** the 90-cell board is far too roomy for the
  dry-clock length — games end ~28 turns / ~48% full, **blocking ≈ 0**, depth-3 stacking carries
  no pressure. **Removing spaces is the right lever**; real scarcity needs ~a third of cells
  (~edge-4), NOT a small corner trim.
- **Corner trim (§26):** `setRemoved` mechanism; prototyped removing **9 blank cells at 3
  alternating corners** (120°-symmetric → balanced for 3p), behind `TRIM_CORNERS`. It's a
  shape/feel trim with a mild friction bump — but its biggest effect is **doubling forced-pass
  (3.7%→7.5%)**, which compounds the no-same-color rule. Kept as a flag, not adopted as the
  scarcity lever.

## 27–28. Mud scoring v2 + mud-upgraded swap (2026-06-14)
- **Mud scoring v2:** mud is a single "6th region" owned by the player with the **FEWEST** chits
  in hand (sole-fewest → all mud cells incl. center K; tie → largest contiguous mud region each).
  Replaced the §24 per-player bonus that could win on an empty hand. Mud's score share fell ~50%→
  5–10%; it now **breaks ties** rather than dominating.
- **Mud-upgraded swap (option A — the lateral pivot):** on a turn you MAKE MUD, swaps cost 1
  discard not 2 (opposite: 1→wheel-opposite; adjacent: 1→either neighbor) = net-0. Interlocks with
  no-same-color as the deliberate release valve to reshape a stuck hand. (Options B region-cutting
  = already mechanical; D draw-any-token = rejected.)

## 29–30. Score-breakdown UI + tiebreaker + cycleLock + re-evolution (2026-06-14)
- **UI:** game-over panel now shows an explicit per-player breakdown (each scored color + value,
  `tie→2nd` in gold, mud component noted). The vestigial `add` number removed.
- **Final tiebreaker:** on a total-score tie, winner = most **distinct region types** scored
  (`scoredTypes`, incl. mud).
- **New gene `cycleLock` [0,2]:** high = tight A/B/A/B cycling; ~0 = broaden/time-the-wedges.
- Re-evolution kicked off under the full ruleset (self-play; per [[feedback_self_play_overfit]] a
  frozen reference panel is the honest next step, deferred).

## 31. Re-evolution results (2026-06-14)
3,168 games / 22 gens. Champion **g16_0** is a coherent, non-degenerate **mud-RUSH / primary /
blank-canvas** build — **the new mud rules worked** (mud-embracing play went from worst under old
scoring to best). **cycleLock verdict: broadening (~0) beats cycling** — answers Mohammad's
question. Champion ~0.385 seat win-share vs old default bots (a genuine but modest edge). Three
varied evolved builds (g16_0/g15_1/g10_0) wired into the mock's bot seats, cycling. CAVEAT
(self-play): population converged, so the edge is partly intra-pop — frozen panel still pending.

## 33. Deep corner notches + draw-visibility rule (2026-07-02, Mohammad)
Driven by the feel-dashboard session (margins ~40% of winner, interaction density 13%, dry
cliff 2%/3%/26%, arc real but invisible — see `game-design-patterns.md` §4e). (a) **Board:**
adopted the 9-cell corner trim AND removed the 3-cell inner bulge behind each notch ("the
spaces sticking furthest out of the line of edged white space") → 73 spaces, whites 42→24,
each corner's 7-white line → two discontinuous rim pairs; canonical in core `DEFAULT_TRIM`,
applied by `newGame` (mock's own trim flag deleted — one board everywhere). Emergent: wedges
alternate 9/15 playable cells. (b) **Visibility:** starting hand hidden; every later gain
(draw/swap) is public (`p.pub` pools, `spendChit`/`gainPublic`, invariant pub≤hand); spender
chooses open-vs-hidden copy (`move.fromHidden`; default spends open first). Intent: player
chooses to rely on the shadow of starting chits or go fully open; expect only 1–2 unknown
chits by late game — makes the measured-but-invisible lead changes *readable*. Mock: hand
shows hidden (dashed) vs open chits, opponents' rows show public chits + hidden count.
Engine tests §N (357 total). Bots don't yet *read* opponents' public pools — a future
genome lever (e.g. contest-aware op2b play).

## 32. No-consecutive-pass rule (2026-06-14) — closes the "pass-to-win" exploit
A player may **not pass two of their own turns in a row**; the only legal back-to-back pass is a
**forced** one (0 chits, or no legal placement under no-same-color). Enforced in the engine
(`canPass`), and the mock greys/rejects the Pass button. Shuts the exploit where a leader freezes
the board by passing. The 0-chit case stays a deliberate **dead end** (not eliminated, triggers no
end, left un-rewarded so nobody games a forced-empty hand). 347/347 engine tests pass.
