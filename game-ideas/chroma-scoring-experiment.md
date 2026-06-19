# Chroma scoring-rule experiment — op1 / op2a / op2b (100 games each)

Run 2026-06-09. Isolated branches in `chroma-branch-{base,op1,op2a,op2b}/`, each a
standalone copy of the engine. Build script: `apply_branches.js`. Fixed archetype
population, identical seeds (volume SEED=4242) → the **only** variable is the
scoring rule.

## The rules
- **base** — current: each color you hold ≥1 chit of scores that color's largest region size.
- **op1** — base PLUS the color's 2nd-largest region when you hold ≥2 chits of it (additive).
- **op2a** — a color scores only for the player holding the MOST chits of it; sole leader gets the FULL largest region, on a tie each tied player gets HALF.
- **op2b** — same "most chits gates scoring", but on a tie each tied player gets the SECOND-largest region instead of half.

## Crucial caveat (read first)
Bots are the fixed archetypes (not re-evolved per rule, per your confirmation), and
the scoring rule is read **only at game-end** — it never feeds back into bot
decisions. So the boards and hands produced are **identical across all four
branches** for the same seeds. The all-seats variety numbers below are therefore a
population constant, identical everywhere. **The signal that moves is the
win-share-weighted "winner" profile** — i.e. what the *winning* hand looks like
under each rule. That is the right question for "does this rule make focusing on
fewer colors worthwhile."

## Population constants (identical across all branches — sanity check)
avg turns 19.9 · all-seats avg distinct-colors 2.68 · all-seats avg hand size 4.06 ·
board mud frequency 0.0106 (≈1 mud cell/board beyond the black center).

## Result — the winning hand under each rule
| rule | winner avg distinct | win-share to FOCUSED (≤2 colors) | win-share to BROAD (≥4 colors) | winner hand size |
|------|--------------------:|---------------------------------:|-------------------------------:|-----------------:|
| base | 3.30 | 0.150 | 0.380 | 4.65 |
| op1  | 3.28 | 0.145 | 0.365 | 4.91 |
| op2a | 2.98 | 0.285 | 0.260 | 4.78 |
| op2b | 2.90 | 0.330 | 0.237 | 4.72 |

Winner distinct-color distribution [d=1,2,3,4,5]:
- base `[0, .15, .47, .31, .07]`
- op1  `[0, .145, .49, .305, .06]`
- op2a `[.048, .237, .455, .21, .05]`
- op2b `[.057, .273, .433, .187, .05]`

## Reading
- **The "6-distinct dominant" premise:** with the current fixed bots, hands never
  reach 6 distinct (they top out at 5, only 5–7% of wins). But the *direction* of
  the worry holds under base: broad (≥4-color) hands take 38% of win-share vs 15%
  for focused (≤2-color) hands.
- **op1 does NOT meet the goal.** Winner breadth is unchanged (3.30 → 3.28). It just
  rewards holding duplicates — winner hand size and per-color presence rise — without
  making narrow focus win more. Breadth-neutral.
- **op2a meets the goal (softly).** Winner breadth 3.30 → 2.98; focused win-share
  nearly doubles (.15 → .285), broad falls (.38 → .26).
- **op2b meets the goal (strongest).** Winner breadth 3.30 → 2.90; focused (.33) now
  basically equals broad (.237 — i.e. broad is no longer the dominant winning shape).
  Single-color (d=1) wins appear (0% → 5.7%).

## Archetype reshuffle (base → op2b win-rate)
Focused builds rise: Primarist +.135, Monochrome +.149, Hoarder +.125, Corehugger
+.096. Broad/utility builds fall: Secondist −.194, EarlyShifter −.267. MudRusher's
*winning* hands are genuinely narrow (winner-distinct 1.75) and its win-rate ticks up
(.188 → .222). This is the intended incentive shift.

## The mud half of the goal is NOT addressed
Board mud frequency is a population constant (0.0106) and cannot move under any of
these three rules with fixed bots — none of op1/op2a/op2b score mud, and the bots
don't re-optimize. To actually test "make placing mud worthwhile" you need either a
rule that scores mud directly, and/or re-evolving a bot population per rule so
strategies adapt. Say the word and I'll run that as a follow-up.

## Recommendation
**op2b** for the fewer-colors goal (op2a is a gentler version of the same lever);
**op1 is out**. Suggested next step: re-evolve bots under op2b to confirm the shift
holds when strategies adapt, and pair it with a separate mud-scoring tweak for the
mud problem.
