# Rule tightening — September 13, 2026

Two-player training kept drifting toward passive play, and the measurements
below suggest the rules as written permit it rather than the trainer inventing
it. Three rule variations are implemented behind flags. **Every flag defaults to
the game as written**, so production, the benchmark panel and the diversity
experiment are untouched until an arm deliberately turns one on.

## What the current rules allow

Measured over 100 recent two-player self-play games and 50,326 replay positions,
all after the replay fix, so none of this is an artifact of that bug:

- **Passing is a first-class move, not a last resort.** `legal()` pushes a
  deliberate pass (`Circulate` with `pass`) unconditionally. It is **50.8% of
  every action selection**. Of the positions where it was legal, **97.1% offered
  real alternatives** — only 2.9% were genuinely stuck. On those 97.1%, search
  put a median 50% of its visits on passing, and preferred it outright in 49.8%.
- **Unusable action types are offered too**, which is the other route to a null
  turn: choosing GROW ends in a pass **79.9%** of the time, MOVE **52.3%**.
  Choosing a type the organism has no elements of sets `num_actions` to zero and
  ends the organism's turn having done nothing at all.
- **Food piles up on eaters and never reaches what spends it.** EAT elements
  hold a median of 1 but a p75 of 39 and a p90 of 90, against the engine ceiling
  of 111; GROW and MOVE elements sit at a p90 of 2 and 3. 37% of EAT-element
  observations are at 5 food or more.
- **87% of round transitions leave the board arrangement unchanged.**

## The flags

| Flag | Zero value | What it changes |
| --- | --- | --- |
| `--require-useful-action` | `0`, current rules | Action types the organism cannot act with are no longer offered, and the deliberate pass survives only where nothing else is legal at all. A legal move always exists. |
| `--eat-threshold N` | `0`, the `FOOD_LIMIT` ceiling | An EAT element at `N` food or more cannot eat. It may still *hold* more, circulated in; the rule is a threshold on eating, not a cap on holding. |
| `--stall-limit N` | `0`, off | Ends a game as `no_progress` after `N` rounds in which neither the arrangement nor the food each player holds changed. |

All three take `-2p` / `-3p` per-player overrides, and all three travel in
`config.json`, in every checkpoint, and into comparison manifests, so a recorded
protocol replays exactly as it was recorded.

## Choosing the stall key and threshold

The cutoff shipped earlier keyed on board arrangement alone and was left off
because it could not tell stalling from playing: among games that end in a
**win**, arrangement alone holds still for a median of 4 rounds and a p90 of 27.

Adding *food held per player* to the key fixes that. Circulating food inside an
organism preserves both halves; eating, growing and moving do not:

| Longest stall per game, rounds | won | drawn / timed out |
| --- | --- | --- |
| arrangement only | median 4, p90 27, max 121 | median 8, p90 145, max 298 |
| arrangement + food held | median 2, p90 **5**, max **15** | median 4, p90 18, max 30 |

| Cutoff at N | won games cut | drawn games cut |
| --- | --- | --- |
| 5 | 12.2% | 49.2% |
| 8 | 4.9% | 28.8% |
| 12 | 2.4% | 16.9% |
| 20 | 0.0% | 8.5% |

No won game in the sample ever stalled 20 rounds on this key, so 20 is free but
catches only the worst. 8 gives roughly a six-to-one ratio of drawn games caught
to won games lost. These are 100 games from one model; the threshold deserves
rechecking against a model trained under the other two rules, where idling is
already much harder and the stall rule is a backstop rather than the lever.

## First measurement: frozen weights, five arms, one seed

Self-play with the current two-player weights on both sides, 16 games per arm,
32 simulations, 1200-choice limit, seed 4242. The model was trained under the
original rules, so under the others it plays off-policy: the question is whether
the passive line is still *available*, not whether the model prefers something
better.

| arm | decisive | 95% interval | vs baseline | mean length | timed out |
| --- | --- | --- | --- | --- | --- |
| baseline | 6/16 | [0.18, 0.61] | — | 400 | 3 |
| `--require-useful-action` | 8/16 | [0.28, 0.72] | p = 0.72 | 573 | 5 |
| `--eat-threshold 5` | 6/16 | [0.18, 0.61] | p = 1.00 | 346 | 3 |
| `--stall-limit 8` | 6/16 | [0.18, 0.61] | p = 1.00 | 391 | 3 |
| **all three** | **12/16** | [0.51, 0.90] | **p = 0.073** | **296** | **1** |

Because every arm shares the seed, the games are matched and can be paired:

| arm | draws that became wins | wins that became draws | games identical to baseline |
| --- | --- | --- | --- |
| `--require-useful-action` | 6 | 4 | 0/16 |
| `--eat-threshold 5` | 0 | 0 | **15/16** |
| `--stall-limit 8` | 0 | 0 | 11/16 |
| all three | **7** | **1** | 0/16 |

**The rules do nothing apart and a great deal together, and removing the pass is
what makes the other two bite.** An eating threshold on its own produced fifteen
games out of sixteen that were identical to the baseline, move for move: the
threshold takes eating away, the organism passes instead, and nothing has
changed. The stall limit on its own only relabels — five games ended as
`no_progress` rather than `repetition`, and not one of them became decisive.
Removing the deliberate pass on its own forces action but has nowhere to push
that action: six draws became wins and four wins became draws, which is noise.
Only when passing is unavailable does an eating threshold force food out to
where it is spent, and only then does the stall limit have something left to
catch.

Sixteen games per arm is a screen, not a finding: p = 0.073 is suggestive and
the paired sign test agrees at 0.07, but neither clears a conventional bar. Both
say the same thing, which is worth something, and the mechanism above is visible
directly in the paired games rather than inferred from the totals.

## A training ablation needs a different evaluation design

Arms playing different games cannot share the frozen-opponent panel the way the
replay ablation did: the anchors were trained under the original rules, and any
evaluation game must be played under *some* rule set, so "arm B under rules B"
against "arm A under rules A" compares nothing. What does work is intrinsic
measurement — does the game resolve, how long does it run, how much of the
action budget goes to passing — and head-to-head play with both models under one
fixed rule set, run once under each set.
