# Eridu — Systemic Findings & Design Review (2026-06)

Consolidated output of the multi-agent retrospective run after the bonus-board /
contest-topology / decision-unification fixes. Three independent tiers fed this:
a 5-analyst GA retrospective + overseer, an adversarial tier (opponent-modeling,
exploit-finder, self-play-overfit + adjudicator), and direct source verification.

The findings are sorted by **who owns the fix**, because that is the line that
matters: engine bugs and harness/genome improvements were fixed here; game
*balance* is yours to call and is only documented, not changed.

---

## 0. The headline correction (read this first)

**The previous GA run crashed**, and every quantitative number the analysts
produced describes the *old / stale* basin, not the fixed engine. The run loaded
a stale 6-organism April population, wrote 3 generations, then died with a
NullPointerException at `bench.clj:416` (a hand-built config dropped
`:inter-fresh-fraction`, which the banner multiplies raw). So treat all
"merchant L5 in 141/216 players", "24% board noop", etc. as **pre-fix
hypotheses to re-confirm**, not measured facts about the current code.

What is trustworthy is anything verified against source (Sections 1–2) — those
do not depend on the GA data at all.

---

## 1. Engine / harness bugs — FIXED in this branch

| # | Bug | Root cause (source-verified) | Fix (commit) |
|---|-----|------------------------------|--------------|
| 1 | GA crash on custom config | `run-bench!` read config keys with bare `(:key config)`, no defaults | merge over `base-config`; warn on population resize (`425ef5a`) |
| 2 | Mutation clamp leak | `mutate-personality` had a 0.05 floor but **no upper clamp**; weights ran to take-weight 11.4, role-action-coupling 1.63 (above its own 1.0 ceiling) | per-key `weight-bounds` table + clamp; cap coupling at 1.0 where consumed (`e2f601c`) |
| 3 | Self-play monoculture | fitness was pure intra-population Elo (shared blind spots inflate Elo without skill); `evolve-generation` selected on raw `avg-reputation`, leaving the niche-diversity term unused | frozen adversarial **reference panel** as an external gradient + per-region cap + monoculture/runaway guard (`6b8d579`, `f1a1f95`) |

These are unambiguous engineering defects, fixed and regression-tested
(`evolve_test.clj`, `decision_test.clj`).

## 2. G1/G2 magistrate contests — FIXED (correction to an earlier misread)

**Earlier I reported G1/G2 as "unclaimable, turn-stats keys written in 0 places."
That was wrong** — I grepped only `game.cljc`. The instrumentation lives in the
influence path in `choice.cljc` (resolve-influence-choices, ~line 814), where
`:magistrate-max-move` and `:magistrate-raiders-flipped` are accumulated per
magistrate as it moves. `game.cljc` only *reads* them. So:

- **G1 "Move one Magistrate four cities in one turn"** was already correct:
  `:magistrate-max-move` is the max per-magistrate *cumulative* movement this
  turn, and `leader-movement` is `{3 4, 4 5, 5 5}` so a leader-3+ reaches 4 in a
  single influence action. Live; just genuinely hard (needs leader 3+ or stacked
  influence). Mohammad confirmed the cumulative reading.
- **G2 "Move a Magistrate through three raiders (owned by any player)"** had a
  real bug: it counted only `:raiding`-side raiders, so the point-side raiders
  the magistrate also crosses were ignored — undercounting against the intended
  "either side, any owner" rule (Mohammad confirmed). **Fixed**: the count now
  includes raiders of either side and any owner, robust to non-canonical route
  keys (only `:raiding` ones are still flipped, but all crossed ones count).

Regression test `g1-g2-magistrate-contests-claimable-test` (point-only raiders →
0 crossings before the fix, ≥3 after) pins both contests live.

> The other feats the analysts called "dead" (M1, M2, F2, I1, K2) read keys that
> *are* written — plausibly just **hard**, not impossible. Their zero-claim rates
> came from stale data; re-measure on the fixed run before concluding anything.

## 3. The dominant strategy — DESIGN, your call (not changed)

Source-confirmed (`game.cljc:1223-1227`): the role end-game bonus is a **flat
+10 points to the opposite track** for reaching level 5, identical for all four
roles. Combined with `reputation = min(amity, glory)`, +10 to your weak track is
decisive, and merchant/leader are the cheapest, highest-frequency roles to max.
The structural diagnosis (a double role-5 rush dominates) is sound and
engine-independent; the *magnitudes* are stale.

The exploit-finder's reassurance matters here: **no single-mechanic hand-crafted
exploit beat fair share** — the rest of the economy looks well-balanced. So this
is one concentrated lever, not a broken game.

Options (do **not** want me to pick — this is a feel decision):

- **Leave it.** It is a clear, legible win condition; specialists losing to
  generalists is a defensible design.
- **Scale it down** (e.g. +6/+8) so action-diversity strategies stay competitive.
- **Diminishing returns** on a *second* maxed role, so double-role-rush is taxed
  but a single specialty still pays.

Strong recommendation from every tier: **don't nerf reflexively, and never tune
it just to make the current bots win** — that would be balancing-to-the-bots,
itself an overfit. Re-measure with the fixed reference-panel run first.

## 4. Opponent-blindness — FIXED as new genome dimensions (neutral defaults)

The decision module played effectively solitaire: the primary action scorer
(`weighted-action-priority`) had **zero** opponent terms, and `decide` never read
opponent score or opponent contest progress — despite contest-claiming being
automatic and race-ordered (first claimer 3 wild pts vs 2/1/1). Critically, a
self-play GA *cannot* evolve a fix for a blind spot the whole population shares.

Adding the missing **features as genes** is the fix — it creates the very
selection pressure that was absent (a feat-racing organism out-claims a blind
one head-to-head). Three traits added, each **exactly 0 at default** so every
committed archetype and saved genome is byte-identical until the GA explores:

- `:standing-awareness` — catch-up urgency on the binding track when behind the
  field's reputation leader (`8c0a753`).
- `:supply-conservation` — husband the last unit of finite raider/temple stock
  (`8c0a753`).
- `:feat-race-urgency` — rush to claim a contest first when an opponent is close
  to the same one (`9513509`). Landed *after* the reference panel so the frozen
  `Ref-FeatRacer` supplies a gradient; hand-tuning it would have been overfit.

Neutrality + liveness are both pinned by tests.

## 5. Overfitting guards applied (what was deliberately NOT done)

The adjudicator and overseer flagged these as traps; all were respected:

- Rejected the specific point ladders (`4/7/10`, "raise H1 / cut M2") — tuned to
  the stale 216-game corpus.
- Did **not** reweight `decision.cljc` to encourage raider/priest play — that
  fixes a symptom of the economy in the bot heuristics (overfit). The raider
  loop, if it needs help, is an economy change (your call), not a bot tweak.
- Did **not** rebalance individual board slots toward glory to fix the amity
  skew — per-slot patching on tiny-n win-rates.
- Did **not** commit the stale evolved population as a "new baseline" — it would
  launder pre-fix data.

## 6. Fixed-harness run — results (2026-06-14)

Full `config-all` run on the corrected engine: 10 runs × 100 gens, pop 20,
counts [1 2 3 4], **170,000 games**, no `:fresh?` reseed from stale data.

- **No collapse.** Zero diversity/runaway warnings; `unique-regions` held at
  **5–9** the whole run (the old harness sank to 1–2). Final population spans
  **7 regions** — merchant-led but with priest/leader strategies surviving.
- **Real skill gain.** `panelWR` (win-rate vs the frozen panel) climbed from
  0.45 to ~0.55–0.64 — the population genuinely beats the external adversaries
  more over time, which intra-monoculture Elo could never have shown.
- **New genes explored *and* selected** (no dead genes): `supply-conservation`
  converged high (mean **0.79**), `feat-race-urgency` settled moderate-high
  (mean **0.45**, up to 0.88 — the Ref-FeatRacer gradient worked),
  `standing-awareness` modest (**0.24**).
- **All weights in-bounds** (0 out-of-bounds) — the clamp held.

Committed baseline: `resources/eridu/evolved-baseline.edn` (20 organisms, full
provenance header). Mirrors `output/bench/evolved-population.edn`.

## 7. Still open — your calls

- **+10 role-5 lever** (Section 3). The fixed-run data is now in: the population
  is still merchant/leader-led even with diversity preserved, consistent with
  this being the dominant lever. Decide leave / scale / diminishing — or leave it
  as a legible win condition. Not changed.
- Nothing else outstanding; G1/G2 are fixed (Section 2).
