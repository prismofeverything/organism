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

## 3. The +10 role-5 bonus — re-examined on fixed-run data (NOT dominant)

Source-confirmed (`game.cljc:1223-1227`): the role end-game bonus is a **flat
+10 points to the opposite track** for reaching level 5, identical for all four
roles. The earlier "this is the dominant lever / a double role-5 rush dominates"
framing came from the **stale crashed-run corpus and does not survive the fixed
run.** Measured over 400,000 player-rows of the 170k-game fixed run:

- **Roles maxed to 5 per player:** zero 73.4%, one 25.6%, two 1.0%, three
  0.001% (2 rows), **four 0 of 400,000.** Maxing all four is impossible: each
  role 1→5 needs four "alone-on-a-space" actions plus 2 pottery + 2 gold (L5
  costs *both* via `can-pay-cost?`), in a 12-turn game — you cannot afford the
  actions/resources for four roles and still score.
- **Reaching L5 is win-correlated but not the main path:** among winners the
  roles-at-L5 split is 0→66%, 1→32%, 2→1.8% — i.e. one role-5 is enriched in
  winners (32% vs 26% of the field) so it *helps*, but **66% of winners maxed
  zero roles.** It is a strong-when-achieved reward that is appropriately hard to
  reach, not a strategy that crowds out the board.
- A turtle/avoid-everyone "grab resources and max roles" line is not viable —
  see the action/resource budget above, and you still must sell/build/raid for
  in-game reputation.

So the recommendation shifts toward **leave +10 as-is** — it rewards a hard,
committed line and the bots find many non-role-5 ways to win. The exploit-finder
also found no single-mechanic build beats fair share. If you ever do touch it,
don't nerf reflexively or tune-to-the-bots.

**The real asymmetry is BETWEEN the roles, not in +10's size.** L5 reach rates:
leader 17.2%, merchant 7.7%, raider 2.2%, **priest 0.6%**. See §3a.

### 3a. Why priest is the weakest role (and leader the most-maxed)

- **Leader is rubber-banded by design** (Mohammad): it earns no points on its
  own — it *multiplies* the other three (sell/temple bonuses, magistrate-flips
  raiders to point). So leveling it is cheap utility, and its L5 +10 amity is the
  main *direct* payoff for an otherwise point-less role — which is exactly why
  it tops the max-rate without being "OP."
- **Priest is a multi-round spatial plan the greedy bot doesn't execute**
  (Mohammad): temples are placed face-up and only score amity once you *travel*
  to flip them, so the human line is "place fast + spread, small travels, then
  prioritize double-move travels to flip." A myopic per-turn scorer places
  temples but under-invests in the later travel-to-flip sequence. Evidence in
  §3b — this looks like an **AI-modeling gap, not necessarily a game imbalance**
  (priest may be fine, even strong, for a planning human).

### 3b. Priest is a *compounding* engine the bot can't see

The temple payoff compounds: `visit-temples-on-travel` flips one face-up temple
and scores **amity = your total face-down count** — so the 1st flip is worth 1,
the 5th is worth 5. The strong line is build a large spread of temples over
rounds 1–2, then travel-to-flip them late (1+2+3+4+5 = 15 amity from five flips).
Placement ceiling scales with priest level (`priest-max-temples {3 5, 4 8, 5 8}`),
so the engine needs sustained multi-round investment: level priest → place many →
flip late.

Fixed-run data (400k player-rows) shows the bot doesn't run that engine:

| segment | avg placed | flipped | flip-rate | avg reputation |
|---------|-----|-----|-----|-----|
| all players | 2.42 | 2.02 | 83% | 10.72 |
| priest-is-top-role | 2.86 | 2.39 | 84% | **8.89** |

- The gap is **not** "place but never flip" — flip-rate is a healthy 84%.
- It's **volume**: priest-top bots reach priest level 3.38 on average (ceiling
  5–8 temples) but place only **2.86**, so the compounding never gets going
  (two flips = 1+2 = 3 amity, trivially beaten by two sells).
- Root cause in `decision.cljc`: temple placement and travel-to-flip use **flat**
  weights (`resolve-travel` scores a flip as `travel-for-temple * 5` regardless
  of face-down count). The scorer can't see that the marginal flip is worth the
  face-down count, so it has no reason to build the base — and the GA can't
  evolve into it because no gene represents the compounding value.

**Fix applied + result (honest).** Added a neutral `:temple-engine` gene that
prices the compounding (placement scales with base size; a flip is worth
face-down+1) plus a `Ref-TempleEngine` panel adversary, and re-ran (170k games).
The fix is correct as *faithfulness* — the bot now models temple scoring — but it
did **not** make priest competitive:

| segment | placed | flipped | reputation |
|---|---|---|---|
| priest-top, before fix | 2.86 | 2.39 | 8.89 |
| priest-top, after fix | 2.98 | 2.55 | **8.79** |
| field (after) | 2.39 | 2.05 | 10.76 |

`temple-engine` was only weakly selected (mean 0.16, max 0.78 — a surviving
niche, not a winning line). So the priest gap is **not** purely a bot-modeling
bug after all: even priced correctly, the temple engine is **action-inefficient
and back-loaded** — ~12 actions (place + level priest + travel-to-flip) for ~15
amity, all deferred to late game, versus amity that selling/role-bonuses produce
immediately per action. The GA won't commit to a deferred engine that loses the
per-action race.

**Conclusion (revised):** keep the bot fix (it's faithful and adds a viable
temple niche), but priest's weakness is ultimately an **economy/design
characteristic**, not a bot bug — a third item for your balance judgment
alongside §3 (the +10 lever). Possible levers if you want priest stronger:
cheaper/faster temple placement, a larger flip payoff, or a lower priest level
ceiling so the base is reachable sooner. **Not tuned here** — making the bot
force priest to win would be balancing-to-the-bots.

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

## 8. Deferred bonus slots — need a data-model change or new choice (your call)

These 8 are NOT band-aided. Each needs a structural change I won't rush overnight; precise plans below for your sign-off:

- **2** — The 'Only you may fulfill those demands' clause requires a per-demand-token ownership/restriction marker. city-demands is currently a flat vector of resource keywords per city (e.g. {:babylon [:gold :gems]}) with NO owner metadata; bonus-sell-in / sell-good-in-city select by (first (filter sellable? demands)) with no awareness of who placed a token or who may fulfill it. Enforcing the restriction is a data-model change, not hackable in this arm.
  - *Plan:* 1) Change city-demands token representation from bare keyword to a record allowing optional ownership, e.g. {:good :gold :restricted-to :alice} (or a parallel map :demand-owners {city [{:idx :player}]}); keep bare-keyword tokens as 'open to all'. 2) Update producers/consumers: place-demand-tokens (accept optional owner), sell-good-in-city + bonus-sell-in + sell-for-glory-in (filter out tokens whose :restricted-to is not the selling player), draw-demand-token/fill-demand-spaces (unchanged, place 
- **2** — Needs a new data-model field. Card: 'Put a random demand token on each of your facedown temples. Only you may fulfill those demands.' The current model has no place to attach a demand token to a temple, nor any owner-restriction on demands: temples are `:players PK :temples {city -> :face-up|:face-down}` (a status, no token slot), and demands live in top-level `:city-demands {city -> [good ...]}` with no owner key. The current arm [22 2] is a scalar proxy: `(update-in [:players PK :amity] + (count-face-down-temples pdata))` — it neither places tokens nor restricts fulfillment. Implementing faithfully requires (a) a per-temple demand-token store and (b) an owner-gate on fulfillment, both genuine schema changes. Not safely hackable inside the dispatch arm.
  - *Plan:* 1) Add field `:temple-demands` to player-state: `{city -> [good ...]}` (a token list keyed by the city whose facedown temple owns it; the player can hold at most one facedown temple per city under the current model, so city is a sufficient key). 2) [22 2] arm: for each city in `(facedown-temple-cities pdata)` draw one token via `draw-demand-token`/`full-demand-bag` and conj it into `[:players PK :temple-demands city]` (reuse the draw helper; do NOT touch top-level :city-demands so other players 
- **2** — Needs the multiple-raiders-per-route model change. Card: 'Place a raider on each route you have a raider.' `:raiders` is `route-key -> single-status` (game.cljc:1633 `(assoc-in [... :raiders route-key] :raiding)`), and place-raider-on explicitly refuses an occupied route (`(not (contains? (:raiders pdata) route-key))`, line 1631). So 'a raider on each route you ALREADY have a raider' is impossible to represent — you cannot stack a second raider on an occupied route. Board 25's `:allow-double-raiders` flag exists but is honored only in choice.cljc deploy resolution (line 641), not by the engine raider store, which is still a one-status-per-route map. The current arm [34 2] is a disclosed approximation: `(reduce place-raider-on state (take 2 (free-routes ...)))` — places up to 2 raiders on FREE routes, not on the player's existing-raider routes. Faithful behavior requires per-route raider multiplicity, a genuine data-model change. Not safely hackable.
  - *Plan:* 1) Change `:raiders` value type from `route-key -> status` to `route-key -> [status ...]` (a vector/multiset of raider statuses per route), OR add a parallel `:raider-counts {route-key -> n}` alongside the existing status map. Prefer the vector form so per-raider flip state (board 17/18 'flip one of those raiders') is representable. 2) Update all readers/writers: place-raider-on (conj a status, drop the occupied-route guard when stacking is intended), count-raiders-deployed (sum vector lengths),
- **3** — Needs a NEW optional interactive choice descriptor (:pick-raider). Card: 'Score 8 Amity if you have Uruk surrounded by Raiders. Then you may flip one of those raiders.' The scoring half ([17 3], game.cljc:2206) is already faithful (8 amity gated on Uruk-surrounded). The deferred half is the optional 'you may flip one of those raiders' — there is no choice descriptor for selecting one of the Uruk-adjacent raiders, and 'may' makes it optional (the player can decline). effect_spec [17 3] marks the second clause `{:kind :flip-raider :optional? true :state :stub}`. Adding it without the descriptor would force a flip (wrong: it's optional) or pick arbitrarily (wrong: player chooses which). Both a new choice descriptor and an optional/decline path are needed — explicitly out of scope to hack.
  - *Plan:* 1) Add a new choice descriptor in bonus.cljc bonus-needs-choice?: `[17 3] {:type :pick-raider :optional? true :scope :uruk-adjacent :prompt "You may flip one of your Uruk-surrounding raiders"}` (and parallel for [18 3] with :scope :kish-adjacent). Requires the choice/UI layer to support an OPTIONAL pick (a 'decline' / nil option) — confirm choice.cljc and play.cljs render an optional descriptor (the existing optional clauses like 18/1, 32/4 'you may sell' set a precedent but those are auto-appli
- **3** — Same class as [17 3]: needs a NEW optional :pick-raider choice descriptor. Card: 'Score 6 Amity if you have Kish surrounded by Raiders. Then you may flip one of those raiders.' Scoring half ([18 3], game.cljc:2235) is already faithful (6 amity gated on Kish-surrounded). The deferred half is the optional flip of one of the Kish-surrounding raiders — no descriptor exists for picking one, and 'may' makes it declinable. effect_spec [18 3] marks it `{:kind :flip-raider :optional? true :state :stub}`.
  - *Plan:* Identical mechanism to [17 3] with :scope :kish-adjacent. Add descriptor `[18 3] {:type :pick-raider :optional? true :scope :kish-adjacent :prompt "You may flip one of your Kish-surrounding raiders"}`; same optional-pick/decline UI support, same bot-bonus-picks handling, same flip helper, same dependency on the :raiders single-vs-vector model decision. Implement together with [17 3] as one :pick-raider feature. Forced-sim test: Kish surrounded → +6 amity; chosen raider flips on pick, no flip on 
- **2** — Needs a queued second free-travel action (board-6-style pending-free-travel), but for TWO travels. Card: 'Take a Gem. Take two travel actions.' Current arm [32 2] (game.cljc:2526) grants the gem AND does ONE real travel via bonus-travel-to to (or choice caravan); the SECOND travel is dropped (effect_spec [32 2] `{:kind :travel :count 2 :state :partial :note 'one travel via human path'}`). A faithful second travel needs the player to choose a second destination AFTER the first travel resolves (the reachable set changes), which the single-shot dispatch arm cannot express — it has only one `choice` value. This needs a queued/pending second-action mechanism, not a model field per se but a pending-action mechanism the dispatch doesn't have for travel-after-travel.
  - *Plan:* 1) Reuse the board-6/31 `:pending-free-travel` flag pattern but generalize to a COUNT (or a pending-action queue): set `[:players PK :pending-free-travel]` to 1 after performing the first travel in [32 2], so the player gets a second interactive travel resolved through the normal travel choice path (choice.cljc consumes :pending-free-travel; end-of-turn dissoc at game.cljc:3288 already clears unused grants). 2) Keep the first travel in-arm (choice = first dest, default caravan) plus the gem; enq
- **1** — Needs the queued free-travel mechanism (board-6 pending-free-travel) after a non-travel primary action. Card: 'Influence a Magistrate then take a Travel action.' Current arm [30 1] (game.cljc:2476) does the influence (bonus-influence to (or choice first-mag-city)) but the 'then take a Travel action' is dropped (effect_spec [30 1] `{:kind :travel :state :partial :note 'auto proxies +glory=leader; human travels'}` — and even the human path only does one ad-hoc travel without a proper post-influence destination prompt). A faithful travel-after-influence needs a second interactive destination chosen AFTER influence resolves, which the single-`choice` dispatch arm cannot express.
  - *Plan:* 1) Resolve the influence as today (choice = magistrate destination), THEN set `[:players PK :pending-free-travel]` true so the player gets one real interactive travel via the existing board-6 free-travel path (choice.cljc consumer; end-of-turn cleanup already in place at game.cljc:3288). 2) This requires the dispatch arm's `choice` to carry the influence target only; the travel destination is supplied later by the free-travel prompt — confirm the WS/choice layer threads a follow-up travel after 
- **3** — Needs the queued free-travel mechanism (board-6 pending-free-travel) after the role increase. Card: 'Increase your lowest role then take a Travel action (you pick if there is a tie).' Current arm [15 3] (game.cljc:2159) faithfully increases the lowest role (with cost, choice resolves a tie among lowest roles), but the 'then take a Travel action' half is dropped (effect_spec [15 3] `{:kind :travel :state :stub :note "'then take a Travel action' dropped"}`). The travel needs a destination chosen AFTER the role increase, separate from the tie-break choice already consumed by the role half — the single-`choice` dispatch arm cannot carry both a tie-break pick and a travel destination.
  - *Plan:* 1) Keep the lowest-role increase exactly as-is (choice = tie-break role among the lowest set). 2) After the increase, set `[:players PK :pending-free-travel]` true to grant one real interactive travel via the board-6 free-travel path (consumed in choice.cljc, cleared end-of-turn at game.cljc:3288). 3) Because the slot already consumes `choice` for the role tie-break, the travel destination MUST come from the follow-up free-travel prompt rather than the dispatch arm — same requirement as [30 1]/[
