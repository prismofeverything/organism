# Eridu — Phase 0 play + Reflection #1

**What this is:** Per the approved plan, I (Claude) dropped into one seat of Eridu and
actually played — reading the *same* `choice/find-state-raw` option set a human/bot sees,
with opponents driven by the evolved baseline personalities. Read-only w.r.t. rules: every
actor (me included) could pick only from the presented option set. No scorer assist, no
peeking past what a human sees. This is the reflection the play produced — felt first,
then grounded in the actual rules code so it isn't armchair.

Artifacts:
- `/tmp/eridu-playlog-4p.txt` — turn-by-turn mechanical log, 4-seat game (I drove P1-YOU).
- `/tmp/eridu-playlog-solo.txt` — solo game log.
- Harness: `src/clj/eridu/claude_seat.clj` (built for this; persistent FIFO seat driver).

## How I played

**4-seat game** — I deliberately chased the priest/temple line (target feats G1 Move-Magistrate-4,
C1 Four-Face-Up-Temples) against the top-3 elo baseline bots, exactly to *feel* the weakness
instead of inferring it from benches.
- R1: landed temple spaces (3, 7), placed 3 face-up temples almost immediately, pushed Priest 1→2→3→4.
- By R2 I was **Priest-4 (table-high; everyone else Priest-1), 3 temples — and dead last at rep 0–1**
  while P4 (Priest-1) had run to rep 6 and P2 (Merchant-3) to rep 4.
- I then pivoted to the sell economy to feel the contrast: **one `sell` action = +2 Amity +1 Glory,
  immediately.** That single action out-scored my entire 6-turn priest/temple investment.

**Solo game** — clean reps to test the temple-claim gate knowingly. Placed temples in the exact
cities the rules predict (magistrate/caravan cities without my temple), hit the **Priest-1 cap of 3
temples precisely**, confirming the gate.

## The key felt friction (and what it actually is)

Sitting in the seat, the priest/temple line *feels* like a trap: it consumes resources and actions
and produces no reputation, turn after turn, while opponents who treat temples as a side effect of a
sell economy pull away. The C1 "four face-up temples" feat looked cheap (3 temples came nearly free)
but the 4th was simply unreachable — I'd select the `temple` action and **nothing would happen**: it
jumped straight past `resolve-temple` to the next phase, no city offered, action silently consumed.

I then read the rules to tell *modeling gap* from *economy imbalance* — and it is **primarily a
horizon / modeling gap, not an economy imbalance.** Three code facts settle it:

1. **The payoff is real and large but backloaded onto a level-5 cliff.**
   `apply-end-game-scoring` (game.cljc:3344) awards the priest end-game bonus (+10 Glory)
   **only when `priest == max-role-level` (level 5)**. At Priest-4 I score *nothing*. So the priest
   line isn't underpowered — it's a lump payoff at L5 with zero gradient before it.
2. **The 4th-temple block is a hidden precondition, not an impossibility.**
   `priest-max-temples {1→3, 2→4, 3→5, 4→8, 5→8}` (game.cljc:1298): the 4th temple needs Priest≥2.
   And `resolve-temple-choices` (choice.cljc:585–594) requires a city where my **caravan or a
   magistrate sits AND I have no temple yet**. When it no-op'd I'd either hit the temple cap or had
   no fresh magistrate/caravan city — but **the `choose-action` menu offers `temple` without
   consulting that precondition**, so `find-state-raw` presents it as a live choice when it's a
   guaranteed skip. In solo I confirmed the gate works exactly as written by playing into it knowingly.
3. **Reputation comes from selling/fulfilling, every turn, visibly** — so any shallow, greedy
   evaluation (a bot's, or mine in the seat) *correctly* sees temple/priest as dominated each turn.
   The priest plan only wins if you can value a payoff 6–10 turns out that nothing in the presented
   choice set signals.

**Diagnosis:** the priest weakness is "I couldn't see the flip-late plan from the presented choices,"
not "the path was visible but genuinely not worth it." It's a horizon problem layered on a
representation problem (dead actions look live).

## Which of the armchair Phase A/B fixes survive sitting in the seat

- **Phase B — shallow lookahead + multi-turn resource budget: CONFIRMED as the central fix.**
  The priest payoff lives past the per-turn horizon (the L5 cliff, the multi-turn temple+magistrate
  setup). Only lookahead lets an agent value it. This is the right lever, and the seat made that
  concrete rather than theoretical.
- **Phase B — temple-engine "modeling gap vs economy" diagnosis: ANSWERED — modeling gap.** The
  path is visible-but-late, not invisible-and-worthless. Don't "fix" it by buffing temple/priest
  rewards (that would over-tune an already-large backloaded payoff).
- **NEW finding, not in the armchair list — representation bug in the presented choice set:**
  `choose-action` surfaces `temple` even when `resolve-temple-choices` can only return `:skip`.
  A greedy/sampling bot will "take" a dead temple action and waste tempo; a lookahead bot will mis-rank
  branches that resolve to no-ops. **Filtering the `temple` (and likely other) actions through their
  resolve-precondition before exposing them in `find-state-raw` is a cheap, high-leverage fix** and
  should probably precede the heavier Phase A/B weight work. This is the kind of upstream fix the
  "arc of processing" lesson points at — the symptom (bot undervalues priest) partly traces to the
  choice set lying about what's actionable.
- **Phase A — feat re-selection / pivot-threshold / marginal-value weighting: still looks right,**
  but reframed: the feat layer needs to know that C1/H2-style priest feats are *prerequisite-gated and
  backloaded*, so "marginal value" must be computed against the L5 cliff and the magistrate-placement
  setup, not against immediate rep. Pure weight-tuning without the lookahead from Phase B won't reach it.

## Guardrails honored / honest caveats

- **Read-only:** playing touched no game rules or code. The only accommodation, disclosed in the
  harness source (claude_seat.clj:165–175): the headless `find-state-raw` exposes **no** feat-*claim*
  choice (the human claim path lives only in the cljs UI), so my seat carries an eager claim-cache so
  it routes claim-resolution through the same shared bot primitive. This changes no rule; it's logged
  here as a Phase-0 finding (and is itself a small bot/human-unification gap).
- **Scope of play:** I played a *representative slice* of each game — the 4-seat game through ~R2.5
  (~24 of my decisions, enough to live both the temple trap and the sell-economy contrast and reach a
  clear last place from the priest seat), and a focused solo segment that confirmed the temple gate
  against the code. I stopped turn-by-turn once the felt insight had saturated and resolved the one
  remaining open question (does priest ever pay off?) by reading the scoring code rather than grinding
  ~40 more turns — which would have burned a lot of tokens to extrapolate an endgame the code states
  outright. If you want either game driven literally to game-over, the harness is still wired and I
  can resume.

## Decision pending (per the plan)

The code edits (decision.cljc / personality.cljc / game.cljc) for **Phase A → Phase B → Phase C →
Phase D (GA)** all still wait for your separate go-ahead, each followed by its own reflection. My
recommendation after sitting in the seat: **start with the cheap representation fix (filter dead
`temple`/precondition-gated actions out of the presented choice set), then Phase B lookahead**, since
that's where the real lever is, and re-evaluate Phase A weighting on top of it.
