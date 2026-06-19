# Game AI + Engine Playbook
### Lessons distilled from Eridu and Chroma, for the next game built on the same skeleton

**Audience:** the next time we sit down to build a board/strategy game with strong AI
opponents in this monorepo. Read this *before* writing the engine, not after the first
playtest finds bugs. Everything here is paid for in real debugging time on Eridu (an
11K-LOC, zero-test recovery) and Chroma (a prototype that taught us the one-engine rule).

---

## 0. The one-paragraph version

Build **one** game engine (`apply` + `enumerate-legal`) and make **bots just players with
an automatic move-picker** that calls the same engine — never a parallel "bot heuristic."
The move-picker is a **static evaluator whose weights are a genome** (GA-evolved) **plus a
shallow goal-directed lookahead** for high-value terminal payoffs. Evolve the genome with
**self-play against a *frozen* reference panel**, not pure self-play, and judge it by
**absolute score against the real win condition**, never by ELO/win-rate against an equally
weak field. Encode card/board effects as **machine-checkable typed clauses**, not prose the
AI "approximates." Write the **invariant tests on day one**. That's the whole skeleton.

---

## 1. Architecture: bots ARE players (the one-engine rule)

The single most important structural decision, learned the hard way on **both** games.

- There is exactly **one** code path that (a) enumerates legal choices for a state and
  (b) applies a chosen move to produce the next state. Humans and bots both go through it.
- A **bot is a player plus a pure function** `pick : (state, weights) -> choice` that selects
  among the *same* enumerated legal choices a human is offered. It has no special powers, no
  shortcut application path, no separate rules.
- Persist the bot's "personality" (its weight vector) **on the player state itself**, so the
  exact same decision data is available in simulation, live multiplayer, and offline/PWA.

**Why this is non-negotiable — two real failures:**
- *Chroma:* bots ran a separate placement routine and **never spent the chits they placed**,
  so their hands ballooned. The conservation invariant only held for humans. Fix was to
  delete the bot path and route bots through the one engine; the bug vanished.
- *Eridu:* the live multiplayer server picked bot moves with a standalone `agent-step`
  heuristic that **had no personality and never read the GA-tuned brain**. Months of GA work
  drove *only* the simulator; the bots people actually watched were dumb. Symptom: "boardtest
  bots claim ~0 feats." This is invisible until you check *which function the live loop
  calls* — so check it explicitly, in every entry point (WS create, demo/generate, POST,
  offline).

**Test for it:** run games with bots and assert **conservation invariants** (resources in =
resources out + spent; piece counts; score components reconcile). Run an **adversarial random
agent** through the same engine — if it can desync state, a personality can too.

---

## 2. The decision algorithm: static evaluator + goal-directed lookahead

Think of the bot as *"an agent with an overarching decision matrix that is weighted and
guided by genetics, plus contextual/situational weighting."* Concretely two layers:

1. **Static evaluator (the genome).** A weighted sum of features of each candidate move:
   immediate points, role/track progress, resource economy, board-bonus value, opponent
   denial, synergy, etc. The weights are the **genome** the GA evolves. Keep features
   **interpretable** — you will read them when diagnosing.
2. **Shallow, goal-directed lookahead (the "horizon").** The static evaluator alone is
   myopic: it cannot see *"achieving A unlocks B."* Bots stall one step short of every
   high-value terminal payoff. The fix is **potential-based shaping / lookahead** toward
   *known* terminal goals — propagate a fraction of a goal's value back onto the moves that
   make it reachable. This is *not* deep tree search (see §4); it's "value the staircase, not
   just the step."

**Model synergy, not a scalar.** "Best feat" / "best bonus" / "best role" is **context
dependent** — it depends on what's reachable, what you've already built, and what it unlocks
next. Score a *combination* (ease × pivot-ability × point-value × synergy × what-it-enables),
not an isolated number. A flat "feat-awareness 0.7" gene cannot express this; a forecasting
term that reads the actual board can.

---

## 3. Evolving the genome without the three traps

### Trap A — Self-play overfitting / monoculture collapse
Pure self-play collapses to a single mutually-exploiting strategy and **inflates fitness**
(everyone beats everyone at a bad game). Cures, all used in Eridu:
- **Frozen reference panel:** a fixed set of hand-built archetypes + role-specialist
  adversaries (a feat-racer, a denier, an engine-builder). Score each organism partly by how
  it does against this *external, unchanging* gradient. (This is exactly AlphaGo's
  "play against frozen earlier checkpoints" idea — see §4.)
- **Diversity-in-selection / region caps:** cap how many of the population may occupy the
  same strategic "region," so niches survive.
- **Capability-as-gene, not bot-tweaks:** when a behavior is missing, add it as a *weighted
  capability the GA can dial*, don't hand-code a special case. The GA then discovers when to
  use it.

### Trap B — The dead-gene trap (verify the whole genome is wired)
Eridu's GA spent a full evolution **optimizing two genes the decision function never read** —
the personality cache stored only 5 of ~54 keys, and `:feat-synergy` / `:bonus-foresight`
were silently dropped. Their "evolved" values were pure drift. **Before trusting any GA run,
assert that every gene in the genome is actually consumed by the evaluator** (a simple test:
perturb each gene in isolation and confirm the chosen move distribution can change). A gene
the evaluator ignores is worse than useless — it wastes optimization pressure and lies to you.

### Trap C — The ELO illusion (measure absolute skill)
ELO and panel win-rate are **self-referential**: when the whole field is weak, "beat the
field" measures nothing. Eridu bots had healthy-looking ELO (~1700, climbing) while scoring
~12 against a 20-point target. **Always also measure absolute performance against the game's
real scoring / win condition**, on a fixed yardstick, across many games. For Eridu the right
dashboard was: mean reputation, feats claimed, role levels reached, resources left unspent,
track balance — not ELO. Build that absolute-metric harness early and look at it first.

> Corollary: a GA can only climb toward the ceiling its **evaluator + lookahead** allow. If
> the bots are absolutely weak, *fix the decision logic first*, then re-evolve. Re-running the
> GA on a broken evaluator just finds the best of a bad bunch.

---

## 4. AlphaGo / Arimaa-style ideas, and what actually ports

These worker-placement / set-collection games are **not** chess. Branching is wide
(many spaces × many resource combos), turns are not zero-sum two-player, and a single move's
value depends on long-horizon engine-building. Lessons from how AlphaGo and Arimaa AIs were
built, translated to our setting:

- **Arimaa's lesson:** it was *designed* to defeat brute-force minimax (huge branching, where
  deep search is futile and **good static evaluation + selective expansion** wins). Our games
  are similarly search-hostile. **Don't reach for deep game-tree search.** Invest in a strong
  static evaluator (the genome) and *selective, shallow* lookahead toward goals — expand only
  the few branches that plausibly reach a terminal payoff.
- **AlphaGo's two-part shape — policy + value — maps cleanly:**
  - *Policy* ≈ our static evaluator: "which move looks good here." GA-evolved weights are a
    cheap stand-in for a learned policy net.
  - *Value* ≈ our goal lookahead: "how good is the position really, accounting for what it
    leads to." Potential-based shaping is a hand-built value signal.
  - If a game ever justifies it, this is the natural place to drop in a small **learned value
    model** trained on self-play game outcomes, replacing the hand-tuned forecasting term.
- **AlphaGo's self-play-against-frozen-checkpoints == our frozen reference panel.** Same
  anti-overfitting mechanism. Periodically *promote* the current champion into the frozen
  panel so the gradient keeps rising (curriculum), but never let the *whole* gradient be the
  live population.
- **MCTS-lite when you can afford it:** for a tight tactical sub-decision (e.g. "is this the
  turn to commit the 4 resources to a terminal bonus?"), a few rollouts using the static
  evaluator as the rollout policy beats a pure greedy pick — that's MCTS with a cheap default
  policy, the AlphaGo skeleton in miniature. Use sparingly; profile first.

**The horizon bug is the recurring boss.** In Eridu it showed up three ways: bots stopped at
one feat (didn't set up feat #2), stalled at role level 4 (never closed the +10 to level 5),
and hoarded resources they could have converted. All three are the *same* failure — the
static evaluator doesn't propagate terminal value backward. **Budget for lookahead/forecasting
from the start; don't bolt it on after the GA "plateaus."**

---

## 5. Rules fidelity: typed clauses, not prose the AI guesses

Eridu's worst recurring class of bug — "a board bonus does the wrong thing every game" — came
from **AI approximations of prose card/board text.** The cure, and now the day-1 standard:

- **Encode every card/board effect as a machine-checkable, typed clause schema** (structured
  data: effect-type, target, magnitude, condition, optional flag), *not* free text the engine
  interprets ad hoc. The same structured clause drives the engine, the bot's valuation, and
  the tests. One source of truth.
- **Normalize shape early.** A real bug: a role's level-5 cost was a *vector* `[:pottery :gold]`
  while levels 3–4 were *scalars*; a helper that built a "needed resources" set put the vector
  in raw, so the bot was blind to the requirement. **Wherever a field can be scalar-or-vector
  (cost, target, effect), flatten/normalize at the boundary** or it *will* silently break one
  code path.
- **Ask about ambiguity before implementing it.** For *every* game: when a rule is ambiguous,
  ask the designer first; don't ship an AI guess. And **build strong, structured game logs**
  (every effect, claim, conversion tagged) — they are how you diagnose both rules bugs and AI
  weakness later. Half this session's diagnosis was only possible because the summary exposed
  amity/glory/roles/resources per game.

---

## 6. The Claude self-play + multi-agent observation walkthrough

How we found the bugs the GA couldn't — a repeatable process for a new game:

1. **Instrument absolute metrics** (per §3C): a harness that plays N games with the current
   brain and reports the real scoring components, role/track progress, unspent resources,
   subsystem engagement — not ELO.
2. **Sweep all rules slots for gaps.** Fan out parallel agents, each owning a slice of the
   content (e.g. all board bonuses, all feats), each checking engine behavior against the
   typed clause. Catches "AI approximation" drift exhaustively, not anecdotally.
3. **Multi-perspective observation.** Run a batch of games and have several agents watch from
   *different lenses* — a feat/goal analyst, a resource-economy analyst, an opponent-denial
   analyst, a board-bonus analyst. Each is blind to the others; union of findings beats one
   generalist. (This is how the dead-gene cache, phantom feats, and agent-step divergence
   surfaced.)
4. **Adversarially verify every finding.** Before acting on "this is a bug," spawn skeptics
   prompted to *refute* it; keep only what survives. Prevents plausible-but-wrong fixes.
5. **Diagnose with disambiguating measurements.** Don't stop at "bots are weak." Decompose:
   *stuck at level 3 (won't commit) vs level 4 (can't close)?* *Lacked resources vs held them
   and didn't spend?* The fix is different for each; the measurement tells you which.
6. **Fix the decision logic, THEN re-evolve.** Genome tuning is the *last* step, on a correct
   evaluator — not a substitute for fixing it.

> Self-play here means *Claude* playing the game (driving the bots, watching the logs,
> forming hypotheses), not just the GA's organisms playing each other. The agent-as-analyst
> loop is where the qualitative bugs die; the GA only does quantitative weight-fitting.

---

## 7. Test-first & invariants (day-1, every game)

- **Conservation invariants** as property tests: resources, pieces, and score components must
  reconcile every turn, for bots and humans alike.
- **One-engine tests:** the bot loop and the human loop hit the same `apply`/`enumerate`.
- **Bot sanity invariants:** a bot never skips a strictly-dominant free action; never ends a
  turn holding the exact resources for the single highest-value move while passing; etc.
- **Genome-wiring test:** every gene measurably influences move choice (§3B).
- **WS/handler tests:** the live multiplayer path is the one users see — test it, don't just
  test the sim.
- Eridu reached **~120 tests / 2250+ assertions**; that suite is what let us refactor the
  engine repeatedly without fear. Start it on commit #1, not after the recovery.

---

## 8. UI philosophy (Mohammad's games)

- **Direct manipulation of the board beats button menus.** Drag the piece, tap the space;
  don't present a list of textual actions.
- **Commit only at end of turn** — let the player arrange the whole turn and confirm once,
  with undo until commit. (Bots, per §1, commit through the same enumerate/apply, just
  without the UI.)

---

## 9. Porting checklist for the next game (the skeleton)

When standing up a new game on the Eridu/Chroma bones, in order:

1. **State + one engine:** `initial-state`, `enumerate-legal`, `apply-move`. cljc so it runs
   server, sim, and browser.
2. **Typed effect schema** for all content; normalize scalar/vector shapes at the boundary.
3. **Structured logging** of every effect/conversion/claim.
4. **Conservation + genome-wiring + one-engine tests** before content grows.
5. **Bot = player + `pick(state, weights)`**, weights cached on player state; *one* pick
   function used by sim, live, and offline. Verify each live entry point calls it.
6. **Static evaluator** with interpretable features; **goal-lookahead** term for terminal
   payoffs from the start.
7. **Absolute-metric harness** tied to the real win condition.
8. **GA with a frozen reference panel**, diversity caps, capability-as-gene; promote champions
   into the panel as a curriculum.
9. **Diagnose absolute weakness → fix evaluator/lookahead → re-evolve.** Never trust ELO as
   the quality bar.
10. **Multi-agent sweep + observation + adversarial verify** as the QA loop.

---

## 10. Bug-class catalog (the ones that recur — grep your new game for these)

| Class | Eridu/Chroma instance | Generalized smell |
|---|---|---|
| Parallel bot path | `agent-step` (Eridu), separate placement (Chroma) | bots don't go through `apply`/`enumerate` |
| Dead gene | 5-of-54-key personality cache | genome key the evaluator never reads |
| ELO illusion | ELO 1700 / rep 12 vs 20 | judging by relative win-rate on a weak field |
| Horizon / terminal stall | one feat, stuck at role-4, hoarded resources | evaluator doesn't propagate terminal value back |
| Scalar-vs-vector shape | role-5 cost `[:pottery :gold]` unflattened | a field that's sometimes a collection |
| Prose-approximated effect | "board bonus wrong every game" | engine interprets text instead of typed clause |
| Bottleneck scoring missed | `reputation = min(amity, glory)` | score = min/least of components; bot pumps one |
| Phantom target | event-feats valued at ~100%, fire ~20% | progress proxy overstates true claimability |

---

*Living document. When the next game teaches us a new class, add a row to §10 and a
paragraph where it belongs. The skeleton is the point — content changes, these bones don't.*
