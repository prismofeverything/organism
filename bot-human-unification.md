# Bot / human input unification — divergence map

Task: make the bot a pure decision-making module that scores the **same**
presented choices a human sees, driven by the (unchanged) GA-evolved
personality weights, with context-modulated scoring.

This document maps the two arms *before* the change and pins down exactly where
they diverged.

## What was ALREADY unified (verified, not assumed)

Contrary to the original framing of "two parallel arms", the choice-enumeration
layer was already shared:

- **One choice interface.** Both arms consume `eridu.choice/find-state-raw`,
  which returns `[phase choices-map]` where `choices-map` is
  `{choice-key → next-state}`. The human UI (`src/cljs/eridu/play.cljs`) renders
  these keys; the bot scores the same map.
- **One protected-phase / trivial-advance contract.** Bot, replay, simulate and
  the live socket all gate on `game/bot-protected-phases` and advance via
  `choice/advance-through-trivial` (see `eridu_ws.clj`, `simulate.clj`,
  `persist_eridu.clj`, `play.cljs`).
- **GA fitness already flows through the interface.** `eridu.evolve/run-tournament`
  → `sim/run-game` → `pers/personality-step` → `find-state-raw`. The Elo earned
  by a weight vector is already earned from games played through the unified
  choice interface. No retarget of `evolve.clj` is required — it is wired
  correctly today; this is now documented in its docstring.
- **The bonus-effect dual-arm was already collapsed.** `apply-bonus-dispatch` is
  a single choice-aware dispatch (`game.cljc`), and the `effect-spec` scaffold is
  at 175/175 authored. (See `bonus-systemic-fixes.md`.)

## Where the arms actually DIVERGED

The divergence was **not** in the move set; it was in how the bot *interprets* a
choice. The human reads a rendered choice and its consequence. The bot's scorer
(`personality-step`, the big `case phase`) re-derived the meaning of a choice by
reaching back into engine internals rather than reading the presented choice.

The worst offender, and the one this change fixes:

| Phase | Bot read (engine-internal) | What the human sees / the presented choice |
|-------|----------------------------|---------------------------------------------|
| `:choose-action` | `(nth (:actions (get game/action-spaces space)) idx)` — recomputes the action descriptor from the static action-space table keyed by the opaque index `idx` and the hidden `[:player-turn :space]` | The choice's **next-state** already carries `[:player-turn :action]` (set verbatim from that same `nth` at `choice.cljc:392`). The action type/resources are a property of the *presented choice*, reachable as `(get-in next-state [:player-turn :action])`. |

The other protected/decision phases already key on **meaningful presented
values** — role keywords (`:choose-role-increase`), city keywords
(`:resolve-temple`/`:resolve-travel`), route vectors (`:resolve-deploy`),
resources (`:resolve-sell`/`:travel-continue`), and the choice-key set itself
(`:resolve-landing`, `:choose-action`'s `:done`/`:free-travel`). The planning
phases (`:choose-die`, `:choose-astronomer`) reason over **public rule
functions** (`move-astronomer-clockwise`, `space-action-types`) — the same
board reasoning a human does, not hidden state — and are left as planning logic.

## The fix

1. **`eridu.decision`** — a new namespace holding the unified decision-making
   algorithm. `decide` is the single entry point with the explicit contract:
   read the presented choices → extract per-choice **features** from the
   `(choice-key, next-state)` pair → process the personality weights **through**
   those features with **context** modulation (game-state-derived adjustments) →
   return the argmax choice. The weight vector is an *input* to this algorithm,
   never the scorer itself.
2. **`:choose-action` now reads the presented choice.** The action descriptor is
   taken from the choice's next-state (`[:player-turn :action]`), not from
   `game/action-spaces`. This is value-identical by construction (the next-state
   field was set from exactly that lookup), so it preserves the GA-evolved
   behavior while removing the engine-internals read.
3. **`personality-step` delegates to `decision/decide`.** The genetics (the
   weight schema, archetypes, GA operators) stay in `eridu.personality`
   unchanged. One decision module, one expression of the genetics.

## Validation

- Full `lein test eridu.*` regression suite (incl. `bonus-effects-test`,
  `effect-spec-test`, `game-test`) — must stay at 0 failures.
- `eridu.decision-test`: identical legal move-set at every protected phase;
  context-responsiveness (the scorer's choice changes when only game-state
  context changes); no engine-internals leak in `:choose-action`.
- Before/after headless `simulate` behavioral batch — per-archetype mean
  reputation must stay within noise of the pre-change baseline.
- Multi-agent scaffolded review + adversarial testing pass.
