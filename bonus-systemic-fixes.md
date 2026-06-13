# Bonus effects — systemic fixes over point patches

Distilled from authoring the full 175-slot `eridu.effect-spec` against the live
dispatch code. The codebase's problem is **patch accumulation**: three parallel
hand-maintained case-tables (`apply-bonus-dispatch`, `apply-bonus-with-choice`,
`bonus-needs-choice?`) plus a status map and an oracle, each edited
independently. The gaps are not 58 unique bugs — they are ~7 structural seams,
each of which a single change closes across many slots.

Slot lists below are exact (from the scaffold). "Point patch" = fix one arm.
"Systemic fix" = the structural change that closes the whole class.

---

## The meta-fix: one clause interpreter over the spec

**Today:** every effect is hand-written three times — the bot/auto arm
(`apply-bonus-dispatch`), the human arm (`apply-bonus-with-choice`), and the
choice descriptor (`bonus-needs-choice?`). They drift (S3, S4) and clauses fall
out of one arm but not another (S1, S5).

**Structural fix:** make `effect-spec/effect-specs` executable. Write ONE
interpreter that walks a slot's `:clauses` in order, dispatching each `:kind` to
a shared helper in a clause-handler registry; generate the choice descriptor from
the `:interactive?` clauses instead of a separate case. That single engine
replaces all three tables and structurally eliminates **S1–S5 and S7 at once** —
there is no longer a second arm to diverge from, and a missing clause is missing
in exactly one place (visible, and caught by the scaffold test).

The scaffold is the precondition: the clause data already exists. The fix passes
below are then "supply the handler for `:kind X`," each unblocking every slot
that uses that clause.

---

## S1 — `sell` proxied as flat amity/glory  (~17 slots)

`auto-sell-in` (already in `apply-bonus-with-choice`) does a real sell: dec
resource, drop demand, +demand-token, +merchant-score amity, fire `:sold`. Most
slots instead hard-code `+N amity`/`+N glory`.

Slots: `[3 4] [6 3] [8 2] [8 3] [9 4] [11 2] [12 3] [16 4] [17 4] [19 2] [23 2]
[26 3] [27 1] [28 3] [29 2] [32 1] [35 1]` + passives `[4 0]`(done) `[10 0]`(stub)
`[24 0]`(stub). "double"/"twice" modifiers (`[6 3] [11 2] [23 2]`) ride on top.

**Fix:** route every `:sell` clause through one sell helper that honors
`:modifier :double` / `:count`. ~17 point patches → 1 handler.

## S2 — `influence-magistrate` dropped or proxied  (~7 slots)

`do-influence` (already in `apply-bonus-with-choice`) traces the path and flips
raiders. Board 30's entire row drops it and proxies a role-level score.

Slots: `[20 3] [25 1] [30 1] [30 2] [30 3] [30 4] [32 4]` + passives
`[5 0]`(stub) `[33 0]`(stub).

**Fix:** route every `:influence-magistrate` clause through `do-influence` in
both paths. Closes the half-built Board 30 wholesale.

## S3 — dual-path divergence (Gap 3) — structural, not just `[17 1]`  (15 slots)

Auto and human arms authored separately and drifted. `[17 1]` is the worst
(auto flips a raider, human grants a resource, rule places a raider) but it is a
*class*: `[3 4] [9 4] [17 1] [18 1] [19 3] [20 3] [21 3] [25 1] [30 1] [30 2]
[30 3] [30 4] [32 4] [33 2] [35 4]`.

**Fix:** the meta-fix above. One interpreter ⇒ no second arm ⇒ nothing to
diverge. This is the single highest-leverage change in the whole effort.

## S4 — choice-TYPE divergence  (≥2 slots, likely more)

The boolean choice check passes but the *type* is wrong: `[35 4]` is tagged
`:pick-role` and the human path INCREASES a role, though the card is
"influence + score raiders"; `[17 1]` is tagged `:pick-resource` for a placement.
The pick-resource bucket (`[3 3] [17 1] [22 3] [23 3] [25 4] [31 3] [31 4]`) is a
catch-all that doesn't match several cards' real choices.

**Fix:** derive the choice descriptor from clause kinds (place→pick-city,
gain→pick-resource, increase→pick-role, influence→pick-city-magistrate), not a
hand-maintained case. Then add a scaffold test asserting `bonus-needs-choice?`
*type* matches the interactive clause's implied type (today we only check
presence). Catches S4 as a build error.

## S5 — `place-demand` dropped or proxied  (6 slots)

`[5 2]` and `[14 3]` place demand tokens correctly (draw from `:demand-bag`,
push onto `:city-demands`). Others proxy it as flat resources/amity.

Slots: `[8 2] [11 1] [16 4] [22 2] [24 2] [28 3]`.

**Fix:** one `place-demand` helper used by every `:place-demand` clause.

## S6 — passive stubs needing ENGINE hooks  (8 passives — NOT point-patchable)

These are the genuinely structural ones; group by the hook each needs:

| Slot | Needs |
|------|-------|
| `[5 0] [14 0] [31 0]` | movement / astronomer-position tracking across turns |
| `[10 0] [24 0]` | a sell action *outside* the normal sell phase |
| `[21 0]` | **data-model change** — temples are keyed by city, can't hold two; needs a temples-as-collection refactor |
| `[30 0]` | take goods from an alternate astronomer's wheel location |
| `[33 0]` | insert an influence action as a deploy side-effect |

**Fix:** these don't get point-patched — they need the engine capability first.
`[21 0]` in particular gates on reshaping the temple data model (worth doing once;
several "even if you already have a temple there" cards quietly depend on it).

## S7 — optional conditional tails dropped  ("then you may …")  (3+ slots)

`[17 3]` "then you may flip", `[18 3]` "then you may flip", `[26 4]` "if you
surround it, place a temple". Always the last, optional, interactive clause.

**Fix:** a generic optional-clause step in the interpreter (S3 meta-fix), gated
on a yes/no when interactive.

---

## Recommended sequence (reconciling patches into structure)

1. **Build the clause interpreter (S3 meta-fix)** over `effect-specs`, with a
   handler registry. Port the cleanest ~20 slots first to prove parity against
   the oracle, then retire `apply-bonus-dispatch` arms as each `:kind` lands.
2. **Land S1 (sell) and S2 (influence) handlers** — they alone clear ~24 of the
   32 optimistic slots, because most proxies are sells or influences.
3. **Derive choice descriptors from clauses (S4)**; delete `bonus-needs-choice?`'s
   hand case; add the type-match scaffold test.
4. **Land S5 (place-demand)**.
5. **S6 engine hooks** last, grouped by capability; do the `[21 0]` temple
   data-model refactor as its own change.

Each step is verified by re-running `lein test eridu.effect-spec-test` (the
known-gaps shrink) and diffing against `bonus_oracle.clj` deltas in the
`bonus-coverage/` bench. A slot is "done" when its clauses are all `:done` AND
the oracle delta matches.
