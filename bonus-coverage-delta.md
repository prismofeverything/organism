# Bonus coverage-delta report — bug-report correlation + multi-perspective review

**Date:** 2026-06-13
**Scope:** Correlate the 6 in-game bug reports (`~/Documents/eridu-bug-reports.jsonl`)
against current bonus-effect coverage, re-framed against the *completed* bot-arm
unification (`d1b10d8`) and the deferred clause-interpreter meta-fix.
**Method:** report-note parse + static code verification of each load-bearing claim
(handlers in `src/cljc/eridu/game.cljc`, contest logic, `perform-influence`). The full
staged re-audit (`bonus-coverage/run-stage1..5`) was **not** re-run inline — it is a
long bench job; `bonus-audit-findings.md` is current as of today's commits. Snapshot
*replay* of each EDN state was not performed; verdicts below are from code inspection,
flagged where a replay would harden them.

---

## Correlation table — 6 reports → coverage

| # | play-key | Player note (abridged) | Maps to | Status |
|---|----------|------------------------|---------|--------|
| 1 | trigger test | slot 5: "didn't get to select a raider to place the temple next to"; "passive didn't work" | **[5 3]** (choice-type-wrong / dual-path) + **[5 0]** passive | OPEN — known finding + S6 passive stub |
| 2 | handtest | "Kish surrounded by raiders but unable to score E1" | **E1 contest** topology | ✅ FIXED by `56b0835` (E1 repro true) |
| 3 | handtest | board 35 slot 0 passive: "had no goods… did not get a choice to select goods" | **[35 0]** turn-start passive | ✅ FIXED (defensive) by `56b0835` BUG B — *verify via replay* |
| 4 | splooooo | "slot #2 on board 18… travel through a prompt window, one space" | **[18 2]** (travel + conditional glory) | OPEN — known finding (Bucket B) |
| 5 | splooooo | "bonus move to Babylon, did not pick up my point raider… systemic issue with prompt based actions instead of action resolution" | **SYSTEMIC** — bonus-travel teleport drops travel side-effects | OPEN — **root-cause confirmed in code** |
| 6 | splooooo | "Board 18 position 1 moved a magistrate across a road when it says river" | **[18 1]** river-`:type` constraint dropped | OPEN — **NEW class, confirmed in code** |

---

## Multi-perspective verdicts

### Report 5 — systemic prompt-vs-resolution (HIGH; the meta-fix's reason to exist)
The player self-diagnosed it correctly. The bonus-travel handlers implement travel as a
direct caravan move, e.g. `[18 2]`:

```clojure
[18 2] (let [s (if choice (assoc-in state [:players player-key :caravan] choice) ...
```

This `assoc-in … :caravan choice` **teleports** the caravan to the destination. It never
traverses the route path, so the real travel-action side-effects — point-raider pickup,
river-crossing triggers — are silently dropped. A bot/auto path that resolves a *travel
action* would pick the raider up; the human prompt path does not. This is the **dual-arm
split** in its purest form, and it is exactly what step D's clause-interpreter is meant
to erase by making "travel" a single executable clause both arms run.
- **structural:** fits S3/S4 (dual-path + prompt-as-teleport), but is broader — it's a
  *travel-action-resolution* gap affecting every bonus that grants travel.
- **adversarial (attempt to refute):** could the caravan-move be the intended shortcut?
  No — the card grants "a Travel action," and the player observed the raider left behind;
  a real travel action picks it up. Refutation fails → bug stands. *(Replay of report 5's
  snapshot would make this airtight.)*
- **bot-vs-human-arm:** the auto arm resolves the action; the human arm teleports. Unify
  on action-resolution, per the redesign direction.

### Report 6 — [18 1] river constraint dropped (HIGH; NEW class)
`[18 1]` routes through `bonus-influence` → `perform-influence` → `road-clockwise-path`,
which walks **road** edges clockwise (`road-clockwise-next`) with **no filter on route
`:type`**. The card (per the report) restricts movement to a **river** route. Nothing in
the path builder or the handler consults `(:type r)`, so a magistrate moves along a road
where only a river move is legal.
- **NEW class** beyond the documented S1–S7: *route-type / terrain constraint dropped on
  magistrate movement.* None of S1–S7 covers terrain-typed movement. Recommend adding it
  as **S8 — typed-movement constraints** (river-only / road-only edges).
- *Confirm the card text for `[18 1]` against the spec/oracle before locking the fix; the
  verdict rests on the player's reading of the card.*

### Reports 1 & 4 — already-known findings
- **[5 3]** "Take a Deploy then a Temple action": findings already flag it choice-type-wrong
  + dual-path (human path inserts a spurious travel and drops the temple). The player's
  "didn't get to select a raider to place the temple next to" is the same defect surfacing.
- **[18 2]** travel + conditional glory: known Bucket-B finding (grant 0 glory when unmet).
  The "travel one space through a prompt" is the same teleport mechanism as Report 5.

### Reports 2 & 3 — closed today
Both fixed by `56b0835` (contest topology via stored `:routes`; defensive turn-start
passive surfacing). Report 3's fix is defensive — **a snapshot replay should confirm** the
`[35 0]` "no goods → choose a good" prompt now fires at turn start.

---

## Seam ledger delta

| Seam | Before | After this correlation |
|------|--------|------------------------|
| S3 dual-path divergence | unified (`d1b10d8`) | holds; Report 5 shows a *travel-resolution* residue the unification didn't reach |
| S4 choice-type | planned | Reports 1 ([5 3]) confirm it bites players |
| S6 passive engine hooks | partial | Report 1 ([5 0]) + Report 3 ([35 0], now fixed) are live instances |
| **S8 (new) typed-movement** | — | **Report 6 ([18 1]) river-only constraint dropped** |

---

## Recommended next steps (NOT executed — awaiting greenlight)

1. **D — clause-interpreter meta-fix (large refactor; deliberately deferred by the prior
   agent).** The observation confirmed `effect_spec.cljc` is authored but the interpreter
   that would *replace* the 175-branch hand-maintained dispatch is not built. Reports 5/1/6
   are all "a clause was dropped on one arm" — precisely what one executable interpreter
   closes. **This is a multi-hour rewrite of live dispatch; recommend an explicit go before
   I rewrite + commit it,** ideally landed incrementally (travel-resolution clause first,
   since Report 5 makes it the highest-value single clause).
2. **Travel-as-resolution fix (smaller, high-value).** Make bonus-travel run the real
   travel action (path traversal + point-raider pickup + river triggers) instead of
   `assoc-in :caravan`. Closes Report 5 and the [18 2]/[18 4] family directly. Good first
   increment of D.
3. **[18 1] river gate (S8).** Add a `:type`-aware movement path; confirm card text first.
4. **E — regression tests.** Add a case per closed/confirmed gap to
   `eridu.bonus-effects-test`: (a) bonus-travel picks up a point raider on the path,
   (b) `[18 1]` rejects a road-only move, (c) `[5 3]` prompts for temple placement, plus
   replay-backed cases for `[35 0]` and E1. **Held back from the green suite** — adding
   currently-failing tests would break CI; land them *with* each fix.
5. **B — full staged re-audit.** Re-run `run-stage1..5` **detached** (long bench) and diff
   against `bonus-audit-findings.md` for a complete seam sweep; offer to kick it off.
