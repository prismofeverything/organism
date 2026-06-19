# Eridu bot skill diagnosis — handoff to the feat-lookahead worktree

**Author:** diagnosis session (2026-06-18). **No code changed** — this is findings only.
**Scope:** absolute-skill bugs *other than* the feat-chaining horizon (which feat-lookahead
already targets). Measured against the **current post-lookahead brain** (HEAD 658c5fc),
`evolved-top` from `output/bench/evolved-population.edn`, via `sim/run-game`.

> Caveat on framing: ELO and panel win-rate are **self-referential** — beating a weak
> field/panel says nothing absolute. The number that matters is **reputation**, and it's
> still ~8.8 solo / ~11.7 4p against a 20-point target. Everything below is measured in
> absolute reputation, not ELO.

---

## TL;DR — one lever dominates: **role-5 closure**, and it doubles as the balance fix.

| Lever (absolute, not ELO) | Worth | Bots get it | Root cause |
|---|---|---|---|
| Reach a level-5 role (+10) | **+6.2 rep** | only **25–29%** of games | stuck-at-L4 (below) |
| Balance amity/glory (`rep=min`) | **+2.9 rep** | only ~40% balanced | *same lever* — see below |
| Role monoculture (cause) | — | only **1.6–1.9** roles reach L3+ | `(* pri 3)` over-weight |
| Resource hoarding | ~4–6 res/game unspent | — | conversion + planning bugs |

These are **interlinked and all distinct from the chaining horizon.**

---

## Why role-5 and balance are the SAME lever

Two design facts (`game.cljc:1307` and `:1317`):

```clojure
role-threshold-costs {:merchant {3 :pottery 4 :gold 5 [:pottery :gold]} ...}  ; L5 needs TWO resources
role-end-game-bonus  {:merchant {:track :glory :points 10} ...}              ; +10 to the OPPOSITE track
```

- Merchant/priest earn **amity** in-game → their L5 bonus is **+10 glory**.
- Raider/leader earn **glory** in-game → their L5 bonus is **+10 amity**.

Since `reputation = min(amity, glory)`, the +10 lands on a maxed role's **weak** track —
it lifts the floor. That's *why* a level-5 role is worth +6.2 rep empirically, and why
closing role-5 is simultaneously the cure for the imbalance leak (~50% of games end lopsided,
gap 4.6–5.3). **Fix role-5 closure and you get the balance delta for free.** Don't build a
separate "balance" heuristic first.

---

## The stall is at L4, and it's two bugs

Top-role level distribution (evolved-top, n=300):

```
        solo            4p
L3      26%             32%
L4      43%   ← stall   41%   ← stall
L5      30%             24%
```

**~41% of games the top role parks at exactly L4 and never closes the +10 jump.**
Of those stuck-at-L4 games, at game end the bot held the two resources the L5 jump needs:

```
            BOTH (had them, didn't close)   ONE      NEITHER    avg unspent res
solo                  29%                   44%       27%           4.2
4p                    47%                   37%       17%           6.1
```

### Bug 1 — acquisition: `needed-resources` is blind to L5 (CONFIRMED code defect)

`decision.cljc:337 needed-resources` collects each role's next-level threshold cost into a
set. For L3/L4 the cost is a single keyword; **for L5 it's a vector `[:pottery :gold]`**, so
the set ends up holding the *vector*, and `resource-planning-bonus` (`:348`, does
`(filter needed space-resources)`) never matches an individual resource. Reproduced:

```
merchant L2→L3 needed: #{:pottery}          contains :pottery? true
merchant L3→L4 needed: #{:gold}             contains :gold?    true
merchant L4→L5 needed: #{[:pottery :gold]}  contains :pottery? FALSE  contains :gold? FALSE
```

→ Once at L4, the bot gets **zero planning pressure** to bank pottery+gold. Explains the
17–44% under-banked. **Fix:** flatten vector costs, e.g.
`(mapcat #(if (vector? %) % [%]) costs)` into the needed set.

### Bug 2 — conversion: had both resources, still didn't close (47% in 4p)

Even when both L5 resources are in hand, the bot frequently never takes the role-increase
action (and ends with 6.1 resources unspent in 4p — heavy hoarding). The strong
`near-max-bonus` (`decision.cljc:688`, −9.6 at L4) lives in the **role-choice** scoring
(*which* role to bump once you've decided to), not necessarily in the **action-choice** that
decides *whether* to spend the turn on role-increase vs sell/travel/etc. Worth verifying the
role-increase action gets a comparable terminal-value boost when L4 + both resources held.
**Timing caveat:** "held both at game end" doesn't prove there were spare turns to close —
but 6.1 unspent resources strongly implies under-conversion, not just bad luck.

> This is the same *shape* as your feat-lookahead — a known terminal payoff the bot should
> plan + hold resources toward — but applied to the **role-5 track bonus**, not contests.
> If the lookahead generalizes to role-5 terminal value cleanly, that's the highest-value
> extension. It does **not** overlap with the contest-feat horizon.

### Bug 3 — monoculture: only 1.6–1.9 roles reach L3+

Per-role mean level (4p): merchant 3.68, leader 2.60, raider 1.84, **priest 1.60**. The bot
tunnels one role. In the role-selection sort key (`decision.cljc:724`):

```clojure
[(+ (* pri 3) level near-max-bonus glory-adj feat-role-adj compete-adj te-role-adj ...) role]
```

`(* pri 3)` gives the top-priority role a +0 vs +3/+6/+9 for the rest — a 3-point-per-rank
moat — while the only diversifying counter-pressure is `level` (×1). So pumping the priority
role from L3→L4 (key 0+3=3) always beats opening a 2nd role at L1 (key 3+1=4). Result: one
track grows, the opposite track (hence `min`) starves. Softening the `(* pri 3)` dominance or
strengthening diversification once the lead role hits ~3 would open a 2nd track — which helps
balance *and* feats that need multiple roles (e.g. H1 needs two roles at 3+).

---

## Suggested priority for the lookahead worktree

1. **Bug 1** (one-line flatten) — cheap, unblocks all L5 acquisition planning.
2. **Bug 2** — make role-5 a terminal goal in the action choice (lookahead-style): when a role
   is at L4 and both L5 resources are held, strongly prefer the role-increase action; add
   late-game pressure to convert hoarded resources rather than sit on 4–6.
3. **Bug 3** — soften role monoculture so a 2nd track opens (balance + multi-role feats).

Expected ceiling: role-5 hit-rate 25–29% → if pushed toward ~70% (it's a strong-by-design,
4-resource, blockable path — designer confirmed intentional), that's roughly +6 rep × the
extra ~40% of games ≈ **+2–2.5 avg rep**, plus the balance delta it carries. Meaningful
progress toward 20, on top of whatever the chaining horizon adds.

## Repro scripts (in /tmp, read-only sims)
`diag_skill.clj` (leak overview), `diag_roles.clj` (per-role + role-5/balance value),
`diag_maxrole.clj` (L4 stall histogram), `diag_bank.clj` (held-resources at stall).
