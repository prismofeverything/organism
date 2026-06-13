# Bonus-board code-path walkthrough suite

**Scope:** all 35 bonus boards x 5 slots = **175 effects**. Slot 0 is the persistent
passive; slots 1-4 are the instant bonus slots. Read/document only — no code changes.

Sources: `src/cljc/eridu/bonus.cljc` (effect-implementation-status, bonus-needs-choice?)
and `src/cljc/eridu/game.cljc` (apply-passive / apply-passive-dispatch,
apply-bonus-effect / apply-bonus-dispatch, apply-bonus-with-choice).

**Entry points**
- Instant slots 1-4 enter through `apply-bonus-effect` -> `apply-bonus-dispatch`
  (keyed by `[board-id slot-idx]`). If `bonus-needs-choice?` returns a descriptor,
  the human/UI path instead routes through `apply-bonus-with-choice` once the player
  picks; the dispatch arm is the bot/no-UI auto-resolve fallback.
- Passive slot 0 enters through `apply-passive` -> `apply-passive-dispatch`
  (keyed by `[board-id trigger-type]`), gated by `has-passive?`. Some passives stash
  `:passive-choice-needed` resolved later by `apply-passive-choice`.

**Honesty tally (all 175 slots):** implemented=117, partial=23, persistent=35  (total=175)

> **Slot-5 bug cross-reference.** The in-game report used "slot 5" but the code
> indexes slots 0-4. Player-facing slot N = code index N-1, so "slot 5" = code
> index 4 (the 4th bonus slot). Each board's **Slot 4** entry below is the one the
> slot-5 bug task maps to; see the per-slot dispatch arm `[board 4]`.

---

# Board 1 — Shield of Gilgamesh

## 1. Board 1 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** When you surround a city with Raiders, place a temple in it
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[1 :deployed]` (gated by `has-passive?`).
- **Trigger:** `:deployed` — fires after a deploy that surrounds a city.
- **Code path / outcome:** If context :surrounded-city is set and player lacks a temple there with supply>0: place a face-up temple, dec temples-supply. Else no-op.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 2. Board 1 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Travel to Kish
- **Entry point:** auto-resolve `apply-bonus-dispatch [1 1]` (no choice descriptor).
- **Code path / outcome:** Set :caravan to :kish (travel to Kish).
- **Honesty:** `:implemented`.

## 3. Board 1 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase Raider and Leader
- **Entry point:** auto-resolve `apply-bonus-dispatch [1 2]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :raider, then :leader (pays threshold cost each).
- **Honesty:** `:implemented`.

## 4. Board 1 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Place two raiders near Lagash
- **Entry point:** auto-resolve `apply-bonus-dispatch [1 3]` (no choice descriptor).
- **Code path / outcome:** Collect routes touching :lagash, drop occupied ones, place up to 2 raiding raiders (place-raider-on).
- **Honesty:** `:implemented`.

## 5. Board 1 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Glory per demand fulfilled
- **Entry point:** auto-resolve `apply-bonus-dispatch [1 4]` (no choice descriptor).
- **Code path / outcome:** +glory = count of held :demand-tokens.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 2 — Seal of Enmerkar

## 6. Board 2 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** When you score a Raider, increase Priest
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[2 :raider-scored]` (gated by `has-passive?`).
- **Trigger:** `:raider-scored` — fires when a raider is scored.
- **Code path / outcome:** Increase :priest one level if <5 and the threshold cost (role-threshold-costs) is affordable; pays the cost. Else no-op.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 7. Board 2 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Merchant and Raider
- **Entry point:** auto-resolve `apply-bonus-dispatch [2 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :merchant, then :raider.
- **Honesty:** `:implemented`.

## 8. Board 2 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** 5 Amity if at a magistrate city
- **Entry point:** auto-resolve `apply-bonus-dispatch [2 2]` (no choice descriptor).
- **Code path / outcome:** If magistrate-in-city? at player's :caravan: +5 amity. Else no-op.
- **Honesty:** `:implemented`.

## 9. Board 2 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Temple in a magistrate city
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [2 3]`; bot fallback `apply-bonus-dispatch [2 3]`.
- **Choice:** :pick-city — "Choose a magistrate city for your temple" (filter :magistrate)
- **Code path / outcome:** Pick a magistrate-city without your temple (else first magistrate-city); place-temple-in face-up. AUTO; human path = with-choice [2 3] pick-city :magistrate.
- **Honesty:** `:implemented`.

## 10. Board 2 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Glory per facedown temple
- **Entry point:** auto-resolve `apply-bonus-dispatch [2 4]` (no choice descriptor).
- **Code path / outcome:** +glory = count-face-down-temples.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 3 — Voyage of Ziusudra

## 11. Board 3 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** River travel -> take a Gem (and gems worth amity)
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[3 :river-crossed]` (gated by `has-passive?`).
- **Trigger:** `:river-crossed` — fires on river travel.
- **Code path / outcome:** Unconditionally +1 :gems (gems worth amity is a scoring-time rule, not applied here).
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 12. Board 3 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Leader (free)
- **Entry point:** auto-resolve `apply-bonus-dispatch [3 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-free :leader (no cost).
- **Honesty:** `:implemented`.

## 13. Board 3 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Temple in Lagash
- **Entry point:** auto-resolve `apply-bonus-dispatch [3 2]` (no choice descriptor).
- **Code path / outcome:** place-temple-in :lagash face-up.
- **Honesty:** `:implemented`.

## 14. Board 3 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Raider near Eridu + a good
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [3 3]`; bot fallback `apply-bonus-dispatch [3 3]`.
- **Choice:** :pick-resource — "Choose a resource to gain"
- **Code path / outcome:** Find a free route touching :eridu, place a raiding raider + 1 :tools; if none, just +1 :tools.
- **Honesty:** `:implemented`.

## 15. Board 3 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Travel then Sell
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [3 4]`; bot fallback `apply-bonus-dispatch [3 4]`.
- **Choice:** :pick-city — "Travel to adjacent city and sell" (filter :adjacent action :sell)
- **Code path / outcome:** Set :caravan :eridu, +2 amity (approximates the sell).
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 4 — Blessing of Inanna

## 16. Board 4 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** When you flip a temple, may sell in that city
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[4 :temple-flipped]` (gated by `has-passive?`).
- **Trigger:** `:temple-flipped` — fires when a temple is flipped.
- **Code path / outcome:** If context city has a demand matching a held resource: sell it (dec resource, drop demand, +demand-token, + merchant-score amity).
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 17. Board 4 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Temple in Eridu
- **Entry point:** auto-resolve `apply-bonus-dispatch [4 1]` (no choice descriptor).
- **Code path / outcome:** place-temple-in :eridu face-up.
- **Honesty:** `:implemented`.

## 18. Board 4 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Gain Tools, Gems, Gold
- **Entry point:** auto-resolve `apply-bonus-dispatch [4 2]` (no choice descriptor).
- **Code path / outcome:** +1 each of :tools, :gems, :gold.
- **Honesty:** `:implemented`.

## 19. Board 4 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Amity = Leader level x 2
- **Entry point:** auto-resolve `apply-bonus-dispatch [4 3]` (no choice descriptor).
- **Code path / outcome:** +amity = leader level x 2.
- **Honesty:** `:implemented`.

## 20. Board 4 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** 2 Amity per raider
- **Entry point:** auto-resolve `apply-bonus-dispatch [4 4]` (no choice descriptor).
- **Code path / outcome:** +amity = 2 x count-raiders-deployed.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 5 — Wisdom of Adapa

## 21. Board 5 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Influence magistrate in your city, travel with it
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[5 :magistrate-moved]` (gated by `has-passive?`).
- **Trigger:** `:magistrate-moved` — fires n/a.
- **Code path / outcome:** NO-OP / skip — comment: 'complex, would need movement tracking.' Passive does nothing.
- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.

## 22. Board 5 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Priest (free)
- **Entry point:** auto-resolve `apply-bonus-dispatch [5 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-free :priest.
- **Honesty:** `:implemented`.

## 23. Board 5 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Place demand tokens in Uruk + gain resources
- **Entry point:** auto-resolve `apply-bonus-dispatch [5 2]` (no choice descriptor).
- **Code path / outcome:** Draw up to 2 demand tokens from :demand-bag, push each onto :uruk demands AND grant matching resource.
- **Honesty:** `:implemented`.

## 24. Board 5 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Deploy then Temple
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [5 3]`; bot fallback `apply-bonus-dispatch [5 3]`.
- **Choice:** :pick-city — "Travel to adjacent city and deploy" (filter :adjacent action :deploy)
- **Code path / outcome:** Place a raiding raider on any free route, then place-temple-in at :caravan.
- **Honesty:** `:implemented`.

## 25. Board 5 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** 2 Amity per raider
- **Entry point:** auto-resolve `apply-bonus-dispatch [5 4]` (no choice descriptor).
- **Code path / outcome:** +amity = 2 x count-raiders-deployed.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 6 — Trade of Dumuzid

## 26. Board 6 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Action space 7 -> free Travel
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[6 :action-space-7]` (gated by `has-passive?`).
- **Trigger:** `:action-space-7` — fires landing on action space 7.
- **Code path / outcome:** Sets flag :pending-free-travel true; actual travel handled by choice.cljc.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 27. Board 6 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Merchant and Priest
- **Entry point:** auto-resolve `apply-bonus-dispatch [6 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :merchant, then :priest.
- **Honesty:** `:implemented`.

## 28. Board 6 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Temple in each magistrate city
- **Entry point:** auto-resolve `apply-bonus-dispatch [6 2]` (no choice descriptor).
- **Code path / outcome:** For each magistrate-city without your temple: place-temple-in face-up.
- **Honesty:** `:implemented`.

## 29. Board 6 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Sell to Babylon (double)
- **Entry point:** auto-resolve `apply-bonus-dispatch [6 3]` (no choice descriptor).
- **Code path / outcome:** Set :caravan :babylon, +4 amity (approximates the double sell).
- **Honesty:** `:implemented`.

## 30. Board 6 / Slot 4 (player-facing #4) — [PARTIAL]

- **Printed intent:** Raider near Lagash + Tools x2
- **Entry point:** auto-resolve `apply-bonus-dispatch [6 4]` (no choice descriptor).
- **Code path / outcome:** +2 :tools only. PARTIAL: 'Raider near Lagash' part is NOT placed.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 7 — March of Lugalbanda

## 31. Board 7 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Place raiders, extra one next to magistrate
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[7 :deployed]` (gated by `has-passive?`).
- **Trigger:** `:deployed` — fires after a deploy.
- **Code path / outcome:** If raider supply available and below raider-max-deployed: find a route adjacent to a magistrate-city with no raider, place a raiding raider, dec supply. Uses magistrate-cities, routes-from-city.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 32. Board 7 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Merchant and Leader
- **Entry point:** auto-resolve `apply-bonus-dispatch [7 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :merchant, then :leader.
- **Honesty:** `:implemented`.

## 33. Board 7 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Temple in a magistrate city
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [7 2]`; bot fallback `apply-bonus-dispatch [7 2]`.
- **Choice:** :pick-city — "Choose a magistrate city for your temple" (filter :magistrate)
- **Code path / outcome:** Pick magistrate-city without your temple; place-temple-in face-up. AUTO; human path = with-choice [7 2].
- **Honesty:** `:implemented`.

## 34. Board 7 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Travel + 3 Glory if at Eridu
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [7 3]`; bot fallback `apply-bonus-dispatch [7 3]`.
- **Choice:** :pick-city — "Choose a city to travel to" (filter :adjacent)
- **Code path / outcome:** Set :caravan :eridu, +3 glory (assumes the 'if at Eridu' condition by travelling there first).
- **Honesty:** `:implemented`.

## 35. Board 7 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Travel + 3 Amity if at Kish
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [7 4]`; bot fallback `apply-bonus-dispatch [7 4]`.
- **Choice:** :pick-city — "Choose a city to travel to" (filter :adjacent)
- **Code path / outcome:** Set :caravan :kish, +3 amity (travels to Kish first).
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 8 — Fury of Enkidu

## 36. Board 8 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Score raider -> flip to active instead of removing
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[8 :raider-scored]` (gated by `has-passive?`).
- **Trigger:** `:raider-scored` — fires when a raider is scored.
- **Code path / outcome:** Sets flag :keep-scored-raider true so score-own-raider-on-route flips to :raiding instead of removing.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 37. Board 8 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Raider and Priest
- **Entry point:** auto-resolve `apply-bonus-dispatch [8 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :raider, then :priest.
- **Honesty:** `:implemented`.

## 38. Board 8 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Place demand + sell
- **Entry point:** auto-resolve `apply-bonus-dispatch [8 2]` (no choice descriptor).
- **Code path / outcome:** +3 amity (approximates place-demand-then-sell).
- **Honesty:** `:implemented`.

## 39. Board 8 / Slot 3 (player-facing #3) — [PARTIAL]

- **Printed intent:** Gain Gold, Gems, Pottery (then sell)
- **Entry point:** auto-resolve `apply-bonus-dispatch [8 3]` (no choice descriptor).
- **Code path / outcome:** +1 each :gold, :gems, :pottery. PARTIAL: no sell step (status :partial).
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 40. Board 8 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Flip all raiders to point
- **Entry point:** auto-resolve `apply-bonus-dispatch [8 4]` (no choice descriptor).
- **Code path / outcome:** Set every held raider to :point.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 9 — Rites of Ninhursag

## 41. Board 9 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Flip temple -> may increase a role
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[9 :temple-flipped]` (gated by `has-passive?`).
- **Trigger:** `:temple-flipped` — fires when a temple is flipped.
- **Code path / outcome:** If any role is upgradeable AND affordable: stash :passive-choice-needed {:pick-role}. Resolved in apply-passive-choice (board 9) which pays cost + increases.
- **Honesty:** `:persistent`; uses a deferred choice resolved in `apply-passive-choice` (board 9).

## 42. Board 9 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Gain Tools, Gold, Pottery + Amity = leader level
- **Entry point:** auto-resolve `apply-bonus-dispatch [9 1]` (no choice descriptor).
- **Code path / outcome:** +1 each :tools, :gold, :pottery; +amity = leader level.
- **Honesty:** `:implemented`.

## 43. Board 9 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase Priest and Leader
- **Entry point:** auto-resolve `apply-bonus-dispatch [9 2]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :priest, then :leader.
- **Honesty:** `:implemented`.

## 44. Board 9 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Raider on each river
- **Entry point:** auto-resolve `apply-bonus-dispatch [9 3]` (no choice descriptor).
- **Code path / outcome:** Place raiding raiders on up to 3 free river routes (:type :river).
- **Honesty:** `:implemented`.

## 45. Board 9 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Sell to magistrate city + temple
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [9 4]`; bot fallback `apply-bonus-dispatch [9 4]`.
- **Choice:** :pick-city — "Choose a magistrate city to sell in" (filter :magistrate)
- **Code path / outcome:** Pick magistrate-city without your temple; set :caravan there, +2 amity, place-temple-in. AUTO; human path = with-choice [9 4] sell-in-magistrate.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 10 — Wealth of Meskalamdug

## 46. Board 10 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Sell gold to empty demand cities
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[10 :sold]` (gated by `has-passive?`).
- **Trigger:** `:sold` — fires on a sell.
- **Code path / outcome:** NO-OP / TODO: 'requires sell phase modification.' Passive does nothing yet.
- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.

## 47. Board 10 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Merchant (free)
- **Entry point:** auto-resolve `apply-bonus-dispatch [10 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-free :merchant.
- **Honesty:** `:implemented`.

## 48. Board 10 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase Merchant (free)
- **Entry point:** auto-resolve `apply-bonus-dispatch [10 2]` (no choice descriptor).
- **Code path / outcome:** increase-role-free :merchant.
- **Honesty:** `:implemented`.

## 49. Board 10 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Raider near magistrate + amity
- **Entry point:** auto-resolve `apply-bonus-dispatch [10 3]` (no choice descriptor).
- **Code path / outcome:** Place a raiding raider on a free route touching a magistrate-city, +2 amity; if none, just +2 amity.
- **Honesty:** `:implemented`.

## 50. Board 10 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Temple in Nippur
- **Entry point:** auto-resolve `apply-bonus-dispatch [10 4]` (no choice descriptor).
- **Code path / outcome:** place-temple-in :nippur face-up.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 11 — Ambition of Sargon

## 51. Board 11 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Extra glory on contest claims
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[11 :feat-claimed]` (gated by `has-passive?`).
- **Trigger:** `:feat-claimed` — fires when a feat/contest is claimed.
- **Code path / outcome:** +glory equal to current :leader level.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 52. Board 11 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Place demand tokens in Lagash
- **Entry point:** auto-resolve `apply-bonus-dispatch [11 1]` (no choice descriptor).
- **Code path / outcome:** Set :caravan :lagash, +1 :gold +1 :pottery (approximates placing demand tokens).
- **Honesty:** `:implemented`.

## 53. Board 11 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Sell to Lagash (double glory)
- **Entry point:** auto-resolve `apply-bonus-dispatch [11 2]` (no choice descriptor).
- **Code path / outcome:** Set :caravan :lagash, +4 glory (double-glory sell approximation).
- **Honesty:** `:implemented`.

## 54. Board 11 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Increase Raider (free)
- **Entry point:** auto-resolve `apply-bonus-dispatch [11 3]` (no choice descriptor).
- **Code path / outcome:** increase-role-free :raider.
- **Honesty:** `:implemented`.

## 55. Board 11 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Glory per facedown temple
- **Entry point:** auto-resolve `apply-bonus-dispatch [11 4]` (no choice descriptor).
- **Code path / outcome:** +glory = count-face-down-temples.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 12 — Currents of Enki

## 56. Board 12 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** River crossing -> place raider on that river
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[12 :river-crossed]` (gated by `has-passive?`).
- **Trigger:** `:river-crossed` — fires when crossing a river.
- **Code path / outcome:** If context :route given, supply available, below max, no raider there: place a raiding raider on that route, dec supply.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 57. Board 12 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase all level-1 roles
- **Entry point:** auto-resolve `apply-bonus-dispatch [12 1]` (no choice descriptor).
- **Code path / outcome:** For each role currently at level 1: set to 2 (free).
- **Honesty:** `:implemented`.

## 58. Board 12 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Gain Gold x3 + Gems
- **Entry point:** auto-resolve `apply-bonus-dispatch [12 2]` (no choice descriptor).
- **Code path / outcome:** +3 :gold, +1 :gems.
- **Honesty:** `:implemented`.

## 59. Board 12 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Increase merchant + sell for glory
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [12 3]`; bot fallback `apply-bonus-dispatch [12 3]`.
- **Choice:** :pick-city — "Choose a magistrate city to sell in" (filter :magistrate)
- **Code path / outcome:** increase-role-with-cost :merchant, +3 glory (sell-for-glory approximation).
- **Honesty:** `:implemented`.

## 60. Board 12 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Glory per facedown temple
- **Entry point:** auto-resolve `apply-bonus-dispatch [12 4]` (no choice descriptor).
- **Code path / outcome:** +glory = count-face-down-temples.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 13 — Pillars of Etana

## 61. Board 13 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Temple placement -> raider adjacent
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[13 :temple-placed]` (gated by `has-passive?`).
- **Trigger:** `:temple-placed` — fires after placing a temple.
- **Code path / outcome:** Find a route adjacent to context city with no raider; if supply + below max: place a raiding raider, dec supply. Uses routes-from-city.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 62. Board 13 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Gain Tools x3 + Glory = leader level
- **Entry point:** auto-resolve `apply-bonus-dispatch [13 1]` (no choice descriptor).
- **Code path / outcome:** +3 :tools; +glory = leader level.
- **Honesty:** `:implemented`.

## 63. Board 13 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Gain Pottery x3 + Glory = leader level
- **Entry point:** auto-resolve `apply-bonus-dispatch [13 2]` (no choice descriptor).
- **Code path / outcome:** +3 :pottery; +glory = leader level.
- **Honesty:** `:implemented`.

## 64. Board 13 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Increase all level-3 roles
- **Entry point:** auto-resolve `apply-bonus-dispatch [13 3]` (no choice descriptor).
- **Code path / outcome:** For each role currently at level 3: increase-role-with-cost.
- **Honesty:** `:implemented`.

## 65. Board 13 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Temple adjacent to one of your raiders
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [13 4]`; bot fallback `apply-bonus-dispatch [13 4]`.
- **Choice:** :pick-city — "Place a temple adjacent to one of your raiders" (filter :adjacent-to-raider)
- **Code path / outcome:** AUTO: place-temple-in the first city adjacent to any held raider. Human path = with-choice [13 4] pick-city :adjacent-to-raider. Fires :temple-placed passive.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 14 — Roads of Shulgi

## 66. Board 14 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Uruk travel bonus action
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[14 :turn-start]` (gated by `has-passive?`).
- **Trigger:** `:turn-start` — fires start of turn.
- **Code path / outcome:** NO-OP / skip — 'complex bonus action'. Passive does nothing.
- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.

## 67. Board 14 / Slot 1 (player-facing #1) — [PARTIAL]

- **Printed intent:** Glory per raider (place raider first)
- **Entry point:** auto-resolve `apply-bonus-dispatch [14 1]` (no choice descriptor).
- **Code path / outcome:** Place a raiding raider on a free route touching :lagash, then +glory = count-raiders-deployed. PARTIAL.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 68. Board 14 / Slot 2 (player-facing #2) — [PARTIAL]

- **Printed intent:** Resources + move magistrate
- **Entry point:** auto-resolve `apply-bonus-dispatch [14 2]` (no choice descriptor).
- **Code path / outcome:** +1 :tools, +1 :pottery. PARTIAL: no magistrate move.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 69. Board 14 / Slot 3 (player-facing #3) — [PARTIAL]

- **Printed intent:** Place demands in Eridu + travel to Eridu
- **Entry point:** auto-resolve `apply-bonus-dispatch [14 3]` (no choice descriptor).
- **Code path / outcome:** Draw up to 2 demand tokens onto :eridu demands, set :caravan :eridu. PARTIAL (status).
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 70. Board 14 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Temple in Babylon
- **Entry point:** auto-resolve `apply-bonus-dispatch [14 4]` (no choice descriptor).
- **Code path / outcome:** place-temple-in :babylon face-up.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 15 — Ascent of Ur-Nammu

## 71. Board 15 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Free role increases (ignore threshold cost)
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[15 :role-increased]` (gated by `has-passive?`).
- **Trigger:** `:role-increased` — fires when increasing a role.
- **Code path / outcome:** Sets flag :free-role-increase true so choose-role-increase-choices ignores threshold cost.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 72. Board 15 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Good per demand fulfilled
- **Entry point:** auto-resolve `apply-bonus-dispatch [15 1]` (no choice descriptor).
- **Code path / outcome:** For each held demand-token: +1 of that resource.
- **Honesty:** `:implemented`.

## 73. Board 15 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase Priest + 4 Glory if Babylon facedown temple
- **Entry point:** auto-resolve `apply-bonus-dispatch [15 2]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :priest; if :babylon temple is :face-down, +4 glory.
- **Honesty:** `:implemented`.

## 74. Board 15 / Slot 3 (player-facing #3) — [PARTIAL]

- **Printed intent:** Increase lowest role + travel
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-role}` -> human path `apply-bonus-with-choice [15 3]`; bot fallback `apply-bonus-dispatch [15 3]`.
- **Choice:** :pick-role — "Choose a role to increase"
- **Code path / outcome:** increase-role-free on the lowest-level role. PARTIAL: no travel.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 75. Board 15 / Slot 4 (player-facing #4) — [PARTIAL]

- **Printed intent:** 3 Amity per raider adjacent to a magistrate
- **Entry point:** auto-resolve `apply-bonus-dispatch [15 4]` (no choice descriptor).
- **Code path / outcome:** +amity = 3 x (raiders on routes touching a magistrate-city). PARTIAL: status flags adjacency caveat.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 16 — Dominion of Hammurabi

## 76. Board 16 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** 2-astronomer space -> third action
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[16 :landing]` (gated by `has-passive?`).
- **Trigger:** `:landing` — fires on landing.
- **Code path / outcome:** If context :astronomer-count == 2: set flag :bonus-extra-action true (third action).
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 77. Board 16 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Pottery per temple
- **Entry point:** auto-resolve `apply-bonus-dispatch [16 1]` (no choice descriptor).
- **Code path / outcome:** +pottery = count-temples-placed.
- **Honesty:** `:implemented`.

## 78. Board 16 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Deploy + amity per raider
- **Entry point:** auto-resolve `apply-bonus-dispatch [16 2]` (no choice descriptor).
- **Code path / outcome:** Place a raiding raider on any free route, then +amity = 2 x count-raiders-deployed.
- **Honesty:** `:implemented`.

## 79. Board 16 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Increase Leader twice
- **Entry point:** auto-resolve `apply-bonus-dispatch [16 3]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :leader twice.
- **Honesty:** `:implemented`.

## 80. Board 16 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Place demands + sell
- **Entry point:** auto-resolve `apply-bonus-dispatch [16 4]` (no choice descriptor).
- **Code path / outcome:** +1 :tools, +3 amity (place-demands-then-sell approximation).
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 17 — Cunning of Kubaba

## 81. Board 17 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Action space 7 -> take a good of choice
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[17 :action-space-7]` (gated by `has-passive?`).
- **Trigger:** `:action-space-7` — fires landing on action space 7.
- **Code path / outcome:** Stash :passive-choice-needed {:pick-resource}. Resolved in apply-passive-choice (board 17): +1 chosen resource.
- **Honesty:** `:persistent`; uses a deferred choice resolved in `apply-passive-choice` (board 17).

## 82. Board 17 / Slot 1 (player-facing #1) — [PARTIAL]

- **Printed intent:** Flip one raider to point (after placement)
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [17 1]`; bot fallback `apply-bonus-dispatch [17 1]`.
- **Choice:** :pick-resource — "Choose a resource to gain"
- **Code path / outcome:** Flip the first :raiding raider to :point. PARTIAL: no placement step. Human path = with-choice [17 1] pick-resource (note: choice arm grants a resource, see divergence).
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 83. Board 17 / Slot 2 (player-facing #2) — [PARTIAL]

- **Printed intent:** Facedown temple in magistrate city
- **Entry point:** auto-resolve `apply-bonus-dispatch [17 2]` (no choice descriptor).
- **Code path / outcome:** For each magistrate-city with temple supply: place a :face-down temple, dec supply. PARTIAL.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 84. Board 17 / Slot 3 (player-facing #3) — [PARTIAL]

- **Printed intent:** 4 Amity if you surround Uruk
- **Entry point:** auto-resolve `apply-bonus-dispatch [17 3]` (no choice descriptor).
- **Code path / outcome:** If every route adjacent to :uruk holds your raider: +8 amity. Else no-op. PARTIAL.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 85. Board 17 / Slot 4 (player-facing #4) — [PARTIAL]

- **Printed intent:** Glory = merchant level (then sell)
- **Entry point:** auto-resolve `apply-bonus-dispatch [17 4]` (no choice descriptor).
- **Code path / outcome:** +glory = merchant level. PARTIAL: no sell.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 18 — Forge of Tubal-Cain

## 86. Board 18 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Keep tools when spent + tools worth glory at end
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[18 :end-game / :resource-spent]` (gated by `has-passive?`).
- **Trigger:** `:end-game / :resource-spent` — fires end of game; when a resource is spent.
- **Code path / outcome:** end-game: +glory equal to current :tools held. resource-spent: if :tools spent, refund +1 :tools (tools never consumed).
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 87. Board 18 / Slot 1 (player-facing #1) — [PARTIAL]

- **Printed intent:** Resources + move magistrate / sell
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [18 1]`; bot fallback `apply-bonus-dispatch [18 1]`.
- **Choice:** :pick-city — "Move magistrate across a river" (filter :magistrate)
- **Code path / outcome:** +2 :tools. PARTIAL: no magistrate move / sell. Human path = with-choice [18 1] move-magistrate.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 88. Board 18 / Slot 2 (player-facing #2) — [PARTIAL]

- **Printed intent:** 5 Glory if facedown temple in Samarra
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [18 2]`; bot fallback `apply-bonus-dispatch [18 2]`.
- **Choice:** :pick-city — "Choose a city to travel to" (filter :adjacent)
- **Code path / outcome:** If :samarra temple is :face-down: +5 glory; else +2 glory (partial fallback).
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 89. Board 18 / Slot 3 (player-facing #3) — [PARTIAL]

- **Printed intent:** 3 Amity if you surround Kish
- **Entry point:** auto-resolve `apply-bonus-dispatch [18 3]` (no choice descriptor).
- **Code path / outcome:** If every route adjacent to :kish holds your raider: +6 amity. Else no-op.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 90. Board 18 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** 4 Amity per point raider, then remove
- **Entry point:** auto-resolve `apply-bonus-dispatch [18 4]` (no choice descriptor).
- **Code path / outcome:** +amity = 4 x (count of :point raiders); then remove those raiders and refund supply (QA lesson 8 'score then remove').
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 19 — Kilns of Ninkasi

## 91. Board 19 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Take pottery -> extra pottery x2
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[19 :goods-taken]` (gated by `has-passive?`).
- **Trigger:** `:goods-taken` — fires when taking goods.
- **Code path / outcome:** If context :resources contains :pottery: +2 :pottery.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 92. Board 19 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Priest twice
- **Entry point:** auto-resolve `apply-bonus-dispatch [19 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :priest twice.
- **Honesty:** `:implemented`.

## 93. Board 19 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Sell to pottery cities
- **Entry point:** auto-resolve `apply-bonus-dispatch [19 2]` (no choice descriptor).
- **Code path / outcome:** +1 :pottery, +3 amity (sell-to-pottery-cities approximation).
- **Honesty:** `:implemented`.

## 94. Board 19 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Discard good + move magistrate + sell
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [19 3]`; bot fallback `apply-bonus-dispatch [19 3]`.
- **Choice:** :pick-resource — "Choose a resource to discard (move magistrate + sell)"
- **Code path / outcome:** Discard first held good, +3 glory. Human path = with-choice [19 3] pick-resource-to-discard then influence+sell at caravan.
- **Honesty:** `:implemented`.

## 95. Board 19 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Flip all raiders to point
- **Entry point:** auto-resolve `apply-bonus-dispatch [19 4]` (no choice descriptor).
- **Code path / outcome:** Set every held raider to :point.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 20 — Vision of Rimush

## 96. Board 20 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Flip temple -> discard pottery for 3 glory
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[20 :temple-flipped]` (gated by `has-passive?`).
- **Trigger:** `:temple-flipped` — fires when a temple is flipped.
- **Code path / outcome:** If pottery>0: stash :passive-choice-needed {:yes-no}. apply-passive-choice (board 20): yes -> dec pottery, +3 glory.
- **Honesty:** `:persistent`; uses a deferred choice resolved in `apply-passive-choice` (board 20).

## 97. Board 20 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Raider on each opposing route
- **Entry point:** auto-resolve `apply-bonus-dispatch [20 1]` (no choice descriptor).
- **Code path / outcome:** Place raiding raiders on up to 2 free routes from current :caravan city.
- **Honesty:** `:implemented`.

## 98. Board 20 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase Merchant twice
- **Entry point:** auto-resolve `apply-bonus-dispatch [20 2]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :merchant twice.
- **Honesty:** `:implemented`.

## 99. Board 20 / Slot 3 (player-facing #3) — [PARTIAL]

- **Printed intent:** Influence + Amity = leader level
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [20 3]`; bot fallback `apply-bonus-dispatch [20 3]`.
- **Choice:** :pick-city — "Choose magistrate destination" (filter :magistrate)
- **Code path / outcome:** +amity = leader level. PARTIAL: no influence step. Human path = with-choice [20 3] influence+sell.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 100. Board 20 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Take goods from astronomer spaces
- **Entry point:** auto-resolve `apply-bonus-dispatch [20 4]` (no choice descriptor).
- **Code path / outcome:** +1 :tools, +1 :gold (take-from-astronomer-spaces approximation).
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 21 — Legacy of Eannatum

## 101. Board 21 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Temple placement -> extra facedown in same city
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[21 :temple-placed]` (gated by `has-passive?`).
- **Trigger:** `:temple-placed` — fires after placing a temple.
- **Code path / outcome:** NO-OP — data model keys temples by city, can't hold two in one city. Passive does nothing.
- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.

## 102. Board 21 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Travel to Eridu (anywhere from Eridu)
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [21 1]`; bot fallback `apply-bonus-dispatch [21 1]`.
- **Choice:** :pick-city — "Travel anywhere (from Eridu)" (filter :any)
- **Code path / outcome:** Set :caravan :eridu. Human path = with-choice [21 1] travel anywhere.
- **Honesty:** `:implemented`.

## 103. Board 21 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase Raider and Leader
- **Entry point:** auto-resolve `apply-bonus-dispatch [21 2]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :raider, then :leader.
- **Honesty:** `:implemented`.

## 104. Board 21 / Slot 3 (player-facing #3) — [PARTIAL]

- **Printed intent:** Travel to Eridu + sell
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [21 3]`; bot fallback `apply-bonus-dispatch [21 3]`.
- **Choice:** :pick-city — "Travel to adjacent city and sell" (filter :adjacent action :sell)
- **Code path / outcome:** Set :caravan :eridu. PARTIAL: no sell. Human path = with-choice [21 3] travel+sell.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 105. Board 21 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Glory per demand fulfilled
- **Entry point:** auto-resolve `apply-bonus-dispatch [21 4]` (no choice descriptor).
- **Code path / outcome:** +glory = count of held demand-tokens.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 22 — Strategy of Naram-Sin

## 106. Board 22 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Action space 7 -> same action twice
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[22 :action-space-7]` (gated by `has-passive?`).
- **Trigger:** `:action-space-7` — fires landing on action space 7.
- **Code path / outcome:** Sets flag :bonus-repeat-action true (same action twice).
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 107. Board 22 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Raider and Merchant
- **Entry point:** auto-resolve `apply-bonus-dispatch [22 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :raider, then :merchant.
- **Honesty:** `:implemented`.

## 108. Board 22 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Demands on facedown temples
- **Entry point:** auto-resolve `apply-bonus-dispatch [22 2]` (no choice descriptor).
- **Code path / outcome:** +amity = count-face-down-temples (demands-on-facedown approximation).
- **Honesty:** `:implemented`.

## 109. Board 22 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Good + travel
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [22 3]`; bot fallback `apply-bonus-dispatch [22 3]`.
- **Choice:** :pick-resource — "Choose a resource to gain"
- **Code path / outcome:** +1 :pottery. Human path = with-choice [22 3] pick-resource.
- **Honesty:** `:implemented`.

## 110. Board 22 / Slot 4 (player-facing #4) — [PARTIAL]

- **Printed intent:** 2 Amity per raider + travel
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [22 4]`; bot fallback `apply-bonus-dispatch [22 4]`.
- **Choice:** :pick-city — "Choose a city to travel to" (filter :adjacent)
- **Code path / outcome:** +amity = 2 x count-raiders-deployed. PARTIAL: no travel. Human path = with-choice [22 4] travel.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 23 — Market of Puabi

## 111. Board 23 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Sell -> glory instead of amity
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[23 :sold]` (gated by `has-passive?`).
- **Trigger:** `:sold` — fires on a sell.
- **Code path / outcome:** If context :amity-scored > 0: move that amount from :amity to :glory (glory instead of amity).
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 112. Board 23 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Priest and Merchant
- **Entry point:** auto-resolve `apply-bonus-dispatch [23 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :priest, then :merchant.
- **Honesty:** `:implemented`.

## 113. Board 23 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Sell twice to Eridu
- **Entry point:** auto-resolve `apply-bonus-dispatch [23 2]` (no choice descriptor).
- **Code path / outcome:** Set :caravan :eridu, +4 amity (sell twice approximation).
- **Honesty:** `:implemented`.

## 114. Board 23 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Good + travel + increase merchant
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [23 3]`; bot fallback `apply-bonus-dispatch [23 3]`.
- **Choice:** :pick-resource — "Choose a resource to gain"
- **Code path / outcome:** +1 :tools, increase-role-with-cost :merchant. Human path = with-choice [23 3] pick-resource.
- **Honesty:** `:implemented`.

## 115. Board 23 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Temple in a magistrate city
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [23 4]`; bot fallback `apply-bonus-dispatch [23 4]`.
- **Choice:** :pick-city — "Choose a magistrate city for your temple" (filter :magistrate)
- **Code path / outcome:** Pick magistrate-city without your temple; place-temple-in. AUTO; human path = with-choice [23 4].
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 24 — Siege of Shulme

## 116. Board 24 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Surround city -> sell there
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[24 :deployed]` (gated by `has-passive?`).
- **Trigger:** `:deployed` — fires after a deploy.
- **Code path / outcome:** NO-OP / skip — 'would need sell logic outside normal sell phase.' Passive does nothing.
- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.

## 117. Board 24 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Raider and Leader
- **Entry point:** auto-resolve `apply-bonus-dispatch [24 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :raider, then :leader.
- **Honesty:** `:implemented`.

## 118. Board 24 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Demands on magistrates
- **Entry point:** auto-resolve `apply-bonus-dispatch [24 2]` (no choice descriptor).
- **Code path / outcome:** +2 glory (demands-on-magistrates approximation).
- **Honesty:** `:implemented`.

## 119. Board 24 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Glory per demand fulfilled
- **Entry point:** auto-resolve `apply-bonus-dispatch [24 3]` (no choice descriptor).
- **Code path / outcome:** +glory = count of held demand-tokens.
- **Honesty:** `:implemented`.

## 120. Board 24 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Goods per demand at magistrates
- **Entry point:** auto-resolve `apply-bonus-dispatch [24 4]` (no choice descriptor).
- **Code path / outcome:** For up to 2 held demand-tokens: +1 of each matching resource.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 25 — Command of Mesannepada

## 121. Board 25 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Two raiders per path
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[25 :deployed]` (gated by `has-passive?`).
- **Trigger:** `:deployed` — fires on a deploy.
- **Code path / outcome:** Sets flag :allow-double-raiders true so resolve-deploy-choices allows placing on occupied routes.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 122. Board 25 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Influence + score raiders
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [25 1]`; bot fallback `apply-bonus-dispatch [25 1]`.
- **Choice:** :pick-city — "Choose magistrate destination" (filter :magistrate)
- **Code path / outcome:** +glory = 2 + count of :point raiders (influence+score approximation). Human path = with-choice [25 1] influence.
- **Honesty:** `:implemented`.

## 123. Board 25 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase Merchant and Leader
- **Entry point:** auto-resolve `apply-bonus-dispatch [25 2]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :merchant, then :leader.
- **Honesty:** `:implemented`.

## 124. Board 25 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Two facedown temples
- **Entry point:** auto-resolve `apply-bonus-dispatch [25 3]` (no choice descriptor).
- **Code path / outcome:** Flip up to 2 :face-up temples to :face-down, +1 amity each.
- **Honesty:** `:implemented`.

## 125. Board 25 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Good + travel
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [25 4]`; bot fallback `apply-bonus-dispatch [25 4]`.
- **Choice:** :pick-resource — "Choose a resource to gain"
- **Code path / outcome:** +1 :gems. Human path = with-choice [25 4] pick-resource.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 26 — Court of Enshakushanna

## 126. Board 26 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Extra 2 amity on magistrate bonus
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[26 :sold]` (gated by `has-passive?`).
- **Trigger:** `:sold` — fires on a sell.
- **Code path / outcome:** If context :glory-scored > 0 (i.e. magistrate bonus fired): +2 :amity.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 127. Board 26 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase Priest and Leader
- **Entry point:** auto-resolve `apply-bonus-dispatch [26 1]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :priest, then :leader.
- **Honesty:** `:implemented`.

## 128. Board 26 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase Priest and Raider
- **Entry point:** auto-resolve `apply-bonus-dispatch [26 2]` (no choice descriptor).
- **Code path / outcome:** increase-role-with-cost :priest, then :raider.
- **Honesty:** `:implemented`.

## 129. Board 26 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Sell + temple
- **Entry point:** auto-resolve `apply-bonus-dispatch [26 3]` (no choice descriptor).
- **Code path / outcome:** +2 amity, place-temple-in at :caravan.
- **Honesty:** `:implemented`.

## 130. Board 26 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Raider + surround check
- **Entry point:** auto-resolve `apply-bonus-dispatch [26 4]` (no choice descriptor).
- **Code path / outcome:** Place a raiding raider on any free route, +2 amity.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 27 — Path of Alulim

## 131. Board 27 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Role increase -> another role for double cost
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[27 :role-increased]` (gated by `has-passive?`).
- **Trigger:** `:role-increased` — fires when increasing a role.
- **Code path / outcome:** If a DIFFERENT role is upgradeable and affordable at DOUBLE cost: stash :passive-choice-needed {:pick-role, double-cost}. apply-passive-choice (board 27) pays 2x cost + increases.
- **Honesty:** `:persistent`; uses a deferred choice resolved in `apply-passive-choice` (board 27).

## 132. Board 27 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Travel + sell
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [27 1]`; bot fallback `apply-bonus-dispatch [27 1]`.
- **Choice:** :pick-city — "Travel to adjacent city and sell" (filter :adjacent action :sell)
- **Code path / outcome:** +3 amity (travel+sell approximation). Human path = with-choice [27 1] travel+sell.
- **Honesty:** `:implemented`.

## 133. Board 27 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Travel + deploy
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [27 2]`; bot fallback `apply-bonus-dispatch [27 2]`.
- **Choice:** :pick-city — "Travel to adjacent city and deploy" (filter :adjacent action :deploy)
- **Code path / outcome:** Place a raiding raider on any free route. Human path = with-choice [27 2] travel+deploy.
- **Honesty:** `:implemented`.

## 134. Board 27 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Travel + temple
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [27 3]`; bot fallback `apply-bonus-dispatch [27 3]`.
- **Choice:** :pick-city — "Travel to adjacent city and place a temple" (filter :adjacent action :temple)
- **Code path / outcome:** place-temple-in at :caravan. Human path = with-choice [27 3] travel+temple.
- **Honesty:** `:implemented`.

## 135. Board 27 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Three goods of choice
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [27 4]`; bot fallback `apply-bonus-dispatch [27 4]`.
- **Choice:** :pick-resource — "Choose a resource to gain (1 of 3)" (count 3)
- **Code path / outcome:** +1 each :tools, :gold, :gems (three goods). Human path = with-choice [27 4] pick-resource x3.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 28 — Stars of Sin-Kashid

## 136. Board 28 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** 4+ astronomers -> role increase at turn end
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[28 :landing]` (gated by `has-passive?`).
- **Trigger:** `:landing` — fires on landing.
- **Code path / outcome:** If context :astronomer-count >= 4: set flag :bonus-role-increase true (role increase at turn end).
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 137. Board 28 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Travel + temple
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [28 1]`; bot fallback `apply-bonus-dispatch [28 1]`.
- **Choice:** :pick-city — "Travel to adjacent city and place a temple" (filter :adjacent action :temple)
- **Code path / outcome:** place-temple-in at :caravan. Human path = with-choice [28 1] travel+temple.
- **Honesty:** `:implemented`.

## 138. Board 28 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Travel + temple
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [28 2]`; bot fallback `apply-bonus-dispatch [28 2]`.
- **Choice:** :pick-city — "Travel to adjacent city and place a temple" (filter :adjacent action :temple)
- **Code path / outcome:** place-temple-in at :caravan. Human path = with-choice [28 2] travel+temple.
- **Honesty:** `:implemented`.

## 139. Board 28 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Sell gold to empty city
- **Entry point:** auto-resolve `apply-bonus-dispatch [28 3]` (no choice descriptor).
- **Code path / outcome:** If gold>0 dec gold; +4 amity (sell-gold-to-empty-city approximation).
- **Honesty:** `:implemented`.

## 140. Board 28 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Raider point-side near Kish
- **Entry point:** auto-resolve `apply-bonus-dispatch [28 4]` (no choice descriptor).
- **Code path / outcome:** Place a raiding raider on a free route touching :kish, then flip it to :point.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 29 — Treasury of Ibbi-Sin

## 141. Board 29 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Pay gold -> 2 amity
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[29 :resource-spent]` (gated by `has-passive?`).
- **Trigger:** `:resource-spent` — fires when a resource is spent.
- **Code path / outcome:** If :gold spent: +2 :amity.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 142. Board 29 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Decrease leader + increase others
- **Entry point:** auto-resolve `apply-bonus-dispatch [29 1]` (no choice descriptor).
- **Code path / outcome:** If leader>1: dec :leader, increase-role-free :merchant and :priest. Else no-op.
- **Honesty:** `:implemented`.

## 143. Board 29 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Travel + sell
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [29 2]`; bot fallback `apply-bonus-dispatch [29 2]`.
- **Choice:** :pick-city — "Travel to adjacent city and sell" (filter :adjacent action :sell)
- **Code path / outcome:** +3 amity (travel+sell approximation). Human path = with-choice [29 2] travel+sell.
- **Honesty:** `:implemented`.

## 144. Board 29 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Raider on each river
- **Entry point:** auto-resolve `apply-bonus-dispatch [29 3]` (no choice descriptor).
- **Code path / outcome:** Place raiding raiders on up to 3 free river routes.
- **Honesty:** `:implemented`.

## 145. Board 29 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Temple in surrounded cities
- **Entry point:** auto-resolve `apply-bonus-dispatch [29 4]` (no choice descriptor).
- **Code path / outcome:** place-temple-in at :caravan (surrounded-cities approximation).
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 30 — Council of Amar-Sin

## 146. Board 30 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Take goods from other astronomer location
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[30 :goods-taken]` (gated by `has-passive?`).
- **Trigger:** `:goods-taken` — fires when taking goods.
- **Code path / outcome:** NO-OP / skip — 'complex'. Passive does nothing.
- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.

## 147. Board 30 / Slot 1 (player-facing #1) — [PARTIAL]

- **Printed intent:** Influence + travel; Glory = leader level
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [30 1]`; bot fallback `apply-bonus-dispatch [30 1]`.
- **Choice:** :pick-city — "Choose a city to travel to" (filter :adjacent)
- **Code path / outcome:** +glory = leader level. PARTIAL: no influence+travel. Human path = with-choice [30 1] travel.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 148. Board 30 / Slot 2 (player-facing #2) — [PARTIAL]

- **Printed intent:** Influence + sell; Amity = leader level
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [30 2]`; bot fallback `apply-bonus-dispatch [30 2]`.
- **Choice:** :pick-city — "Choose magistrate destination" (filter :magistrate)
- **Code path / outcome:** +amity = leader level. PARTIAL: no influence+sell. Human path = with-choice [30 2] influence+sell.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 149. Board 30 / Slot 3 (player-facing #3) — [PARTIAL]

- **Printed intent:** Deploy + influence; Glory = raider level
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [30 3]`; bot fallback `apply-bonus-dispatch [30 3]`.
- **Choice:** :pick-city — "Choose magistrate destination then deploy" (filter :magistrate)
- **Code path / outcome:** +glory = raider level. PARTIAL: no deploy+influence. Human path = with-choice [30 3] influence+deploy.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 150. Board 30 / Slot 4 (player-facing #4) — [PARTIAL]

- **Printed intent:** Influence + temple; Amity = priest level
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [30 4]`; bot fallback `apply-bonus-dispatch [30 4]`.
- **Choice:** :pick-city — "Choose magistrate destination" (filter :magistrate)
- **Code path / outcome:** +amity = priest level. PARTIAL: no influence+temple. Human path = with-choice [30 4] influence+temple.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 31 — Horizon of Sharkalisharri

## 151. Board 31 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Other astronomer on space 7 -> bonus travel
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[31 :landing]` (gated by `has-passive?`).
- **Trigger:** `:landing` — fires on landing.
- **Code path / outcome:** NO-OP / skip — 'complex positioning check'. Passive does nothing.
- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.

## 152. Board 31 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Increase all level-1 roles
- **Entry point:** auto-resolve `apply-bonus-dispatch [31 1]` (no choice descriptor).
- **Code path / outcome:** For each role at level 1: set to 2.
- **Honesty:** `:implemented`.

## 153. Board 31 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Increase all level-3 roles
- **Entry point:** auto-resolve `apply-bonus-dispatch [31 2]` (no choice descriptor).
- **Code path / outcome:** For each role at level 3: increase-role-with-cost.
- **Honesty:** `:implemented`.

## 154. Board 31 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Resource + facedown temple
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [31 3]`; bot fallback `apply-bonus-dispatch [31 3]`.
- **Choice:** :pick-resource — "Choose a resource to gain"
- **Code path / outcome:** +1 :gems; if a :face-up temple exists, flip it :face-down, +2 amity. Human path = with-choice [31 3] pick-resource.
- **Honesty:** `:implemented`.

## 155. Board 31 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Resource + deploy
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-resource}` -> human path `apply-bonus-with-choice [31 4]`; bot fallback `apply-bonus-dispatch [31 4]`.
- **Choice:** :pick-resource — "Choose a resource to gain"
- **Code path / outcome:** +1 :tools; place a raiding raider on any free route. Human path = with-choice [31 4] pick-resource.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 32 — Jewel of Ku-Bau

## 156. Board 32 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Sell: discard gem for priest-level scoring
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[32 :sold]` (gated by `has-passive?`).
- **Trigger:** `:sold` — fires on a sell.
- **Code path / outcome:** If gems>0 and priest-lv>merchant-lv: stash :passive-choice-needed {:yes-no}. apply-passive-choice (board 32): yes -> discard gem, re-score the sell at priest-level amity (merchant-score priest-lv) replacing merchant amity (QA lesson 7 fix).
- **Honesty:** `:persistent`; uses a deferred choice resolved in `apply-passive-choice` (board 32).

## 157. Board 32 / Slot 1 (player-facing #1) — [PARTIAL]

- **Printed intent:** Sell + glory per demand
- **Entry point:** auto-resolve `apply-bonus-dispatch [32 1]` (no choice descriptor).
- **Code path / outcome:** +glory = count of held demand-tokens (sell+glory-per-demand approximation).
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 158. Board 32 / Slot 2 (player-facing #2) — [PARTIAL]

- **Printed intent:** Gem + travel
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [32 2]`; bot fallback `apply-bonus-dispatch [32 2]`.
- **Choice:** :pick-city — "Choose a city to travel to" (filter :adjacent)
- **Code path / outcome:** +1 :gems. PARTIAL: no travel. Human path = with-choice [32 2] travel.
- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. See note in code path.

## 159. Board 32 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Raider between temple cities
- **Entry point:** auto-resolve `apply-bonus-dispatch [32 3]` (no choice descriptor).
- **Code path / outcome:** Place a raiding raider on a free route between two of your temple cities.
- **Honesty:** `:implemented`.

## 160. Board 32 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Influence + sell
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [32 4]`; bot fallback `apply-bonus-dispatch [32 4]`.
- **Choice:** :pick-city — "Choose magistrate destination" (filter :magistrate)
- **Code path / outcome:** +2 amity, +2 glory (influence+sell approximation). Human path = with-choice [32 4] influence+sell.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 33 — Vanguard of Enmebaragesi

## 161. Board 33 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Deploy -> influence adjacent magistrate
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[33 :deployed]` (gated by `has-passive?`).
- **Trigger:** `:deployed` — fires after a deploy.
- **Code path / outcome:** NO-OP / skip — 'would need to insert an influence action'. Passive does nothing.
- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.

## 162. Board 33 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Decrease merchant + increase others
- **Entry point:** auto-resolve `apply-bonus-dispatch [33 1]` (no choice descriptor).
- **Code path / outcome:** If merchant>1: dec :merchant, increase-role-free :raider and :priest. Else no-op.
- **Honesty:** `:implemented`.

## 163. Board 33 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Facedown temple + travel
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [33 2]`; bot fallback `apply-bonus-dispatch [33 2]`.
- **Choice:** :pick-city — "Choose a city to travel to" (filter :adjacent)
- **Code path / outcome:** Flip first :face-up temple to :face-down, +3 amity; if none, +1 amity. Human path = with-choice [33 2] travel.
- **Honesty:** `:implemented`.

## 164. Board 33 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Temple in Uruk
- **Entry point:** auto-resolve `apply-bonus-dispatch [33 3]` (no choice descriptor).
- **Code path / outcome:** place-temple-in :uruk face-up.
- **Honesty:** `:implemented`.

## 165. Board 33 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Deploy + travel
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [33 4]`; bot fallback `apply-bonus-dispatch [33 4]`.
- **Choice:** :pick-city — "Travel to adjacent city and deploy" (filter :adjacent action :deploy)
- **Code path / outcome:** Place a raiding raider on any free route. Human path = with-choice [33 4] travel+deploy.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 34 — Honor of Agga

## 166. Board 34 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Score raiders -> amity instead of glory
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[34 :raider-scored]` (gated by `has-passive?`).
- **Trigger:** `:raider-scored` — fires when a raider is scored.
- **Code path / outcome:** Sets flag :raider-score-amity true so score-own-raider-on-route adds amity instead of glory.
- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).

## 167. Board 34 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Pay tools for raiders around Uruk
- **Entry point:** auto-resolve `apply-bonus-dispatch [34 1]` (no choice descriptor).
- **Code path / outcome:** Place raiders on up to min(tools,available,2) free routes touching :uruk, paying 1 tool each.
- **Honesty:** `:implemented`.

## 168. Board 34 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Raider on each existing route
- **Entry point:** auto-resolve `apply-bonus-dispatch [34 2]` (no choice descriptor).
- **Code path / outcome:** Place raiding raiders on up to 2 free routes (each existing route approximation).
- **Honesty:** `:implemented`.

## 169. Board 34 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Sell at each magistrate+temple city
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [34 3]`; bot fallback `apply-bonus-dispatch [34 3]`.
- **Choice:** :pick-city — "Sell in a city with Magistrate + your Temple (no travel)" (filter :magistrate-and-my-temple multi)
- **Code path / outcome:** AUTO: +amity = count of cities that are magistrate AND hold your temple. Human path = with-choice [34 3] multi-pick sell, no travel.
- **Honesty:** `:implemented`.

## 170. Board 34 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Sell at each magistrate+temple city (same as #3)
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [34 4]`; bot fallback `apply-bonus-dispatch [34 4]`.
- **Choice:** :pick-city — "Sell in a city with Magistrate + your Temple (no travel)" (filter :magistrate-and-my-temple multi)
- **Code path / outcome:** Same as [34 3] (duplicate of #3 per status comment '34-4 same as 34-3').
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---

# Board 35 — Wanderer of Dumuzi

## 171. Board 35 / Slot 0 (passive) — [PERSISTENT (passive)]

- **Printed intent:** Start of turn, no goods -> gain good of choice
- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[35 :turn-start]` (gated by `has-passive?`).
- **Trigger:** `:turn-start` — fires start of turn.
- **Code path / outcome:** If total of all resources == 0: stash :passive-choice-needed {:pick-resource}. apply-passive-choice (board 35): +1 chosen resource.
- **Honesty:** `:persistent`; uses a deferred choice resolved in `apply-passive-choice` (board 35).

## 172. Board 35 / Slot 1 (player-facing #1) — [IMPLEMENTED]

- **Printed intent:** Travel + sell
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-city}` -> human path `apply-bonus-with-choice [35 1]`; bot fallback `apply-bonus-dispatch [35 1]`.
- **Choice:** :pick-city — "Travel to adjacent city and sell" (filter :adjacent action :sell)
- **Code path / outcome:** +3 amity (travel+sell approximation). Human path = with-choice [35 1] travel+sell.
- **Honesty:** `:implemented`.

## 173. Board 35 / Slot 2 (player-facing #2) — [IMPLEMENTED]

- **Printed intent:** Pay pottery for temples
- **Entry point:** auto-resolve `apply-bonus-dispatch [35 2]` (no choice descriptor).
- **Code path / outcome:** Pay up to 2 :pottery, place that many temples in distinct free cities.
- **Honesty:** `:implemented`.

## 174. Board 35 / Slot 3 (player-facing #3) — [IMPLEMENTED]

- **Printed intent:** Increase role of choice
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-role}` -> human path `apply-bonus-with-choice [35 3]`; bot fallback `apply-bonus-dispatch [35 3]`.
- **Choice:** :pick-role — "Choose a role to increase"
- **Code path / outcome:** increase-role-with-cost on the lowest-level role. Human path = with-choice [35 3] pick-role.
- **Honesty:** `:implemented`.

## 175. Board 35 / Slot 4 (player-facing #4) — [IMPLEMENTED]

- **Printed intent:** Influence + score raiders
- **Entry point:** `bonus-needs-choice?` returns `{:type :pick-role}` -> human path `apply-bonus-with-choice [35 4]`; bot fallback `apply-bonus-dispatch [35 4]`.
- **Choice:** :pick-role — "Choose a role to increase"
- **Code path / outcome:** +glory = 2 + count of :point raiders (influence+score approximation). Human path = with-choice [35 4] pick-role / score.
- **Honesty:** `:implemented`.
- **Slot-5 bug link:** this slot (code index 4) is the "slot 5" referenced by the separate bug task.

---
