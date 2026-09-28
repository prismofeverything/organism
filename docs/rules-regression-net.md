# How we know a rules change has not broken the game

## What failed, and why nothing saw it

`require-useful-action` filters which action type an organism may declare for its
turn. It asked whether the action could be taken *that instant* rather than at
any point in the turn — and a turn is several actions, any of which may be a
circulate. Food on the wrong element is a detour, not a wall.

A player reported it: an organism holding four food, three on its eat element and
one on its move, none on either of its two growers, with three spaces it could
grow into, was offered only EAT and MOVE. It could have circulated onto a grower
and grown. That is an ordinary mid-game position.

It was live for six days, in all three engines, on the site, and in every
training run. Nothing detected it, and the reason is worth stating plainly:

- **Cross-engine parity proves agreement, not correctness.** The same
  misunderstanding was written into Clojure, Python and Rust, so all 1,326
  sampled positions agreed. A shared misreading passes by construction.
- **The rule tests asserted the intended behaviour**, which was the thing that
  was wrong. `useful_action_rule_removes_deliberate_passing_without_stranding_anyone`
  checked that *some* choice remained, never that the *right* choices remained.
- **The measurement that "validated" it** was "deliberate passing fell from 50.8%
  of action selections to 0.2%". True, and exactly what removing legal moves also
  produces. Only the number that was supposed to go down was measured.

Every check in the loop compared the implementation to itself.

## The three layers now in place

### 1. A runnable baseline

All four tightenings are flags in `organism.game`, defaulting on:

    *eat-threshold*  *require-useful-action*  *sacrifice-yields-nothing*  *stalemate-ends-game*

`game/with-original-rules` plays the game as first written. Before this, the
Clojure engine could only play one game, so a change had nothing to be compared
against. A baseline you cannot run is not a baseline.

`organism.baseline-test` holds each flag to account: the tightening bites when
on, the original behaviour returns when off.

### 2. Attribution

`organism.scripts.rules-attribution` walks the original game and, at every
position, asks the game twice — once as written, once with every tightening on.
For each difference it asks which single rule accounts for it, by turning that
rule off and leaving the others on.

    lein run -m organism.scripts.rules-attribution --games 6 --steps 400 [--verbose]

Three ways to fail:

- **unattributed** — the original game allows a move, the tightened game does
  not, and no rule claims it
- **unjustified** — a rule owns the removal but cannot justify it (below)
- **invented** — the tightened rules offer something the original game does not

Attribution alone is not enough, and this is the part that matters. The bug *was*
attributable: `require-useful-action` removed GROW, and turning it off restored
it, so a harness that only attributed would have passed. Owning a removal is not
being entitled to it. `require-useful-action` claims only to remove types the
organism cannot use, so that claim is checked **by searching the base game's turn
tree** — declare the type and look for an action of it actually being performed,
past any number of circulates. That search is independent of the predicate under
test, which is the whole point; asking the predicate would be the same circle.

Verified both ways: with the broken filter restored it reports unjustified
removals of GROW and MOVE; with the fix it passes. `organism.attribution-test`
runs a small sample on every `lein test`.

### 3. Golden positions

`organism.golden-positions-test` — boards with the answer stated by a person, not
derived from any implementation. A board carries no rules, so a position recorded
by a build whose rules were wrong is still safe to pin; what makes it golden is
the stated answer. The reported position is the first case, with the designer's
words in it, alongside the negative case so the rule keeps its teeth.

This is the only place ground truth enters. Add a case whenever a rule is decided
or found wrong, and prefer constructed positions to harvested ones — a position
scraped from a buggy build may be unreachable in the real game, and asserting
about an impossible board is worse than asserting nothing.

## Before changing a rule

1. Add or update a golden position saying what the rule should do
2. Make the rule a flag if it is not already
3. Run `lein test` — baseline, attribution and golden positions all run
4. Run the attribution script at a larger sample than the test uses
5. Check `tests/check_native_parity.py` for cross-engine agreement — last, and
   knowing it cannot catch a shared misreading

## What this still does not cover

- **Rules that are too permissive.** Attribution catches moves wrongly removed
  and moves wrongly invented at the choice level, but a rule that should forbid
  something and does not looks like the original game.
- **Anything not written down.** Golden positions only cover intent somebody
  recorded.
- **The other two engines.** The flags and the harness are Clojure. Rust has the
  same flags; Python has none. Parity ties them to Clojure's behaviour, so a
  Clojure-side regression caught here would have been caught before reaching
  them — but a Rust-only change is still only covered by parity.
