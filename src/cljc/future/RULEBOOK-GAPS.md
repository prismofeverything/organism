# FUTURE — single-shot generation: what the rulebook was missing

Inputs available to the single-shot attempt:

* `Sol_ Burrowing the Future - Rules.md` — the rulebook
* `beginning-board-update.png` — board image
* `sol2_playerboard_v4.png` — player board image
* The pre-existing `organism` framework (websockets, ajax, components, layout
  shell, Selmer templates, shadow-cljs build, Ring routes, persistence)
* `journey/` as the reference implementation pattern

This document records (a) every interpretive call I made while writing
`board.cljc`, `game.cljc`, and `play.cljs`, (b) what the rulebook structurally
lacks for a faithful one-shot implementation, and (c) principles distilled
from the `journey/` codebase about what really needs to be supplied.

---

## A · Rulebook ambiguities I had to resolve

For each item: **rule text → choice I made**.

### Board topology

1. **Sun-wedge orientation.** Rules say first player places on silver, then
   "next wedge CCW" gets green, then blue, purple, void. The angular
   position of the silver wedge is never stated. *Chose:* wedge 0 = silver,
   on the beam (north); going CCW around the sun gives the placement order.
2. **Sun-to-silver adjacency.** Rules state sundivers can "fly to one of the
   5 spaces in the sun" — implies adjacency to silver, but the mapping is
   never specified. *Chose:* sun wedge `k` is adjacent to silver-orbit
   space `k` (1 : 1, by angular overlap).
3. **Sun-to-sun adjacency.** Never stated. *Chose:* no adjacency between sun
   wedges — you must go through silver.
4. **Inter-ring adjacency rule.** Rulebook implies but never defines the
   rule. *Chose:* angular-range overlap (standard for nested rings).
5. **Frontmost-adjacent-in-ring.** When the mothership shifts orbits or a
   launch targets an inner/outer ring, the destination is "the first space
   in front of the mothership up or down." With Fibonacci ring sizes,
   multiple inner/outer candidates overlap. *Chose:* the candidate whose
   angular midpoint is the smallest CCW distance from the source.

### Sun & components

6. **Initial component placement section.** Components placed at setup go
   onto the wedge — but not specified whether into the inner (active) or
   outer (exhausted) zone. *Chose:* active.
7. **Sundivers in the sun.** Rules say "the red section is for sundivers
   without resources, the inner color section is for sundivers carrying
   a resource." Behavior for sundivers carrying a *non-matching* resource is
   undefined. *Chose:* treat them as outer-section (can only do red
   activation); inner activation requires a *matching* resource.
8. **Sun-outer refresh scope.** "Move all exhausted components to the
   active half of the space." Just this wedge, or all wedges? *Chose:*
   just this wedge.
9. **Which sundiver returns on outer activation?** "Return the sundiver."
   Ambiguous when multiple are present. *Chose:* one, preferring a
   non-matching-resource sundiver; resource returned to its market row.
10. **Mothership pull when already on a sun wedge.** The rule covers only
    "pulled into the sun" from silver. *Chose:* no-op when already on sun.

### Cards, dice, deck

11. **Single d4+d10 roll for planet setup.** "Find the remainder after
    dividing die roll by its orbital number — advance the planet that many
    spaces." *Chose:* one roll for all planets (mod each ring size).
12. **Card values 1–13.** Used for anything beyond suit identity? Rulebook
    never references the values. *Chose:* unused.
13. **Drawing from an empty deck.** Not addressed. *Chose:* no-op stop;
    discard pile is never reshuffled.
14. **13th flare timing.** "Game ends immediately." Does that mean before
    or after the last-card-resolution? *Chose:* check after applying that
    draw's flare effect but before last-card-resolution would advance
    planets (game just ends).
15. **Card-of-zero-draws turn.** If the player picks "done activating"
    without activating anything, do they still draw? *Chose:* yes, with the
    accumulated draw count (zero is allowed; pass-equivalent).

### Movement

16. **Movement points = flat 5.** Explicit in the rulebook; I followed.
17. **Launching from habitat — fourth space.** "To either side (up or down
    one ring) to the *front* of the mothership." *Chose:* frontmost
    adjacent in the inner ring + frontmost adjacent in the outer ring.
18. **Sundiver on/off planet during movement.** "It is free to place your
    sundiver on or off of the planet." *Skipped:* my sundivers always stay
    on their current space; planets orbit without carrying sundivers along.
    **This is a real mechanic that I did not implement.**
19. **Link-chain travel.** "Travel along a continuous chain of paths of one
    player color to any space on that path." *Chose:* BFS from the start
    position restricted to links owned by a single player; landing at any
    visited space costs 1 movement point; if not the moving player's color,
    the chain owner gains 1 energy.

### Cities, links, activation

20. **City activation owner-bonus negotiation.** "Owner has the choice to
    take the bonus." Requires a multi-player decision phase. *Skipped:*
    activator always gets bonus (city actions = base + bonus).
21. **Link "first link exhausts, same color reuses" optimization.**
    Implemented across a single TURN (not just a single city activation):
    `:exhausted-colors` set in phase-data tracks colors already paid for.
22. **Link adjacency.** "A link is placed between two spaces." Adjacency
    isn't explicit. *Chose:* board adjacency (same as movement).
23. **Per-player link saturation.** "A space is saturated for a player if
    they already have two links into/out of that space." Implemented as
    counting links of any direction touching the space, per owner.
24. **Single-link-between-spaces rule.** "Only one link between any two
    spaces." *Chose:* applies across all players (a global edge constraint).
    The rulebook is ambiguous here — could be per-player.
25. **City buildable conditions.** "If there is no city of that color in
    this ring already." *Chose:* `market-cities` count for the ring must be
    zero. Note: rulebook is "of that color" but elsewhere only one city per
    ring (period) seems implied by the level system.
26. **Replenishing resources.** "Replenish the resources of that row up
    to 5." Where do they come from? *Chose:* infinite (only the cap of 5
    matters).
27. **Vaporized sundivers.** "It may never return." Tracked as a counter
    only; they are removed from board and never recreated.

### Turn / flame

28. **Initial flame holder.** "Random or most hostile." *Chose:* the first
    player in the supplied list — deterministic for testing.
29. **Turn order direction.** "Player next in turn order." Direction never
    defined. *Chose:* the order players were passed to `create-game`.
30. **Mothership advance to flame.** "Advance your mothership to the space
    with the flame of justice." *Chose:* warp the mothership to the
    flame-space directly (the flame is always 1 space in front of the
    mothership, so this is just a 1-step CCW advance).
31. **No "pass" option.** Rules don't allow passing. *Chose:* `:move` is
    always available because launching is always a valid choice when
    habitat > 0, and `:moving` always has at least `:done-moving`.

### Scoring & end

32. **What counts as a "path".** "Every player scores valid paths (links
    between cities or between a city and the sun)." *Chose:* a path =
    a pair of city endpoints in the same connected component of the link
    graph where the player owns at least one link. The owner-vs-color
    qualification logic follows the rulebook's "valid endpoint" definition.
33. **The "+1 for each component beyond 1" bonus.** "For each end of the
    path that is valid because it is a city of another player that matches
    the color of one of their components, they get an additional point for
    each component beyond 1 they have of that color." *Implemented* but
    the rulebook's "beyond 1" is ambiguous — does it count exhausted
    components too? *Chose:* yes (total components in the wedge).
34. **Salvation tie-victory.** "If all players end with the same number of:
    points, components, cities — communal victory." **Not implemented.**
35. **"Tied players lose."** I implemented the deterministic "highest
    untied score wins" cascade but it's a one-shot interpretation.

---

## B · Structural gaps — things the rulebook did not contain at all

These are the supplementary specifications a true one-shot generator would
need *in addition to* the rules text:

### Visual layout

* **Board geometry.** Pixel/SVG radii, sun layout, beam direction, color
  hexes. I inferred from the image, but a deterministic spec would be a
  layout DSL or coordinate table.
* **Wedge color assignment.** The image suggests an orientation; the
  rulebook does not specify which wedge is which color in space.
* **Player-board.** The second image (`sol2_playerboard_v4.png`) hints at
  optional per-player UIs (e.g., per-color SOL counters); they don't appear
  in the rules text I was given.

### State representation

* **Canonical state shape.** Where do sundivers live? Per-space lists?
  Per-player dictionaries? The rulebook describes mechanics, not data.
* **Pieces vs counts.** Are components individuated (this is *Alice's*
  component on wedge 3) or just counted? The rulebook implies
  individuation by ownership but only the *count* matters mechanically.

### State machine

* **Phase enumeration.** The rulebook describes a turn narratively. To
  generate code you need to enumerate the discrete phases (`:place-mothership`,
  `:pre-action`, `:action`, `:moving`, `:activating`, `:placing-links`)
  and their transitions.
* **Sub-choice trees.** Within "activate", which choices spawn sub-phases
  (link placement) vs single transitions? The journey codebase models this
  as nested `[:player-turn :action ...]` keys — a pattern not derivable
  from prose.
* **Auto-advance policy.** When a phase has exactly one choice, do you
  skip past it (journey does, for non-strategic phases)?

### Choice / action enumeration

* **Action key shapes.** The framework requires `legal-actions` to return
  `{choice-key → next-state}`. The shape of `choice-key` (vectors with
  verbs and positional args) is a convention not derivable from the
  rulebook.
* **Action labels.** Player-facing strings ("place mothership @ silver·0")
  are not in the rulebook; they're UX.

### UI

* **Panel layout.** Side-panel composition (status / players / market /
  solar / supply / hand / actions) is not in the rulebook.
* **Interaction model.** Click a space vs click an action button vs
  drag-drop? Not specified.
* **Where to surface phase context.** "5 moves left", "activating
  cities", "10/13 flares" — implicit UX.

### Edge cases & defaults

* **Empty deck.** Reshuffle? Stop drawing? Not specified.
* **Invalid action recovery.** What if a player has no legal action?
  Specifically: no sundivers, no habitat, no energy → can't move OR
  activate. Not addressed.
* **Out-of-pool resources / energy / components.** The rulebook does say
  energy stays in [0, 5] for market; doesn't say what happens for
  components / city platforms / links running out.

### Bot policy

* **No play heuristic.** The `generate` mode needs a bot that produces
  reasonable-looking games. The rulebook doesn't even hint at strategy
  shape. I used "random pick that prefers non-end actions."

### Persistence

* **Save/load.** The Organism framework persists games to Mongo; the
  rulebook is silent on serialization.

---

## C · Principles from `journey/` (the most complete reference)

What journey's structure tells us about what *any* new game in this
framework needs:

1. **A small public surface.** journey exposes effectively three functions
   to the server/UI: `create-game`, `current-player/current-phase`, and a
   phase-aware `legal-actions` (in journey's case `find-state`). Everything
   else is internal.

2. **Phases are first-class.** `[:player-turn :phase]` (journey) or
   `:phase` (future) is the state-machine driver. Every transition writes a
   new phase keyword.

3. **Phase data lives in a `:phase-data` (or `:player-turn :action`)
   nested map.** Sub-choice tracking (links remaining, moves remaining,
   activated spaces, exhausted colors this turn) belongs there, not at the
   top level.

4. **Legal actions = `{choice-key → next-state}`.** Both server (apply
   action) and bot/UI (enumerate / preview) consume the same shape.

5. **Auto-advance through degenerate phases.** Journey's `find-state`
   loops past phases with exactly one choice — cleaner than asking the
   player to click "OK, only one option."

6. **Choice keys are vectors with a verb head.** `[:fly src dst]`,
   `[:link a b color]`. Trivially serializable, easy to dispatch on.

7. **Board rendering is inert SVG.** No game logic in the rendering layer.
   Clicks just bubble up choice keys.

8. **The frontend bundle serves four pages.** `play / observe / create /
   generate` selected by JS globals set in the Selmer template
   (`js/playKey`, `js/isCreate`, `js/isObserve`, `js/isGenerate`).
   `mount-components` dispatches.

9. **Bots are pure functions over `legal-actions`.** The simplest possible
   bot picks one of the returned keys; sophistication is layered on by
   filtering and weighting.

10. **Shared client framework lives under `organism.*`.** Game-specific
    code never touches WebSockets directly — it goes through
    `organism.websockets`, `organism.ajax`, `organism.components`,
    `organism.layout`. New games can be written without knowing how those
    work.

---

## D · A minimal supplementary spec that would have made one-shot succeed

For any new game targeted at this framework, the input that would have
produced a complete implementation from a single prompt:

1. The rulebook (prose).
2. The board image + a **spatial spec** (orbits/rings/sizes/sectors as
   data — radii are nice-to-have, can be computed).
3. A **state-shape document** — top-level keys + per-player keys.
4. A **phase diagram** — phase names, transition triggers, sub-phase
   structure for multi-step actions (link placement, activation).
5. A **choice enumeration** — for each phase, the set of `choice-key`
   shapes the legal-actions function should produce.
6. An **edge-case policy** for: empty deck, no legal actions, ties,
   tie-breakers, salvation, multi-player decisions (owner bonus).
7. A **first-turn trace** — 5–10 turns of an example game showing state
   evolution; eliminates 80 % of ambiguity by example.
8. A **UI panel inventory** — what data the player must see.
9. A **bot policy** statement — "random uniform among non-end choices" is
   fine; just say so.

The rulebook alone gave me roughly 70 % of what I needed. The remaining
30 % was inferred from the image (geometry) + the journey codebase (state
shape, phase model, choice key shapes, UI panels). Without journey as a
template, the implementation would have been more guesswork and the gaps
above would have been deeper.
