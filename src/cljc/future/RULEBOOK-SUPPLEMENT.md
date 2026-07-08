# FUTURE — Rulebook supplement (v1)

Companion to `Sol_ Burrowing the Future - Rules.md` (rulebook v2, 2026-07-06)
and the `beginning-board-update.png` board image. Provides items 2–9 of the
minimal supplementary spec listed in `RULEBOOK-GAPS.md §D`, so a subsequent
one-shot generation of `board.cljc`, `game.cljc`, and `play.cljs` succeeds
without further interpretive guesswork.

Where the rulebook is silent, this document makes an explicit call and marks
it **[POLICY]**. Anything not marked is verbatim from the rulebook; anything
marked **[POLICY]** may be re-litigated in a later revision without changing
the code's public shape.

Conventions:

- All identifiers are Clojure keywords / vectors / strings.
- `PK` = player-key (string).
- `sid` = space id (see §2.2).
- `ck` = choice-key (vector with keyword head, see §5).
- Angular direction: **CCW is forward, CW is backward**, in every ring,
  including the sun. This matches the rulebook's "forward means CCW"
  statement and is the single source of truth for "in front of the
  mothership".

---

## §2 · Spatial spec — the board as data

### §2.1 Rings

Six rings, inner to outer:

| ring     | size | keyword   | orbital? |
|----------|------|-----------|----------|
| Sun      | 5    | `:sun`    | no       |
| Silver   | 5    | `:silver` | yes      |
| Green    | 8    | `:green`  | yes      |
| Blue     | 13   | `:blue`   | yes      |
| Purple   | 21   | `:purple` | yes      |
| Void     | 34   | `:void`   | yes      |

```clojure
(def ring-sizes {:sun 5 :silver 5 :green 8 :blue 13 :purple 21 :void 34})
(def orbits [:silver :green :blue :purple :void])          ; inner → outer
(def ring-adjacency [:sun :silver :green :blue :purple :void])
```

### §2.2 Space IDs

```
[:sun k]           k ∈ 0..4        ; sun wedges
[:orbit r n]       r ∈ orbits,      ; orbital ring space
                   n ∈ 0..(size-1)
```

Sun wedges and orbital spaces are the only kinds of on-board location.
Habitats and reserves are *containers*, not locations, and use per-player
counts (see §3).

### §2.3 Indexing convention

- **Orbital rings**: index `0` lies on the beam. Index increases **CW in
  screen-space** (per rendering convention). "Forward" (CCW) therefore
  means index `- 1 mod size`.
- **Sun wedges**: wedge `0` lies on the beam (silver). Index increases
  **CW in screen-space**. "Forward" (CCW) means index `- 1 mod 5`.

Rationale: rulebook v2 fixes the sun wedge colors going **CW from silver at
the beam**: `silver → green → blue → purple → void`. With CW = index+1,
the mapping is unambiguous.

```clojure
(def wedge-color                 ; canonical, per rulebook v2
  {0 :silver, 1 :green, 2 :blue, 3 :purple, 4 :void})
(def color->wedge (into {} (map (fn [[k v]] [v k]) wedge-color)))
```

**[POLICY]** The existing `board.cljc` in this branch had the wedges going
the opposite direction (`{0 :silver, 1 :void, 2 :purple, 3 :blue, 4 :green}`);
that mapping was inferred from rulebook v1's CCW placement text. Rulebook v2
supersedes it — regenerate.

### §2.4 Angular geometry

All rings share a common origin: the beam at angular position `0` (top of
screen). CW is angle-positive, CCW is angle-negative. Angular positions
are stored as **fractions of a full turn** (`0..1`) rather than degrees or
radians, avoiding float drift.

```clojure
(defn angular-range [sid]
  ;; Returns [start end) as fractions of full circle; CW from north.
  (cond
    (= :orbit (first sid))
    (let [[_ r n] sid, size (ring-sizes r)]
      [(/ n size) (/ (inc n) size)])
    (= :sun (first sid))
    (let [[_ k] sid]
      [(/ k 5) (/ (inc k) 5)])))
```

### §2.5 Adjacency

Two spaces are adjacent iff either:

1. **In-ring**, same ring, indices differ by ±1 mod size. Applies to the
   sun too (sun wedges *are* adjacent to their in-ring neighbors, contra
   the older interpretation).
2. **Cross-ring**, rings are ring-adjacent (see §2.1 `ring-adjacency`),
   and their `angular-range`s overlap by more than zero.

Consequences (worked out — good tests):

- Sun ↔ Silver: 1:1 by angular alignment. Since both have 5 spaces of
  equal width, wedge `k` is adjacent to `[:orbit :silver k]`.
  Explicitly: `[:sun 0] ↔ [:orbit :silver 0]`, `[:sun 1] ↔ [:orbit :silver 1]`, …
- Silver (5 spaces of 1/5 width) ↔ Green (8 spaces of 1/8 width): each
  silver space overlaps 1 or 2 green spaces. Silver `0` (span `0..1/5`)
  overlaps green `0` and green `1`.
- Every subsequent inter-ring adjacency is derived by the same overlap
  computation. There is no need for a hand-maintained table.

Movement uses adjacency, and **[POLICY]** sundivers can enter the sun by
flying from a silver space to the adjacent sun wedge. This is the *only*
route into the sun for sundivers.

### §2.6 Beam

```clojure
(def beam-orbital-spaces
  [[:orbit :silver 0] [:orbit :green 0] [:orbit :blue 0]
   [:orbit :purple 0] [:orbit :void 0]])
```

`[:sun 0]` (silver wedge) is also on the beam but is not a valid initial
mothership placement — the rulebook restricts placement to the 5 orbital
beam spaces.

### §2.7 Direction

```clojure
(defn front-space [sid]
  ;; CCW forward = index - 1 mod size in this indexing convention.
  (case (first sid)
    :orbit (let [[_ r n] sid] [:orbit r (mod (dec n) (ring-sizes r))])
    :sun   (let [[_ k] sid]   [:sun (mod (dec k) 5)])))

(defn back-space [sid]
  (case (first sid)
    :orbit (let [[_ r n] sid] [:orbit r (mod (inc n) (ring-sizes r))])
    :sun   (let [[_ k] sid]   [:sun (mod (inc k) 5)])))
```

The **space in front of the mothership** is always `(front-space
mothership-sid)`. That is the space the flame lands on after a ring-shift
or first-turn placement (see §4.2).

### §2.8 Ring-shift target

When the mothership shifts from ring `r` to ring `r'` (one step up or
down `ring-adjacency`), the new position is the **frontmost** ring-`r'`
space that is cross-ring-adjacent to the current mothership. "Frontmost"
means smallest CCW distance from the source's angular midpoint.

```clojure
(defn frontmost-adjacent-in-ring [adjacency src-sid target-ring]
  (let [candidates (filter #(and (= :orbit (first %)) (= target-ring (second %)))
                           (get adjacency src-sid #{}))
        src-mid    (angular-midpoint src-sid)]
    (first (sort-by
            (fn [c] (mod (- src-mid (angular-midpoint c)) 1.0))
            candidates))))
```

Silver → Sun shift lands on **[POLICY]** the sun wedge whose angular
midpoint is smallest CCW distance from the silver source (1:1 alignment
makes this deterministic: silver space `n` → `[:sun n]`).

### §2.9 Planets

```clojure
:planets {:silver [:orbit :silver n], :green ..., :blue ..., :purple ..., :void ...}
```

Each planet lives at one space in its own color's ring. Planets orbit
**CW = back = index+1**. Orbit distance per color:

```clojure
(def planet-advance-rate
  {:silver 1, :green 1, :blue 1, :purple 2, :void 3})
```

Sundivers **on a planet** ride along when the planet orbits; sundivers
**off** stay on the original space. Per-sundiver on/off is a decision
made at end of movement, before draws (see §4.6).

### §2.10 Sun wedge structure

Each sun wedge has two halves: **active** (colored) and **exhausted** (red).
Both halves are the same space for adjacency and movement purposes.

Placement of sundivers on a wedge is determined by resource:

- Sundiver **carrying a resource matching the wedge color** → active half.
- Sundiver **without a resource** → exhausted half.
- Sundiver **with a non-matching resource** → **[POLICY]** exhausted half.

Placement of a player's *components* is separate from sundiver placement:

- Components move `reserve → active` (via inner sun activation).
- Components move `active → exhausted` (via color-access exhaust — see §5.7).
- Components move `exhausted → active` (via outer sun activation of that
  wedge).

State keeps active/exhausted component counts per wedge, per player, in
`:solar-network`.

---

## §3 · State shape

### §3.1 Top-level

```clojure
{:board             {:adjacency {sid → #{sid...}}
                     :spaces    [sid ...]
                     :orbits    {orbit → [sid ...]}
                     :wedges    [[:sun 0] ... [:sun 4]]}
 :players           {PK → player-data}
 :turn-order        [PK ...]              ; insertion order, CW seat rotation
 :flame             PK                    ; whose turn it is
 :flame-space       sid | nil             ; where the flame token lives
 :phase             phase-keyword         ; one of §4
 :phase-data        {}                    ; per-phase transient (§5)
 :turn              int                   ; 0-based turn counter
 :deck              [card ...]            ; top of deck = last element
 :hands             {PK → [card ...]}     ; cards drawn this turn, unresolved
 :discard           [card ...]
 :flares-drawn      int                   ; 0..13
 :market-resources  {color → 0..5}
 :market-cities     {color → 0..4}
 :energy-pool       int                   ; shared pool, sum with player energies = 89
 :planets           {color → sid}
 :sundivers         {sid → [{:owner PK :resource nil|color :on-planet? bool} ...]}
 :cities            {sid → {:owner PK :color color}}
 :links             #{{:a sid :b sid :owner PK :color color} ...}
 :solar-network     {wedge-idx → {:active {PK → int}, :exhausted {PK → int}}}
 :winner            nil | :salvation | {:result :win :winner PK :scores {PK → int}}
                                        | {:result :none :scores {PK → int}}}
```

Cards:

```clojure
{:suit  :silver | :green | :blue | :purple | :void | :flare
 :value 1..13}                            ; :value is unused mechanically; kept for shuffle identity
```

Links: order-independent by `:a`/`:b`; canonicalize by sorting the pair
when creating a link record.

### §3.2 Per-player

```clojure
{:wedge-color       color                 ; color of the wedge seeded at setup
 :mothership        sid | :supply         ; :supply until first-turn placement
 :habitat           int                   ; sundivers in habitat
 :reserve           int                   ; sundivers in reserve (never launched)
 :energy            int                   ; energy held in habitat
 :components        int                   ; components remaining in personal supply
 :city-platforms    int                   ; unplaced city platforms
 :links-supply      int                   ; unplaced links
 :vaporized         int}                  ; sundivers permanently removed
```

Every piece is accounted for. For player `P` at any time:

- Sundivers total = `habitat + reserve + on-board(P) + vaporized = 13`
- Components total = `reserve + Σ solar-network active(P) + Σ solar-network exhausted(P) = 8`
- City platforms total = `city-platforms + platforms-on-board(P) = 5`
- Links total = `links-supply + links-on-board(P) = 13`

These are useful invariants for tests.

### §3.3 Constants

```clojure
(def starting-components-per-player 8)
(def starting-sundivers-per-player  13)   ; 8 habitat + 5 reserve
(def starting-links-per-player      13)
(def starting-platforms-per-player  5)
(def starting-habitat-sundivers     8)
(def starting-reserve-sundivers     5)
(def starting-energy-per-player     5)
(def initial-energy-pool            89)
(def initial-market-resources       {:silver 1 :green 2 :blue 3 :purple 4 :void 5})
(def market-resource-cap            5)
(def cities-per-color               4)
(def card-suits [:silver :green :blue :purple :void :flare])
(def cards-per-suit                 13)
(def flares-to-end                  13)
(def movement-points                5)

(def resource-price-by-stock        {5 1, 4 1, 3 2, 2 3, 1 5})
(def city-level-actions             {1 {:base 1 :bonus 1}
                                     2 {:base 2 :bonus 1}
                                     3 {:base 3 :bonus 2}})
```

### §3.4 `:phase-data` shape by phase

Each phase's transient state (see §4 for the machine, §5 for choices):

- `:place-mothership`  `{}`
- `:resolve-mothership` `{}`
- `:choose-action-type` `{}`
- `:moving` `{:moves-left n, :used-any? bool}`
- `:on-planet-decisions` `{:remaining [sundiver-ref ...]}` where `sundiver-ref = [sid idx]`
- `:activating` `{:target :sun|:planets|:cities, :remaining #{sid ...}, :cards-owed n, :activated-count n, :exhausted-colors #{color ...}}`
- `:activating-space` `{:current sid, ...parent activating fields}`
- `:link-placement`   `{:actor PK, :actions-left n, :is-bonus? bool, :parent-activation activating-fields}`
- `:owner-bonus-decision`   `{:owner PK, :activator PK, :bonus n}`
- `:activator-bonus-decision` `{:activator PK, :bonus n}`
- `:drawing-cards`    `{:cards-owed n, :cards-drawn n}`
- `:orbit-planets`    `{:last-card card}`
- `:advance-mothership` `{}`
- `:pass-flame`       `{}`
- `:game-over`        `{:scores {PK → int}, :winner ..., :salvation? bool}`

The **choice-player** in `:owner-bonus-decision` is the *owner*, not the
current flame-holder; see §6.6.

---

## §4 · Phase diagram

Terminology: **phase** = state-machine node; **automatic phase** =
degenerate node with exactly one legal choice (see §9 for auto-advance).

### §4.1 Turn boundary

Turns are *player*-turns. A turn begins when a new player's turn starts
(`:pass-flame` transitions to a start phase) and ends when `:pass-flame`
completes for the current player.

### §4.2 Start of turn

If `motherships[current-player] = :supply`:

```
:place-mothership → [:place-mothership space] → :choose-action-type
```

Else:

```
:resolve-mothership → [:shift-in]|[:shift-out]|[:stay] → :choose-action-type
```

On `[:place-mothership space]`: motherships[cp] := space; flame-space :=
`(front-space space)`.

On `[:shift-in]`: motherships[cp] := frontmost-adjacent-in-ring in inner
ring; flame-space := `(front-space new-position)`.

On `[:shift-out]`: analogous with outer ring.

On `[:stay]`: motherships[cp] and flame-space unchanged. (The flame stays
where it was placed at the end of the previous turn — in front of the
current player's mothership — which is the space the mothership advances
into at end-of-turn.)

### §4.3 Action-type branch

```
:choose-action-type
    ├── [:choose-move]     → :moving  (:moves-left = 5)
    └── [:choose-activate] → :activating (target unset)
```

`[:choose-activate]` is legal only if at least one of the three
activation targets has ≥1 player sundiver.

### §4.4 Moving branch

```
:moving
    ├── [:launch dst]      (dst ∈ launch-targets(state), habitat > 0)
    ├── [:fly src dst]     (src has player sundiver, dst adjacent to src)
    ├── [:path src dst]    (path-travel via link chain — see §5.4)
    └── [:done-moving]     (always)
```

- `launch-targets`: `{mothership-space, front-space(mothership-space),
  frontmost-adjacent-in-ring(inner), frontmost-adjacent-in-ring(outer)}` —
  up to 4 unique spaces.
- `[:launch dst]`: `habitat -= 1`, add sundiver `{owner, nil, false}` at
  dst.  `moves-left -= 1`.
- `[:fly src dst]`: pick *one* player sundiver at src, move to dst,
  preserving `:resource` and `:on-planet?` (but drop `:on-planet?` if
  leaving a planet space). `moves-left -= 1`.
- `[:path src dst]`: as `[:fly]`, but dst reachable via same-owner-color
  link chain; if chain owner ≠ current-player, chain-owner gains 1 energy.
  `moves-left -= 1`.
- `[:done-moving]`: transition to `:on-planet-decisions`. `moves-left`
  may be > 0.

### §4.5 Activating branch

```
:activating (target unset)
    ├── [:activate-sun]     → :activating (target := :sun, remaining := sun-spaces-with-my-sundivers)
    ├── [:activate-planets] → :activating (target := :planets, remaining := planet-spaces-with-my-sundivers)
    └── [:activate-cities]  → :activating (target := :cities, remaining := city-spaces-with-my-sundivers)

:activating (target set, activated-count = 0)
    ├── [:activate-space sid]  (sid ∈ remaining)
    └── nothing else — you MUST activate ≥1

:activating (target set, activated-count > 0)
    ├── [:activate-space sid]  (sid ∈ remaining, if remaining non-empty)
    └── [:done-activating]     → :drawing-cards
```

Each `[:activate-space sid]` transitions to a **sub-phase** determined by
target:

- `:sun`     → `:activating-sun-space`     (choose inner/outer + sundiver)
- `:planets` → `:activating-planet-space`  (choose buy/build + sundiver)
- `:cities`  → `:link-placement`           (base actions for activator)

Sub-phases resolve, add to `cards-owed`, then return to
`:activating (target set, activated-count > 0)`. Details in §5.

### §4.6 On-planet decisions

```
:on-planet-decisions
    for each remaining player sundiver on a planet-space:
      ├── [:planet-on  [sid idx]]
      └── [:planet-off [sid idx]]
    auto-completes → :drawing-cards
```

Only sundivers on planet-spaces (spaces where a planet currently resides)
get this decision. Sundivers that were on a planet but whose planet has
moved away were already left behind at their space during the earlier
`:orbit-planets` phase of a **previous** turn — so this list is derived
freshly at start-of-phase from the current planet positions.

### §4.7 Drawing cards

`:drawing-cards` iterates card draws one at a time:

```
:drawing-cards
    single choice: [:draw-next] until (cards-drawn = cards-owed)
                                     OR (:flares-drawn = 13, terminate immediately)
```

Each `[:draw-next]`:

1. Pop top card from `:deck`.
2. Append to `:hands[current-player]`.
3. If flare:
   a. `:flares-drawn += 1`.
   b. Pull the mothership one ring inward (§4.7.1).
   c. If `:flares-drawn = 13`, mark game-ending; skip remaining draws
      and orbit; transition directly to `:game-over` after applying
      this card.
4. If deck was empty: **[POLICY]** no-op (no card drawn, no effect).
5. `cards-drawn += 1`.

Once `cards-drawn = cards-owed`, transition to `:orbit-planets`.

`cards-owed` breakdown:

- Move branch: 1.
- Activate branch, per activation:
  - Sun space:    1 card
  - Planet space: 1 card
  - City space:   `city-level-actions[level][:base] + city-level-actions[level][:bonus]` (i.e., level 1 = 2, level 2 = 3, level 3 = 5)
    but **[POLICY]** cards drawn equal cards *earned* by activation
    (base + bonus if actually taken); simpler interpretation:
    cards-owed for a city = base + bonus regardless of decline.

The rulebook says "each city you activate you draw cards based on its
level" — this doesn't distinguish base vs. bonus. **[POLICY]** use
`base + bonus` for the draw count (fixed by city level, independent of
whether the bonus was actually taken by anyone).

#### §4.7.1 Flare-pull mechanics

Flare pull: mothership one ring inward, along `ring-adjacency`.

- If mothership is on `[:orbit :silver …]` (would enter sun):
  - Advance mothership 1 forward (CCW) in silver (index `- 1 mod 5`).
  - `energy(cp) -= ⌈energy(cp) / 2⌉`; the paid energy returns to
    `:energy-pool`.
- If mothership is on any other orbital ring:
  - New position = `frontmost-adjacent-in-ring(adjacency, current, inner-ring)`.
- **[POLICY]** If mothership is on a sun wedge (only reachable via
  voluntary shift-in from silver — see §2.8 note): flare has no effect.
- **[POLICY]** Flare pull does *not* set the flame-space (which was
  established at start-of-turn and is where the mothership will end up
  via `:advance-mothership`). The flame remains at its start-of-turn
  position; if the flare pulled the mothership *past* the flame-space,
  the end-of-turn `:advance-mothership` still moves to the flame-space
  (which may now be more than 1 step forward).

### §4.8 Orbit-planets

```
:orbit-planets
    single choice: [:orbit-resolved]
```

Determined by the **last** card in `:hands[current-player]`:

- Flare: orbit *all* planets by `planet-advance-rate[color]` each.
- Suit color: orbit only that color's planet by
  `planet-advance-rate[color]`.

Orbit: new-index = `(old-index + rate) mod ring-size` (CW = back = +index).

For each sundiver at the *old* planet-space with `:on-planet? true`:
move it (as a full sundiver record) to the new planet-space. Sundivers
`:on-planet? false` stay at the old space.

### §4.9 Advance-mothership

```
:advance-mothership
    single choice: [:advance-resolved]
```

Move current player's mothership to `flame-space`. This may traverse
multiple spaces (e.g., after a flare pull moved the mothership past the
flame-space); the mothership "advances to" the flame-space directly.
No intermediate effects.

**[POLICY]** If `flame-space = nil` (should not happen mid-turn under
normal flow), skip this transition.

### §4.10 Pass-flame

```
:pass-flame
    single choice: [:begin-next-turn]
```

1. `:turn += 1`.
2. `:flame := next-in-turn-order(current-player)`.
3. If new flame-holder has `mothership ≠ :supply`, `:flame-space :=
   (front-space (mothership new-flame-holder))`. Else `:flame-space := nil`.
4. Move discards: `:discard := into :discard (:hands[old-flame-holder])`;
   clear `:hands[old-flame-holder]`.
5. Reset `:exhausted-colors` (part of prior turn's phase-data — do NOT
   carry over).
6. Any exhausted components remain exhausted across turn boundaries;
   they only un-exhaust via outer sun activation.
7. Transition to `:place-mothership` or `:resolve-mothership` per §4.2.

### §4.11 Game-over

```
:game-over
    single choice: [:end]                 (terminal; loops on itself in UI)
```

Reached when `:flares-drawn` hits 13 (during `:drawing-cards`). Compute
final scores per §6.4 and salvation per §6.5. Set `:winner`.

### §4.12 Transition table (compact)

```
:place-mothership     → :choose-action-type
:resolve-mothership   → :choose-action-type
:choose-action-type   → :moving | :activating
:moving               → :moving | :on-planet-decisions
:activating           → :activating | :on-planet-decisions
:activating-sun-space → :activating
:activating-planet-space → :activating
:link-placement       → :link-placement | :owner-bonus-decision | :activator-bonus-decision | :activating
:owner-bonus-decision → :link-placement | :activator-bonus-decision
:activator-bonus-decision → :link-placement | :activating
:on-planet-decisions  → :drawing-cards
:drawing-cards        → :drawing-cards | :orbit-planets | :game-over
:orbit-planets        → :advance-mothership
:advance-mothership   → :pass-flame
:pass-flame           → :place-mothership | :resolve-mothership | :game-over
```

---

## §5 · Choice enumeration

Every phase's `legal-actions` returns a map `{choice-key → next-state}`.
Choice keys are vectors with a keyword head. This section fixes the
canonical set. `→ P` denotes "transition to phase P".

### §5.1 `:place-mothership`

```
{[:place-mothership sid] → :choose-action-type
   | sid ∈ beam-orbital-spaces}
```

### §5.2 `:resolve-mothership`

```
{[:shift-in]  → :choose-action-type    ; iff inner-ring(current-ring) exists
 [:shift-out] → :choose-action-type    ; iff outer-ring(current-ring) exists
 [:stay]      → :choose-action-type}   ; always
```

Where `current-ring` is `(second (:mothership current-player))` for
orbital, or `:sun` for a sun wedge. Sun's inner ring is nothing;
silver's outer ring is green; void's outer ring is nothing. **[POLICY]**
`[:shift-in]` from silver goes to the sun; `[:shift-out]` from sun goes to
silver.

### §5.3 `:choose-action-type`

```
{[:choose-move]     → :moving
 [:choose-activate] → :activating}     ; iff any activation target has ≥1 sundiver
```

### §5.4 `:moving`

`moves-left = n`, `used-any? = b`.

```
{[:launch dst]          → :moving     ; dst ∈ launch-targets(state) and habitat > 0 and n > 0
 [:fly src dst]         → :moving     ; src has ≥1 player sundiver, dst adj to src, n > 0
 [:path src dst]        → :moving     ; per §5.4.1, n > 0
 [:done-moving]         → :on-planet-decisions}
```

#### §5.4.1 Path travel enumeration

`[:path src dst]` is legal iff there is a subset `L ⊆ :links` such that:

- All links in `L` share the same `:owner` (call them the *chain owner*).
- All links in `L` share the same `:color` (**[POLICY]** — rulebook says
  "same player color" not "same link color"; interpret as **same owner
  suffices; color can vary**; leaving the color constraint out).

  *Choose the second reading:* same owner, any color mix. This matches
  the rulebook's "path is a continuous set of links of the same player".

- `L` forms a connected subgraph containing both `src` and `dst`
  (endpoints in the union of link endpoints).
- `src` has ≥1 player sundiver.

Compute by BFS over the link-graph restricted to `owner = X` for each
distinct owner. Enumerate all `(src, dst)` pairs where src has a
sundiver, dst is reachable in owner-X's graph. Cost: 1 movement point
per hop of the traversal? Rulebook: "travel along a path to any space on
that same path". *[POLICY]* Cost = 1 movement point total (not per hop);
you fly the whole chain for one point. Landing energy: if chain owner ≠
current-player, chain-owner gains 1 energy.

### §5.5 `:on-planet-decisions`

```
{[:planet-on  ref] → :on-planet-decisions  ; ref = [sid sundiver-idx]
 [:planet-off ref] → :on-planet-decisions}
```

Auto-completes when `remaining` empty → `:drawing-cards`.

### §5.6 `:activating` (top level)

```
;; Initial (target unset):
{[:activate-sun]     → :activating       ; iff ∃ player sundiver on any [:sun k]
 [:activate-planets] → :activating       ; iff ∃ player sundiver on any planet-space
 [:activate-cities]  → :activating}      ; iff ∃ player sundiver on any city-space

;; Target set, activated-count = 0 (must do ≥ 1):
{[:activate-space sid] → :activating-<target>-space
 | sid ∈ remaining}

;; Target set, activated-count > 0:
{[:activate-space sid] → :activating-<target>-space   ; iff remaining non-empty
 [:done-activating]    → :drawing-cards}
```

### §5.7 `:activating-sun-space`

`current = [:sun k]`. Legal choices depend on this player's sundivers on
the wedge:

```
{[:sun-outer sundiver-idx]              → :activating   ; iff sundiver has no resource
                                                        ;  OR non-matching resource
 [:sun-inner sundiver-idx]              → :activating   ; iff sundiver has matching resource
                                                        ;  AND components-in-reserve > 0}
```

Effect of `[:sun-outer sundiver-idx]`:

- Remove sundiver from `[:sun k]`.
- Add to `habitat(cp)`.
- If sundiver had a resource `res`, return it to `:market-resources[res]`
  (capped at 5; **[POLICY]** overflow is discarded).
- Gain energy: `2 + 1 × active-count(k, cp) + 2 × exhausted-count(k, cp)`.
- Move all exhausted components (any owner) on `[:sun k]` to active.
- `cards-owed += 1`.

Effect of `[:sun-inner sundiver-idx]`:

- Remove sundiver from `[:sun k]`. Add to `reserve(cp)`.
- Return sundiver's resource (which must match `wedge-color[k]`) to
  reserve of `cp` **[POLICY]** — rulebook says "Return the resource and
  activating sundiver to reserve"; "reserve" is otherwise not a resource
  container. Interpret as: **return the resource to the market row of
  its color** (capped at 5, overflow discarded).

  Cleaner alternative: rulebook literally means "to the general supply"
  and doesn't matter mechanically. Either reading yields identical
  behavior if we treat market as capped at 5 with overflow discarded.

- Decrement `components(cp)` and add one component to
  `:solar-network[k][:active][cp]`.
- `cards-owed += 1`.

### §5.8 `:activating-planet-space`

`current = [:orbit r n]` where `:planets[r] = current`. Legal:

```
{[:planet-buy   sundiver-idx]                → :activating
   ; iff sundiver at current has no resource
   ;   AND :market-resources[r] ≥ 1
   ;   AND energy(cp) ≥ resource-price-by-stock[:market-resources[r]]

 [:planet-build sundiver-idx resource-color] → :activating
   ; iff sundiver at current has resource resource-color ≠ r
   ;   AND :market-cities[resource-color] < cities-per-color
   ;   AND (cities[current] = nil)
   ;   AND :market-cities[r] = 0     ; "no city of that color in this ring already"
   ;                                 ; but the rulebook is ambiguous — see [POLICY]
   ;   AND city-platforms(cp) ≥ 1
   ;   AND components(cp) ≥ 0}
```

**[POLICY]** "if there is no city of that color in this ring already" —
disambiguate: no city of the **city color** (`resource-color`) in *this*
ring (`r`). Enforced by checking `(∀ sid ∈ orbit-spaces(r).
(cities[sid].:color ≠ resource-color))`.

Effect of `[:planet-buy sundiver-idx]`:

- `energy(cp) -= resource-price-by-stock[:market-resources[r]]` (paid to
  pool).
- `:market-resources[r] -= 1`.
- Set sundiver's `:resource` to `r`.
- `cards-owed += 1`.

Effect of `[:planet-build sundiver-idx resource-color]`:

- Remove sundiver from `current`. `vaporized(cp) += 1`.
- `:market-cities[r] += 1` **[POLICY]** — the rulebook says "Place its
  resource on the market row corresponding to the ring of the planet on
  the city side"; interpret the "city side" increment as a
  level-tracker for the *ring* (not the resource color).
- Decrement `city-platforms(cp)`. Increment `platforms-on-board` (see
  §3.2 accounting).
- Place city at `current` with `:owner = cp, :color = resource-color`.
- Refill `:market-resources[r] := min(5, current + (5 - current))` — i.e.,
  set to 5 **[POLICY]** — "Replenish the resources of that row up to 5".
- `cards-owed += 1`.

### §5.9 `:link-placement`

`phase-data = {:actor pk, :actions-left n, :is-bonus? bool,
:parent-activation {...}}`.

The parent activation's phase-data must be carried because on
`[:done-linking]` we return to it. `:exhausted-colors` from the parent
survives.

```
{[:link src dst color]  → :link-placement       ; per §5.9.1, actions-left -= 1
 [:done-linking]        → §5.9.2}
```

#### §5.9.1 Link legality

`[:link src dst color]` is legal iff **all** of:

1. `actor` has ≥1 unplaced link (`links-supply(actor) ≥ 1`).
2. `actor` has ≥1 energy (spent to pool).
3. `dst` is adjacent to `src`.
4. No existing link between `src` and `dst` (any player, any color).
5. `src` is a valid start:
   - `src` is a sun wedge, OR
   - `src` is a city space, OR
   - `src` is an endpoint of an existing link that traces (via same-owner
     chain) to a city.
6. `src` is **not saturated for actor**: fewer than 2 existing actor
   links touch `src`.
7. `dst` is **not saturated for actor**: fewer than 2 existing actor
   links touch `dst`.
8. `color` matches the color of `src` in the sense of "you are linking
   *out of* `src`" (§5.9.3).
9. Actor has color-access for `color` this turn (§5.9.3).

Effect:

- `energy(actor) -= 1` → pool.
- Add `{:a src :b dst :owner actor :color color}` to `:links`.
- `links-supply(actor) -= 1`.
- **First exhaust of `color` this turn:** if `color ∉ exhausted-colors`,
  find an *active component* of `color` on the sun (any wedge) and
  exhaust it (see §5.9.4). Add `color` to `exhausted-colors`.
- If exhaust owner ≠ actor: `energy(exhaust-owner) += 1` (from pool).
- `actions-left -= 1`.

#### §5.9.2 `[:done-linking]` transition

If `is-bonus?` true → transition to `:activating (target set, activated-count > 0)`.

If `is-bonus?` false (base actions just finished):

- Let `city-owner = (cities[activation-space]).:owner`.
- If `city-owner ≠ actor`: transition to `:owner-bonus-decision`.
- Else: `actor` may take the bonus without asking. Transition to
  `:activator-bonus-decision` with **owner and activator collapsed** —
  actor picks `[:take-bonus]` or `[:decline-bonus]`.

#### §5.9.3 Color access

`src` color:

- If `src = [:sun k]`: color = `wedge-color[k]`.
- If `src` is a city space: color = `(cities[src]).:color`.
- If `src` is a link-connected chain-to-city space: color = color of the
  *outgoing link* being placed; **[POLICY]** = color of the city the
  chain leads to.

"Linking out of `src` requires an exhausted component of `color` from
this turn." Specifically:

- If `color ∈ exhausted-colors`: no exhaust required. Anyone can reuse
  this color for further links this turn.
- Else: the actor must find an *active* component of `color` on *any*
  sun wedge (`solar-network[k][:active][actor] > 0` where `wedge-color[k]
  = color`), and exhaust it (§5.9.4). Add `color` to `exhausted-colors`.
  If actor has no matching-color active component: `[:link _ _ color]`
  is not legal.

**[POLICY]** — rulebook says "If you exhaust a component of another
player, they receive one energy for every link you create using that
color." — this permits exhausting *any player's* active matching-color
component if the actor has none of their own. But it triggers an energy
transfer.

Refinement: If actor has an active matching-color component of their own,
prefer that (no transfer). Else exhaust any active matching-color
component of any player; that player gains 1 energy for every link
placed this turn that uses this color. Track this bookkeeping in
`phase-data :exhaust-owners {color → PK}` for post-hoc payout at
end-of-turn — **or** pay 1 energy on each link (equivalent for a
single-color chain).

**[POLICY]** simplify: **pay on each link**. Every `[:link _ _ color]`
placed while `color ∈ exhausted-colors AND (exhaust-owner ≠ actor)`
transfers 1 energy from actor (or pool) to the exhaust-owner.

#### §5.9.4 Choosing which component to exhaust

Preference order for exhausting a matching-color active component:

1. If actor has an active matching-color component (in any wedge), exhaust
   the actor's own.
2. Else exhaust any player's active matching-color component (chosen
   deterministically — **[POLICY]** by iteration order over players in
   `:turn-order`).
3. Else: link is not legal (already covered above).

### §5.10 `:owner-bonus-decision`

`choice-player = :owner`. Legal:

```
{[:take-bonus]    → :link-placement (actor := owner, actions-left := bonus, is-bonus? := true)
 [:decline-bonus] → :activator-bonus-decision}
```

If `activator = owner` (self-owned city or unowned), this phase is
skipped — see §5.9.2.

### §5.11 `:activator-bonus-decision`

`choice-player = :activator = current-player`.

```
{[:take-bonus]    → :link-placement (actor := activator, actions-left := bonus, is-bonus? := true)
 [:decline-bonus] → :activating (target set, activated-count > 0)}
```

### §5.12 `:drawing-cards`, `:orbit-planets`, `:advance-mothership`, `:pass-flame`

Each exposes exactly one legal choice-key. Eligible for auto-advance.

```
:drawing-cards           {[:draw-next]        → :drawing-cards | :orbit-planets | :game-over}
:orbit-planets           {[:orbit-resolved]   → :advance-mothership}
:advance-mothership      {[:advance-resolved] → :pass-flame}
:pass-flame              {[:begin-next-turn]  → :place-mothership | :resolve-mothership | :game-over}
:game-over               {[:end]              → :game-over}          ; terminal fixed point
```

---

## §6 · Edge-case policy

### §6.1 Empty deck

**[POLICY]** No reshuffle. If a draw is required and `:deck = []`, the
draw is a no-op. `cards-drawn += 1` still advances the counter (so we
don't loop forever). Any subsequent card-effect-based orbit uses the
**last non-nil card in :hands[cp]** for orbit resolution; if
`:hands[cp]` is empty (all draws were empty), `:orbit-planets` is a
no-op.

### §6.2 No legal actions

Guaranteed non-empty for every phase:

- `:resolve-mothership`: `[:stay]` is always legal.
- `:choose-action-type`: `[:choose-move]` is always legal.
- `:moving`: `[:done-moving]` is always legal.
- `:activating` (before target chosen): if no target has ≥1 sundiver,
  **[POLICY]** the whole `[:choose-activate]` from §5.3 was suppressed;
  we can't reach `:activating` in that case.
- `:activating` (target chosen, activated-count = 0): if `remaining =
  ∅`, that's a contradiction — the target selection guaranteed ≥1 space.
  **[POLICY]** if this state is reached anyway (e.g., mid-turn state
  change ate the last sundiver), emit `[:no-activation-possible]` that
  jumps to `:on-planet-decisions` with `cards-owed = 0`.
- `:link-placement`: `[:done-linking]` is always legal.
- All automatic phases: single legal choice.

### §6.3 13th flare

`:flares-drawn = 13` transitions immediately (mid-`:drawing-cards`) to
`:game-over`. No further draws, no orbit, no mothership advance, no
flame pass. The card that brought the count to 13 counts as drawn — its
flare-pull effect is applied *before* the terminate.

### §6.4 Scoring

At `:game-over`:

Score each player's **valid connections**.

A **connection** is a maximal path in the link graph, restricted to one
player's links (all links owned by that player), that includes at least
one endpoint on a sun wedge OR terminates on a city that itself
transitively (through other same-player connections) reaches the sun.

Formal:

```
sun-anchored? (link-graph P, sid) =
  (∃ path in link-graph(P) from sid to some [:sun k])

city-anchored? (cities, sid) = (sid ∈ keys(cities))

reachable-sun-anchored? (link-graph P, sid, cities) =
  sun-anchored?(link-graph(P), sid)
  ∨ (city-anchored?(cities, sid) ∧ (∃ other city X. connection between sid & X exists
                                     ∧ reachable-sun-anchored? X))

Valid connection = an edge-set L' ⊆ links(P):
  connected subgraph, containing (a) a sun wedge endpoint and one other endpoint OR
                                 (b) two endpoints, each of which is a city that's
                                     sun-anchored via a chain of P's links.
Both endpoints are valid per: sid endpoints validity —

endpoint sid is VALID for P iff:
  - sid is a city on a platform of color P (city.:owner = P), OR
  - sid is a city of color c AND P has a solar component of color c
    (active or exhausted count > 0 in wedge(c)), OR
  - sid is a sun wedge AND P has a component in that wedge.
```

Score = **count of valid connections** created by P.

Bonus: for each connection endpoint that is valid because it's *another
player's city* of color matching one of P's components, `+1` per component
beyond 1 in that color (i.e., `+max(0, active(w)+exhausted(w) − 1)` where
`w = wedge-of(matching-color)`).

**[POLICY]** "beyond 1" counts both active and exhausted components.

### §6.5 Salvation

Compute:

- `equal-points?`      = `(apply = (vals (:scores game-over-state)))`
- `equal-components?`  = `(apply = (map (fn [p] (total-components-in-play state p)) turn-order))`
- `equal-cities?`      = `(apply = (map (fn [p] (count (filter #(= p (:owner %)) (vals (:cities state))))) turn-order))`

Where `total-components-in-play(state, p) = solar-active(p) +
solar-exhausted(p)` (reserve components in personal supply don't count
— "unrealized").

If all three: `:winner = :salvation`.

Else apply the tie cascade:

```
sorted-scores = distinct desc-sorted scores
for tier in sorted-scores:
  tier-players = players with this score
  if (count tier-players) = 1:
     :winner = {:result :win, :winner (first tier-players), :scores scores}
     return
otherwise (all tiers were ties):
     :winner = {:result :none, :scores scores}
```

### §6.6 Owner-bonus multi-player decision

`:owner-bonus-decision` and `:activator-bonus-decision` change the
**choice-player** *within a single turn*. The choice-player is stored
in `phase-data` (for `:owner-bonus-decision`, `choice-player = :owner`).

For UI: `current-player(state)` returns the *turn-holder* (the flame
player). `choice-player(state)` returns the flame player OR the phase-data
override, if any. Bots need `choice-player`, so live-play bot code should
compute it and step whichever player is bot.

For server-side coordination with disconnected owners:

**[POLICY]** owners who don't respond within `30 s` auto-decline. Bots
resolve immediately (no timeout).

### §6.7 Multiple sundivers per space

Sundivers are individuated within `:sundivers[sid]` as a *vector* (order
matters — index is stable identity within a turn). Sundiver identity
across turns is not preserved (server doesn't need to track long-lived
sundiver ids).

### §6.8 Path-travel over city intermediates

Path travel can pass through a city space en route to a further space on
the same chain. The path-travel choice specifies `src` and `dst`; if the
chain traverses cities, that's fine. No activation triggers on
transit.

### §6.9 Non-matching resource returned

When a sundiver returns to habitat (outer-sun, city activation) while
carrying a resource, the resource returns to `:market-resources` capped
at 5. Overflow: **[POLICY]** discarded.

### §6.10 Zero-move pass

`[:done-moving]` with `moves-left = 5` is a valid pass. `cards-owed = 1`
still.

### §6.11 Player has no habitat & no launch target

If `habitat(cp) = 0` OR `launch-targets(state, cp) = ∅`, `[:launch _]`
is not legal. Fly/path/done-moving still available; `[:done-moving]`
covers the "no-op turn" fallback.

### §6.12 First-turn placement + flame

First player's first turn:

- `flame-space` starts as `nil`.
- On `[:place-mothership space]`, `flame-space := (front-space space)`.
- Turn proceeds normally.

Subsequent players' first turn: same shape (mothership was `:supply`),
but `flame-space` was set to `nil` in the previous `:pass-flame` (since
the new flame-holder had no mothership). Placement establishes it.

### §6.13 Voluntary shift-in from silver

**[POLICY]** Allowed. Mothership can move onto `[:sun k]` via
`[:shift-in]` from silver. Sun wedge stays valid until a subsequent
`[:shift-out]` moves it back to silver. Flame moves to the space in
front of the sun wedge (which is `[:sun ((k-1) mod 5)]`).

Subsequent shifts from a sun wedge: `[:shift-in]` is not legal (no ring
inside sun). `[:shift-out]` legal, back to silver.

Flare pull with mothership already on sun: no effect (§4.7.1).

---

## §7 · First-turn trace

Three-player game — **Sola** (silver seat), **Vega** (green), **Lyra**
(blue). Deck seed placeholder; orbital dice roll = **27** (d4=2, d10=7).

### §7.0 Setup delta

```
:motherships    → {Sola :supply, Vega :supply, Lyra :supply}
:sundivers      → all sids empty
:solar-network  → wedges 0..4 empty except:
                   wedge 0 (silver) :active {Sola 1}
                   wedge 1 (green)  :active {Vega 1}
                   wedge 2 (blue)   :active {Lyra 1}
:players        → each has habitat=8, reserve=5, energy=5, components=7,
                   city-platforms=5, links-supply=13, vaporized=0
:market-resources → {silver 1, green 2, blue 3, purple 4, void 5}
:market-cities  → {silver 0, green 0, blue 0, purple 0, void 0}
:energy-pool    → 89 - 15 = 74
:planets        → {silver [:orbit :silver 2]  ; 27 mod 5 = 2
                    green  [:orbit :green 3]   ; 27 mod 8 = 3
                    blue   [:orbit :blue 1]    ; 27 mod 13 = 1
                    purple [:orbit :purple 6]  ; 27 mod 21 = 6
                    void   [:orbit :void 27]}  ; 27 mod 34 = 27
:flame          → Sola
:flame-space    → nil
:phase          → :place-mothership
:phase-data     → {}
:turn           → 0
:deck           → shuffled 78 cards
:hands          → all empty
:discard        → []
:flares-drawn   → 0
```

### §7.1 Turn 1 — Sola

**`:place-mothership`** — Sola picks `[:place-mothership [:orbit :silver 0]]`.
```
motherships[Sola] := [:orbit :silver 0]
flame-space       := [:orbit :silver 4]      ; (front-space [:orbit :silver 0])
                                             ; = [:orbit :silver ((0-1) mod 5)] = 4
→ :choose-action-type
```

**`:choose-action-type`** — `[:choose-activate]` illegal (no sundivers on
board). Sola picks `[:choose-move]`.
```
phase-data := {:moves-left 5, :used-any? false}
→ :moving
```

**`:moving`** — Sola picks `[:launch [:orbit :silver 0]]` (space of
mothership).
```
habitat(Sola)                       :  8 → 7
sundivers[[:orbit :silver 0]] += {:owner Sola, :resource nil, :on-planet? false}
moves-left                          : 5 → 4
```

Sola picks `[:launch [:orbit :silver 4]]` (in front of mothership).
```
habitat(Sola)                       :  7 → 6
sundivers[[:orbit :silver 4]] += {:owner Sola, :resource nil, :on-planet? false}
moves-left                          : 4 → 3
```

Sola picks `[:done-moving]`.
```
→ :on-planet-decisions
```

**`:on-planet-decisions`** — no player sundivers on planet-spaces.
Silver planet at `[:orbit :silver 2]`, Sola has no sundivers there.
Auto-complete.
```
→ :drawing-cards, cards-owed := 1
```

**`:drawing-cards`** — draw top card. Say it's `{:suit :blue, :value 4}`.
```
hands[Sola] += that card
:deck top popped
→ (cards-drawn = 1 = cards-owed) :orbit-planets, last-card = :blue
```

**`:orbit-planets`** — blue planet orbits back 1.
```
:planets[:blue] : [:orbit :blue 1] → [:orbit :blue 2]
→ :advance-mothership
```

**`:advance-mothership`** — Sola's mothership → flame-space.
```
motherships[Sola] := [:orbit :silver 4]
→ :pass-flame
```

**`:pass-flame`** — discard Sola's hand into main discard, hand
`[{:suit :blue}]` → discard.
```
turn                    : 0 → 1
flame                   : Sola → Vega
flame-space             : nil    ; Vega's mothership still :supply
discard                 += [{:suit :blue}]
hands[Sola]             := []
→ :place-mothership (Vega's first turn)
```

### §7.2 Turn 2 — Vega

**`:place-mothership`** — Vega picks `[:place-mothership [:orbit :green 0]]`.
```
motherships[Vega] := [:orbit :green 0]
flame-space       := (front-space [:orbit :green 0]) = [:orbit :green 7]
→ :choose-action-type
```

**`:choose-action-type`** — Vega has no on-board sundivers. Picks
`[:choose-move]`.

**`:moving`** — Vega picks `[:launch [:orbit :green 0]]`, then
`[:launch [:orbit :silver 4]]` (front-of-adjacent-inner: from green 0 the
inner ring is silver; frontmost-adjacent-silver from green 0 is silver 4
per angular overlap and CCW distance).

Wait — this is a good example of the tricky bit. Green 0 is at angular
range `[0, 1/8]`. Silver 4 is at `[4/5, 5/5]`. These do NOT overlap. So
silver 4 is NOT adjacent to green 0.

Silver spaces adjacent to green 0: silver 0 (`[0, 1/5]`) — overlap
`[0, 1/8]`. Only silver 0.

So the "front-of-adjacent-inner" from green 0 is silver 0 (only one
adjacent silver). But this is *not* front-of-Vega's-mothership. It's the
frontmost silver space adjacent to Vega's mothership at green 0.

`launch-targets(state, Vega)`:
- Space of mothership: [:orbit :green 0]
- Front-space: [:orbit :green 7]
- Frontmost-inner: [:orbit :silver 0]
- Frontmost-outer: [:orbit :blue 0]  (green 0 adjacent to blue 0)

Vega picks `[:launch [:orbit :green 7]]` (front-of-mothership).
```
habitat(Vega) : 8 → 7
sundivers[[:orbit :green 7]] += Vega
moves-left    : 5 → 4
```

Vega picks `[:launch [:orbit :silver 0]]` (front-of-adjacent-inner).
```
habitat(Vega) : 7 → 6
sundivers[[:orbit :silver 0]] += Vega
moves-left    : 4 → 3
```

Vega picks `[:done-moving]`.

**`:drawing-cards`** — draw. Say `{:suit :silver}`.

**`:orbit-planets`** — silver planet orbits back 1: `[:orbit :silver 2]
→ [:orbit :silver 3]`.

**`:advance-mothership`** — motherships[Vega] := [:orbit :green 7].

**`:pass-flame`** — Lyra becomes flame; flame-space := nil.

### §7.3 Turn 3 — Lyra

Similar shape — places on `[:orbit :blue 0]`, launches, moves, draws.
Elided for space.

### §7.4 Turn 4 — Sola (2nd turn)

**`:resolve-mothership`** — Sola's mothership at `[:orbit :silver 4]`.
Legal: `[:shift-in]`, `[:shift-out]`, `[:stay]`.

Sola picks `[:shift-out]`:
```
Green space adjacent to silver 4? Silver 4 angular range [4/5, 1] = [.8, 1].
Green space 6 = [6/8, 7/8] = [.75, .875] — overlaps [4/5, .875].
Green space 7 = [7/8, 1] = [.875, 1] — overlaps [.875, 1].
Two green candidates. Frontmost (smallest CCW distance from silver 4 mid = .9):
   green 7 mid = .9375 (CCW distance from .9 = .0375)
   green 6 mid = .8125 (CCW distance from .9 = .9125)
   → green 7 is frontmost.

motherships[Sola] := [:orbit :green 7]
flame-space       := (front-space [:orbit :green 7]) = [:orbit :green 6]
                     ; (7 - 1) mod 8 = 6
→ :choose-action-type
```

**`:choose-action-type`** — Sola picks `[:choose-activate]`. Sola has a
sundiver on `[:orbit :silver 0]`, which is a *planet space* (silver
planet). And on `[:orbit :silver 4]` (currently, before advance —
actually, after the previous turn's advance, Sola's mothership is on
`[:orbit :silver 4]`; her sundiver is on `[:orbit :silver 4]` too (yes,
from the launch on turn 1). Silver planet was at `[:orbit :silver 3]`
after turn 2 — not with Sola's sundivers.

So Sola's activation targets:
- Sun: no sundiver.
- Planets: silver planet at [:orbit :silver 3]. Sola has no sundiver
  there. → not available.
- Cities: no cities exist yet. → not available.

`[:choose-activate]` is illegal. Sola must `[:choose-move]`.

**`:moving`** — Sola flies (say) her sundiver from `[:orbit :silver 4]`
to `[:orbit :green 6]` (adjacent, cross-ring). `[:fly [:orbit :silver 4]
[:orbit :green 6]]`.
```
sundivers[[:orbit :silver 4]] -= Sola-sundiver
sundivers[[:orbit :green 6]] += that sundiver
moves-left : 5 → 4
```

Sola picks `[:done-moving]`.

*(further turns not detailed)*

### §7.5 What the trace demonstrates

- Placement, launch, fly, done-moving are exercised.
- Front-of-mothership computation across a ring change (silver 4 → green 7)
  demonstrates §2.8.
- Front-of-adjacent-inner (green 0 → silver 0, not silver 4) demonstrates
  §2.5 angular overlap correctly.
- Empty-hand pass at end of turn discards.
- Planet orbit resolves *before* mothership advance.
- Choose-activate legality gate (§6.2) suppresses illegal target selection.

---

## §8 · UI panel inventory

Single-screen layout: board on the left, side panel on the right.
Everything monospace, dark theme (`#06070d` background, `#ccbbee`
foreground, `#ff8844` accent — matches the existing `future/play.cljs`).

### §8.1 Board (SVG)

- Concentric circular rings for sun + 5 orbits; radii per
  `board/orbit-radii` (silver innermost outward).
- Sun wedges: 5 triangle+arc segments. Inner triangle in wedge color;
  outer arc red. Space-halves are visually distinct but click-through
  to the same space id.
- Beam: bright yellow radial line at angle 0 (top of screen).
- Planets: filled circle at planet-space's centroid, colored by ring
  color, with a subtle glow.
- Motherships: arrow-glyph at mothership's space, pointing CCW.
  Player-colored fill (silver / green / blue / purple / void per
  wedge-color).
- Flame-of-Justice: small flame icon at `:flame-space`.
- Sundivers: circular tokens (color = owner). If carrying a resource,
  center dot of resource color. If `:on-planet?`, overlapping planet
  glyph.
- Cities: pentagonal glyph, color = city color, base outline = platform
  owner color.
- Links: thick line segments between space-centroids, colored by
  `:color`; dashed outline colored by `:owner`.
- Hover: highlight adjacent spaces + legal actions from hovered space.
- Click: submits action if unambiguous, else opens disambiguation
  popover.

### §8.2 Sidepanel — subpanels top-to-bottom

**Status** — always visible:
- Current phase (colored by phase).
- Current player + wedge color swatch.
- If bot: "BOT" badge.
- Sub-status by phase:
  - `:moving`: "MOVES LEFT: n" and per-move choice hints.
  - `:activating`: "TARGET: sun/planets/cities", "ACTIVATED: n",
    "CARDS OWED: n".
  - `:link-placement`: "ACTIONS LEFT: n" and "COLOR: c".
  - `:drawing-cards`: "DRAW n / N".
- Flare counter: "FLARES: 3/13".
- Turn counter.

**Players** — one row per player:
- Name, wedge-color swatch, energy (E), habitat sundivers (H),
  reserve sundivers (R), components-in-supply (C), platforms (P),
  links-supply (L), on-board sundivers, vaporized (V).
- Components on sun: 5-cell mini-grid (one per wedge) showing active/exhausted counts.

**Market** — five rows (one per color):
- Color swatch, "RES: n/5", "CITY LVL: n/4",
  cost: `resource-price-by-stock[n]` (5,3,2,1,1).

**Solar** — five columns (one per wedge):
- Wedge color, list of active components with player counts,
  list of exhausted components with player counts.

**Supply** — global:
- Energy pool.
- Total remaining resources (cap × 5 rows) — implied by market panel;
  optional.
- Card counts by suit remaining in deck (approx — expensive to render
  precisely, so aggregate: "DECK: 62 / DISCARD: 16 / FLARE: 10/13").
- Cities remaining per color: `cities-per-color - :market-cities[c]`.

**Hand** — current player's drawn-but-unresolved cards:
- One row per card in `:hands[current-player]`, suit + value.

**Actions** — legal-actions rendered as buttons:
- Only enabled if this browser's session player is the choice-player.
- Labels via `action-label` (already in `future/play.cljs`) — extend to
  cover all §5 choice keys.
- Button colors: default gray, terminal choices dimmer, high-impact
  choices ([:planet-build], [:sun-inner]) accented.
- "WAITING ON <choice-player>" if not this browser's turn.

**Log** — right-side or expandable panel:
- Last N actions: `T<turn> <player> [<phase>] <action-label>`.

### §8.3 Non-play pages

- `/future/home` — landing.
- `/future/create` — lobby (already shipped via
  `organism.components/create-lobby`).
- `/future/play/:play` — game view (WebSocket).
- `/future/observe` — observer list.
- `/future/observe?live` — spectate mode of one game.
- `/future/generate` — local single-page bot simulator (no server).
- `/future/rules` — rulebook rendered as HTML.

Bot-holder pages, player stats pages, etc. are optional and not
required for a one-shot implementation.

---

## §9 · Bot policy

**Default bot policy: random-uniform among non-terminal legal choices,
falling back to random-uniform among all legal choices when only
terminals remain.**

Terminal choice-keys (drop-first-if-alternatives-exist):

```clojure
(def terminal-choice-keys
  #{[:done-moving] [:done-activating] [:done-linking]
    [:decline-bonus] [:no-activation-possible]})
```

Algorithm:

```clojure
(defn bot-pick [actions]
  (let [entries (vec actions)
        non-term (filterv (fn [[ck _]] (not (contains? terminal-choice-keys ck))) entries)
        pool    (if (seq non-term) non-term entries)]
    (rand-nth pool)))
```

Bot pacing (live play): 500 ms/step, via a `future` in
`organism.routes.future-ws/run-bot-turns!` that loops while
`(choice-player state)` is in the game's bot set. See existing
implementation for the pattern.

**[POLICY]** — this bot is a legality demonstrator, not a strategist.
It will lose to any human who has read the rules. Its purpose is:

1. Prove the state machine terminates (100% of games reach `:game-over`).
2. Cover every phase transition (random exploration hits all sub-phases
   over enough games).
3. Serve as the null bot for `/future/generate` local sim and for
   the "just fill the seat" bot in live play.

Sophistication (heuristic weighting, feat-based scoring, MCTS, GA-tuned
policies) is layered on top. See `journey/bots.cljs` and
`organism.persist-journey-bots` in this repo for the flow-DSL bot
pattern used by journey — it's transferable.

---

## Appendix A — Invariants (for tests)

Every state must satisfy:

- Sum of energies in play = 89.
- Every player's total components (reserve + solar active + solar
  exhausted) = 8.
- Every player's total sundivers (habitat + reserve + on-board +
  vaporized) = 13.
- Every player's total city-platforms (supply + placed) = 5.
- Every player's total links (supply + placed) = 13.
- `:market-resources` values in `0..5`.
- `:market-cities` values in `0..4`.
- At most one city per space.
- At most one link between any two spaces.
- `:flame ∈ :turn-order`.
- `:phase ∈` §4 enumerated phases.
- If `:phase = :game-over`, `:winner ≠ nil`.

## Appendix B — Serialization notes

The state is written via `pr-str` and read via `cljs.reader/read-string`
for websocket transport (see `future-ws.clj` and `play.cljs`
`safe-read`). Requirements:

- All keys in `:sundivers`, `:cities`, `:links` are vectors or sets that
  round-trip via `pr-str`/`read-string`.
- Cards, sundivers, players are plain maps.
- No functions or non-EDN values ever appear in state.

## Appendix C — What's *not* in this document (deferred)

- Sound design.
- Animations (no card, orbit, or activation animation specified — the
  reference impl can add fade/tween as appropriate).
- Card art, board art beyond the geometric layout.
- Multi-player persistence beyond the in-memory `games` atom.
- Replay / undo (available in `journey_ws.clj` but not required for
  future v1).
- Achievements, statistics, tournaments.
- Bot flowchart DSL (see `journey/bot-flow.clj`) — future v1 can ship
  with only the random bot.
- AI opponent stronger than random.

—

*End of supplement.*
