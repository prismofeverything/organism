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

- **Orbital rings**: index `0` is **centered on the beam** — i.e., the
  beam's radial line passes through the *middle* of every orbit's
  starting space, not through a boundary between two spaces. Consequently
  the starting spaces of all five orbits (silver 0, green 0, blue 0,
  purple 0, void 0) are mutually radially aligned, and each is adjacent
  to the spaces on either side of it in the next ring out (see §2.5).
  Index increases **CW in screen-space** (per rendering convention).
  "Forward" (CCW) therefore means index `- 1 mod size`.
- **Sun wedges**: wedges are **offset by half a wedge width** relative
  to the beam — the beam does *not* pass through the middle of any sun
  wedge; it passes along the boundary between two wedges. This offset is
  what makes every sun wedge adjacent to two silver spaces (and vice
  versa). Index increases **CW in screen-space**. "Forward" (CCW) means
  index `- 1 mod 5`.

Rationale: rulebook v2 fixes the sun wedge colors going **CW from silver**:
`silver → green → blue → purple → void`. Combined with the offset above,
wedge `0` is the *silver* wedge and sits just **CCW of the beam** (its
angular midpoint is at position `-1/10` = `9/10` on the unit circle),
matching the board image `beginning-board-update.png`.

```clojure
(def wedge-color                 ; canonical, per rulebook v2 + offset
  {0 :silver, 1 :green, 2 :blue, 3 :purple, 4 :void})
(def color->wedge (into {} (map (fn [[k v]] [v k]) wedge-color)))
```

**[POLICY]** The existing `board.cljc` in this branch had the wedges going
the opposite direction (`{0 :silver, 1 :void, 2 :purple, 3 :blue, 4 :green}`);
that mapping was inferred from rulebook v1's CCW placement text. Rulebook v2
supersedes it — regenerate. The existing `board.cljc` also treated wedge 0
as centered on the beam and used 1:1 sun↔silver adjacency; both of those
are superseded here as well.

### §2.4 Angular geometry

All rings share a common origin: the beam at angular position `0` (top of
screen). CW is angle-positive, CCW is angle-negative. Angular positions
are stored as **fractions of a full turn** (`0..1`) rather than degrees or
radians, avoiding float drift.

Every space is a wedge with a **center** and a **half-width** on the
unit circle. This representation is preferred over `[start end)` because
several ranges wrap through `0` (the beam) and half-width form avoids
that discontinuity.

```clojure
(defn angular-center+half [sid]
  ;; Returns [center half-width] as fractions of a full turn.
  ;; center is on the unit circle mod 1; distance is computed circularly.
  (case (first sid)
    :orbit (let [[_ r n] sid, size (ring-sizes r)]
             ;; Orbital index i is centered at i/size; index 0 sits on the beam.
             [(mod (/ n size) 1.0) (/ 0.5 size)])
    :sun   (let [[_ k] sid]
             ;; Sun wedge k is centered at (k - 0.5)/5 mod 1 — offset by
             ;; half a wedge CCW from the beam. Wedge 0 (silver) has center
             ;; -1/10 ≡ 9/10.
             [(mod (/ (- k 0.5) 5) 1.0) 0.1])))

(defn circular-distance [a b]
  ;; Shortest CW/CCW distance on the unit circle, in [0, 0.5].
  (let [d (mod (- a b) 1.0)]
    (min d (- 1.0 d))))

(defn angular-overlap
  ;; Amount by which two spaces overlap on the unit circle, in [-0.5, half-a+half-b].
  ;; Positive = overlap; zero = touching at a single boundary; negative = gap.
  [sid-a sid-b]
  (let [[ca ha] (angular-center+half sid-a)
        [cb hb] (angular-center+half sid-b)]
    (- (+ ha hb) (circular-distance ca cb))))
```

For rendering (§8.1), a plain `[start end)` may still be handy; derive it
from center ± half, taking care to wrap.

### §2.5 Adjacency

Two spaces are adjacent iff either:

1. **In-ring**, same ring, indices differ by ±1 mod size. Applies to the
   sun too (sun wedges *are* adjacent to their in-ring neighbors, contra
   the older interpretation).
2. **Cross-ring**, rings are ring-adjacent (see §2.1 `ring-adjacency`),
   and their angular overlap exceeds a minimum threshold `min-overlap`
   (see §2.5.1). This applies uniformly to sun↔silver as well as
   between orbital rings — sun↔silver is no longer a special-cased 1:1
   table.

#### §2.5.1 Minimum-overlap threshold

Because the orbits are all centered on the beam and the sun is offset by
half a wedge, cross-ring space boundaries frequently coincide *exactly*.
Two spaces whose boundaries meet at a single point have `angular-overlap
= 0` and should **not** count as adjacent — they touch tangentially, not
substantively. Conversely, the on-board visual has been redrawn with
some intra-ring boundaries shifted off perfect even angles to make
inter-ring adjacencies visually unambiguous; the *canonical* adjacency
graph nevertheless comes from the mathematical center/half-width
representation of §2.4, not from the drawn geometry.

**[POLICY]** `min-overlap = 1/1000` (a tolerance both well above
floating-point noise and well below every intended overlap listed
below). Two ring-adjacent spaces are adjacent iff `angular-overlap sid-a
sid-b > min-overlap`.

The smallest intended overlap in the intended adjacency graph is between
purple 0 and void 1 (or void 33): `1/42 + 1/68 - 1/34 = 55/1428 - 42/1428
= 13/1428 ≈ 0.0091`. `min-overlap = 1/1000` therefore keeps all intended
adjacencies and rejects zero-width tangencies. If a smaller threshold is
needed later, raising `min-overlap` to `0.01` would prune the
purple↔void distant-neighbor pairs above — do not do this without
regenerating the test fixtures.

Consequences (worked out — good tests):

- **Sun ↔ Silver**: because the sun is offset by half a wedge and both
  rings have 5 spaces of width `1/5`, each sun wedge overlaps *two*
  silver spaces (`overlap = 0.1` each) and each silver space overlaps
  *two* sun wedges. Explicitly:
  - `[:sun 0]` (span `[.8, 1]`) ↔ `[:orbit :silver 4]` and `[:orbit :silver 0]`
  - `[:sun 1]` (span `[0, .2]`) ↔ `[:orbit :silver 0]` and `[:orbit :silver 1]`
  - `[:sun 2]` ↔ `[:orbit :silver 1]` and `[:orbit :silver 2]`
  - `[:sun 3]` ↔ `[:orbit :silver 2]` and `[:orbit :silver 3]`
  - `[:sun 4]` ↔ `[:orbit :silver 3]` and `[:orbit :silver 4]`
- **Silver (5) ↔ Green (8)**: each silver space overlaps 2 or 3 green
  spaces. Silver `0` (center `0`, half `.1`) overlaps green `7`, `0`,
  `1` (the beam-aligned "3-neighbor" case). Silver `1` (center `.2`)
  overlaps green `1` and `2` only.
- Similarly at each ring-ring boundary the "beam-aligned" (index-0)
  space of the inner ring overlaps *three* outer-ring spaces (the
  centered one plus one on either side), while off-beam inner-ring
  spaces overlap 1–2. See the invariant list in Appendix A.
- Every inter-ring adjacency is derived by the same overlap computation
  — there is no hand-maintained table.

Movement uses adjacency, and **[POLICY]** sundivers can enter the sun by
flying from a silver space to either of its two adjacent sun wedges.
This is the *only* route into the sun for sundivers.

### §2.6 Beam

The beam is a radial line at angular position `0` (top of screen). It
passes through the **middle** of exactly five spaces — the index-`0`
space of each orbit — and through **no** sun wedge (per the offset in
§2.3). Formally:

```clojure
(def beam-orbital-spaces
  [[:orbit :silver 0] [:orbit :green 0] [:orbit :blue 0]
   [:orbit :purple 0] [:orbit :void 0]])
```

These five spaces are pairwise radially aligned: `[:orbit :silver 0]` is
adjacent to `[:orbit :green 0]`, `[:orbit :green 0]` to `[:orbit :blue
0]`, etc., because each is fully contained (angularly) in the next.
Additionally, thanks to the centering, each of them is adjacent to *both*
neighbors of the next ring's beam-space in its outer ring (silver 0 ↔
green 7 and green 1; green 0 ↔ blue 12 and blue 1; …).

No sun wedge lies on the beam — the beam passes along the boundary
between `[:sun 0]` (silver, span `[.8, 1]`) and `[:sun 1]` (green, span
`[0, .2]`).

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
means smallest CCW distance from the source's angular center. Note that
a fully-aligned candidate (e.g., blue 0 from green 0) has CCW distance
`0` and always wins ties over any offset candidate.

```clojure
(defn angular-center [sid] (first (angular-center+half sid)))

(defn frontmost-adjacent-in-ring [adjacency src-sid target-ring]
  (let [candidates (filter #(and (= :orbit (first %)) (= target-ring (second %)))
                           (get adjacency src-sid #{}))
        src-c      (angular-center src-sid)]
    (first (sort-by
            (fn [c] (mod (- src-c (angular-center c)) 1.0))
            candidates))))
```

**[POLICY]** The mothership can *never* be on a sun wedge. Silver → Sun
shifting is disabled:

- `[:shift-in]` is not a legal choice when the mothership is on silver.
- The flare-pull from silver keeps the mothership on silver (§4.7.1),
  it does not push it into the sun.

The 5 sun wedges remain reachable for **sundivers** (via sundiver flight
from silver, §5.4), but the mothership token itself only occupies
orbital spaces.

Sun → Silver shift is therefore unreachable in normal play and is
omitted from the choice enumeration.

### §2.9 Planets

```clojure
:planets {:silver [:orbit :silver n], :green ..., :blue ..., :purple ..., :void ...}
```

Each planet lives at one space in its own color's ring. Planets orbit
**CCW = forward = index − 1** (same direction the mothership advances).
Orbit distance per color:

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
 ;; :flame-space is DERIVED, not stored: it is
 ;;   (front-space (mothership (:flame state))) when that mothership ≠ :supply,
 ;;   else nil. See §4.9 and §6.12.
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
 :resources         {sid → [color ...]}   ; free-standing resource tokens
                                          ; dropped on-board by sun activations
                                          ; (§6.9); empty/missing = none
 :cities            {sid → {:owner PK :color color}}
 :links             #{{:a sid :b sid :owner PK} ...}  ; color is derived
                                                     ; from endpoints, not stored
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
(def city-level-cards               {1 1, 2 2, 3 3})  ; cards drawn per
                                                      ; activation, by level
```

### §3.4 `:phase-data` shape by phase

Each phase's transient state (see §4 for the machine, §5 for choices):

- `:place-mothership`  `{}`
- `:resolve-mothership` `{}`
- `:choose-action-type` `{}`
- `:moving` `{:moves-left n, :used-any? bool}` — includes `[:planet-on]`
  / `[:planet-off]` choices (§4.4).
- `:activating` `{:target :sun|:planets|:cities, :remaining #{sid ...}, :cards-owed n, :activated-count n, :exhausted-colors #{color ...}, :exhaust-owners {color → PK}}`
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

On `[:place-mothership space]`: `motherships[cp] := space`. The
flame-space is derived (`= (front-space space)`), no explicit assignment.

On `[:shift-in]`: `motherships[cp] := frontmost-adjacent-in-ring in inner
ring`. Flame-space derived from the new mothership.

On `[:shift-out]`: analogous with outer ring.

On `[:stay]`: mothership unchanged. Flame-space (derived) is likewise
unchanged.

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
    ├── [:launch dst]           (dst ∈ launch-targets(state), habitat > 0, n > 0)
    ├── [:fly src dst]          (src has player sundiver, dst adjacent to src, n > 0)
    ├── [:path src dst]         (path-travel via link chain — see §5.4, n > 0)
    ├── [:planet-on  [sid idx]] (sundiver at [sid idx] is on a planet-space
                                 whose planet is currently there, and
                                 currently :on-planet? = false, n > 0)
    ├── [:planet-off [sid idx]] (same but currently :on-planet? = true, n > 0)
    └── [:done-moving]          (always → :drawing-cards)
```

- `launch-targets`: `{mothership-space, front-space(mothership-space),
  frontmost-adjacent-in-ring(inner), frontmost-adjacent-in-ring(outer)}` —
  up to 4 unique spaces.
- `[:launch dst]`: `habitat -= 1`, add sundiver `{owner, nil, false}` at
  dst.  `moves-left -= 1`.
- `[:fly src dst]`: pick *one* player sundiver at src, move to dst,
  preserving `:resource` and `:on-planet?` (but drop `:on-planet?` if
  leaving a planet space). `moves-left -= 1`.
- `[:path src dst]`: as `[:fly]`, but dst reachable via same-owner link
  chain; if chain owner ≠ current-player, chain-owner gains 1 energy.
  `moves-left -= 1`.
- `[:planet-on  [sid idx]]`: flip that sundiver's `:on-planet?` to `true`.
  Only legal on a space that currently *is* the planet-space for its ring
  (`:planets[(orbit-of sid)] = sid`). `moves-left -= 1`.
- `[:planet-off [sid idx]]`: flip that sundiver's `:on-planet?` to `false`.
  Same eligibility check as `:planet-on`. `moves-left -= 1`.
- `[:done-moving]`: transition to `:drawing-cards`. `moves-left` may be > 0.

**[POLICY]** Boarding / disembarking a planet costs 1 movement point
(rulebook update). The old separate `:on-planet-decisions` phase after
movement is removed — those decisions now happen *during* movement,
consuming move points like any other action. See §4.6 for the old-phase
note.

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

### §4.6 On-planet decisions (removed)

Previously a dedicated auto-phase between `:moving` and `:drawing-cards`.
Boarding/disembarking a planet is now a `:moving`-phase choice that costs
1 movement point (see §4.4). This section is retained for changelog
context only — do not implement `:on-planet-decisions` as a phase.

Sundivers that were on a planet whose planet has moved away are left
behind at their old space during `:orbit-planets` (see §4.8); that
bookkeeping is unchanged.

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
  - City space:   `city-level-cards[level]` — **level 1 = 1, level 2 = 2,
    level 3 = 3** (§3.3). This is *independent of* the base/bonus split
    (`city-level-actions`): base and bonus each have their own separate
    counts (e.g., level 3 = 3 base + 2 bonus = 5 total link actions), but
    the number of cards drawn is only a function of level.
  - **[POLICY]** cards-owed for a city is fixed at `city-level-cards[level]`
    regardless of whether the bonus was actually taken by anyone.

#### §4.7.1 Flare-pull mechanics

Flare pull: mothership one ring inward, along `ring-adjacency`.

- If mothership is on `[:orbit :silver …]`:
  - **[POLICY]** Stay on silver — the mothership never enters the sun
    (§6.13). Advance mothership 1 forward (CCW) in silver (index `- 1
    mod 5`).
  - `energy(cp) -= ⌈energy(cp) / 2⌉`; the paid energy returns to
    `:energy-pool`.
- If mothership is on any other orbital ring:
  - New position = `frontmost-adjacent-in-ring(adjacency, current, inner-ring)`.

Because the flame-of-justice is always the space in front of the current
player's mothership (see §3.1 / §4.9 — no independent `:flame-space`
field), a flare-pull that moves the mothership *implicitly* moves the
flame too. There is no "flame drift" to reason about.

### §4.8 Orbit-planets

```
:orbit-planets
    single choice: [:orbit-resolved]
```

Determined by the **last** card in `:hands[current-player]`:

- Flare: orbit *all* planets by `planet-advance-rate[color]` each.
- Suit color: orbit only that color's planet by
  `planet-advance-rate[color]`.

Orbit: new-index = `(old-index − rate) mod ring-size` (CCW = forward =
−index).

For each sundiver at the *old* planet-space with `:on-planet? true`:
move it (as a full sundiver record) to the new planet-space. Sundivers
`:on-planet? false` stay at the old space.

### §4.9 Advance-mothership

```
:advance-mothership
    single choice: [:advance-resolved]
```

Move current player's mothership one step forward (CCW): `motherships[cp]
:= (front-space motherships[cp])`. Since the flame is defined as the
space in front of the mothership, this is equivalent to "advance
mothership to flame-space" — and after the advance the flame is again
one step in front of the new mothership position. Always a single-step
advance; a flare-pull earlier this turn moved *both* the mothership and
(implicitly) the flame, so the advance still traverses exactly one space.

### §4.10 Pass-flame

```
:pass-flame
    single choice: [:begin-next-turn]
```

1. `:turn += 1`.
2. `:flame := next-in-turn-order(current-player)`.
3. (No explicit `:flame-space` assignment — it's derived from the new
   flame-holder's mothership, which will be `:supply` on first turn
   and a placed sid otherwise.)
4. Move discards: `:discard := into :discard (:hands[old-flame-holder])`;
   clear `:hands[old-flame-holder]`.
5. Reset `:exhausted-colors` and `:exhaust-owners` (part of prior
   turn's phase-data — do NOT carry over; the "first link of color X
   this turn" bookkeeping is per-turn).
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
:moving               → :moving | :drawing-cards
:activating           → :activating | :drawing-cards
:activating-sun-space → :activating
:activating-planet-space → :activating
:link-placement       → :link-placement | :owner-bonus-decision | :activator-bonus-decision | :activating
:owner-bonus-decision → :link-placement | :activator-bonus-decision
:activator-bonus-decision → :link-placement | :activating
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
                                       ; AND current-ring ≠ :silver (§2.8)
 [:shift-out] → :choose-action-type    ; iff outer-ring(current-ring) exists
 [:stay]      → :choose-action-type}   ; always
```

Where `current-ring` is `(second (:mothership current-player))`. Silver's
outer ring is green; void's outer ring is nothing. **[POLICY]** silver has
no inner-ring shift target — the mothership never enters the sun (§2.8),
so `[:shift-in]` is illegal from silver.

### §5.3 `:choose-action-type`

```
{[:choose-move]     → :moving
 [:choose-activate] → :activating}     ; iff any activation target has ≥1 sundiver
```

### §5.4 `:moving`

`moves-left = n`, `used-any? = b`.

```
{[:launch dst]              → :moving   ; dst ∈ launch-targets(state) and habitat > 0 and n > 0
 [:fly src dst]             → :moving   ; src has ≥1 player sundiver, dst adj to src, n > 0
 [:path src dst]            → :moving   ; per §5.4.1, n > 0
 [:planet-on  [sid idx]]    → :moving   ; sundiver at [sid idx] is on a current planet-space
                                        ;   with :on-planet? = false, n > 0
 [:planet-off [sid idx]]    → :moving   ; sundiver at [sid idx] is on a current planet-space
                                        ;   with :on-planet? = true, n > 0
 [:done-moving]             → :drawing-cards}
```

#### §5.4.1 Path travel enumeration

`[:path src dst]` is legal iff there is a subset `L ⊆ :links` such that:

- All links in `L` share the same `:owner` (call them the *chain owner*).
  Links don't carry a color — chain color is a function of the endpoints,
  and the rulebook's "chain of one player color" resolves to "chain of
  one player's ownership". See §3.1.
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

### §5.5 `:on-planet-decisions` (removed)

Folded into `:moving` (see §4.6, §5.4). No separate phase; no separate
choice-key namespace.

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
- If sundiver had a resource `res`, **drop it on `[:sun k]`** — push
  `res` into `:resources[[:sun k]]`. Do not return to the market row.
  See §6.9.
- Gain energy: `2 + 1 × active-count(k, cp) + 2 × exhausted-count(k, cp)`.
- Move all exhausted components (any owner) on `[:sun k]` to active.
- `cards-owed += 1`.

Effect of `[:sun-inner sundiver-idx]`:

- Remove sundiver from `[:sun k]`. Add to `reserve(cp)`.
- Drop the sundiver's resource (which must match `wedge-color[k]`) onto
  `[:sun k]`: push it into `:resources[[:sun k]]`. Do not return to the
  market row. See §6.9.
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

`:parent-activation` is a **snapshot of the outer `:activating`
phase-data** captured when the link-placement sub-phase was entered
(via `[:activate-space sid]` on an activation with `target := :cities`).
It carries the fields the outer phase needs when we come back to it on
`[:done-linking]` — `:target`, `:remaining`, `:activated-count`,
`:cards-owed`, `:exhausted-colors`, `:exhaust-owners`. Nesting it under `:parent-activation`
(rather than mutating the top-level phase-data) means the machine can
push/pop cleanly without conflating the two phases' bookkeeping. On
`[:done-linking]` (§5.9.2) we merge the parent snapshot back onto
`:phase-data` and set `:phase` back to `:activating`; the running
`:exhausted-colors` inside `:parent-activation` reflects any exhausts
that happened during this and prior link-placements this turn.

```
{[:link src dst]                → :link-placement   ; if outbound-color(src) ∈ exhausted-colors this turn
 [:link src dst exhaust-pk]     → :link-placement   ; if first link of outbound-color(src) this turn;
                                                    ; exhaust-pk = player whose active matching-color
                                                    ; component gets exhausted (see §5.9.4)
 [:done-linking]                → §5.9.2}
```

Note: `[:link ...]` takes no `color` argument. The link's color is
implicit — it is the color of `src` under the outbound-color derivation
in §5.9.3 — and is not stored on the link record (§3.1). The optional
`exhaust-pk` argument surfaces when this link is the first of its color
this turn *and* multiple players own active matching-color components;
the actor picks which one to exhaust (§5.9.4).

#### §5.9.1 Link legality

`[:link src dst]` / `[:link src dst exhaust-pk]` is legal iff **all** of:

1. `actor` has ≥1 unplaced link (`links-supply(actor) ≥ 1`).
2. `actor` has ≥1 energy (spent to pool).
3. `dst` is adjacent to `src`.
4. No existing link between `src` and `dst` (any player).
5. `src` is a **valid start**. Let `color = outbound-color(src)` (§5.9.3).
   `src` is valid iff one of:
   - `src` is a sun wedge (always valid), OR
   - `src` is a city space **and** there exists a chain of `actor`'s
     links from `src` (transiting cities and other `actor`-owned link
     endpoints) that terminates at a sun wedge. That is, the city must
     be *sun-anchored* via `actor`'s own link graph. Isolated cities
     (no links reaching the sun yet) are **not** valid starts, OR
   - `src` is a link-endpoint space that is itself sun-anchored via a
     chain of `actor`'s links (i.e., `actor` has a link touching `src`
     and BFS through `actor`'s link graph from `src` reaches a sun
     wedge).

   In short: `src` must reach the sun through `actor`'s existing links,
   unless `src` *is* a sun wedge. This is the "grow from the sun"
   invariant — every link chain must be anchored to the sun.
6. `src` is **not saturated for actor**: fewer than 2 existing actor
   links touch `src`.
7. `dst` is **not saturated for actor**: fewer than 2 existing actor
   links touch `dst`.
8. Actor has color-access for `color` this turn (§5.9.3).
9. If `color ∈ exhausted-colors` this turn: the action must be
   `[:link src dst]` (no exhaust-pk, none needed).
   If `color ∉ exhausted-colors`: the action must be `[:link src dst
   exhaust-pk]` and `exhaust-pk` must satisfy
   `solar-network[wedge-of(color)][:active][exhaust-pk] > 0` — i.e., that
   player has ≥1 active matching-color component. If no player does,
   the link is not legal (color-access failure).

Effect (let `color = outbound-color(src)`):

- `energy(actor) -= 1` → pool.
- Add `{:a src :b dst :owner actor}` to `:links` (color not stored).
- `links-supply(actor) -= 1`.
- **First exhaust of `color` this turn:** if `color ∉ exhausted-colors`,
  move one component from
  `solar-network[wedge-of(color)][:active][exhaust-pk]` to
  `[:exhausted][exhaust-pk]`. Add `color` to `exhausted-colors`;
  record `phase-data :exhaust-owners[color] := exhaust-pk` (see §5.9.4).
- If the recorded `exhaust-owner ≠ actor` for `color` (regardless of
  whether this link triggered the exhaust or a prior one did): pay
  1 energy from `actor` (or pool if actor has none) to
  `exhaust-owner` for this link. See §5.9.3.
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

`outbound-color(src)` is the color of a hypothetical link placed *out
of* `src`:

- If `src = [:sun k]`: color = `wedge-color[k]`.
- If `src` is a city space: color = `(cities[src]).:color`.
- If `src` is a link-connected chain-to-city space (i.e., no city at
  `src`, but a chain of `actor`'s links from `src` reaches a city):
  color = color of the city the chain terminates at.

Because links don't store a color (§3.1), a chain that branches through
multiple cities of different colors would leave `outbound-color` ambiguous.
**[POLICY]** the chain from `src` must terminate at a single city to be a
valid launching point for a new link; if it reaches multiple cities of
different colors, the link is not legal from that `src`. (In practice
this rarely matters — most chains have a single city anchor.)

"Linking out of `src` requires an exhausted component of `color` from
this turn." Specifically:

- If `color ∈ exhausted-colors` this turn: no exhaust required — the
  color has been "paid" already. The link action is `[:link src dst]`
  (no exhaust-pk arg). If the recorded `exhaust-owner ≠ actor`, this
  link still transfers 1 energy from actor to that owner (per-link
  payout, see below).
- Else (first link of this color this turn): the actor must choose an
  active matching-color component to exhaust. The link action is
  `[:link src dst exhaust-pk]`. Any player with `solar-network[wedge-of(
  color)][:active][pk] > 0` is a legal choice — including the actor
  themselves. If no player has one, the link is not legal.

**[POLICY]** — rulebook: "If you exhaust a component of another player,
they receive one energy for every link you create using that color." So
if `exhaust-pk ≠ actor`, every link placed this turn using `color`
transfers 1 energy from actor to `exhaust-pk` (paid on each link, to
avoid deferred bookkeeping).

**[POLICY]** simplify: **pay on each link**. Every `[:link src dst
(exhaust-pk?)]` placed transfers 1 energy from actor (or pool if actor
has none) to `exhaust-owners[color]` when that owner ≠ actor.

#### §5.9.4 Choosing which component to exhaust

**The actor chooses** which matching-color active component to exhaust.
There is no automatic preference for the actor's own component (previous
policy is superseded).

Enumeration:

1. If `color ∈ exhausted-colors` this turn: no choice — the actor already
   picked (on an earlier link this turn). The action is `[:link src dst]`.
2. Else: the actor chooses among all players `pk` with `solar-network[
   wedge-of(color)][:active][pk] > 0`. The action is
   `[:link src dst exhaust-pk]`, and `legal-actions` enumerates one such
   choice-key per legal `exhaust-pk`. Even if `actor` has their own active
   matching-color component, the actor may still choose to exhaust
   someone else's — this is a real strategic decision (transferring
   energy is sometimes valuable, e.g., to strengthen an ally or
   incentivize a chain).
3. If no player has an active matching-color component: link is not
   legal (already covered in §5.9.1 clause 9).

The chosen `exhaust-pk` is recorded in `phase-data :exhaust-owners
[color] := exhaust-pk` and used for per-link energy transfers throughout
this turn.

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
  jumps to `:drawing-cards` with `cards-owed = 0`.
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

### §6.9 Resource stays on the board when sundiver returns

When a sundiver returns to habitat (outer-sun activation) or reserve
(inner-sun activation) while carrying a resource, the **resource is left
behind on the space the sundiver was on** — it does *not* go back to the
market row. Concretely: the sundiver record is removed from
`:sundivers[sid]` and its owner is credited to habitat/reserve, but the
`:resource` value on that record is dropped onto `sid` as a free-standing
resource token (mechanism TBD in state shape — see note below).

**[POLICY]** state shape for on-board resource tokens: add
`:resources {sid → [color ...]}` at the top level of state (§3.1), a
per-space multiset of resources. Empty (or missing) means none. Sundivers
that later pick up a resource take it from this pile (rules for pickup
are per the rulebook, not respecified here).

Consequences vs. the previous policy:

- `:market-resources` is unchanged by sundiver returns; only `[:planet-
  buy]` and city construction touch it (§5.8).
- The "cap 5, overflow discarded" note is superseded — nothing overflows
  because nothing is returned.
- Free-standing resource tokens accumulate on the board where sundivers
  drop them, and become a spatial pickup opportunity for other sundivers.

### §6.10 Zero-move pass

`[:done-moving]` with `moves-left = 5` is a valid pass. `cards-owed = 1`
still.

### §6.11 Player has no habitat & no launch target

If `habitat(cp) = 0` OR `launch-targets(state, cp) = ∅`, `[:launch _]`
is not legal. Fly/path/done-moving still available; `[:done-moving]`
covers the "no-op turn" fallback.

### §6.12 First-turn placement + flame

Flame-space is derived (§3.1): `(front-space (mothership flame-holder))`
when mothership is placed, else `nil`.

First player's first turn: mothership is `:supply` at the top of the
turn, so derived flame-space is `nil`. On `[:place-mothership space]`,
mothership becomes `space` and derived flame-space becomes
`(front-space space)`. Turn proceeds normally.

Subsequent players' first turn: same shape — their mothership starts
`:supply`, derived flame-space is `nil` until they place. No state
mutation is needed to "clear" the flame between turns because it was
never stored.

### §6.13 Mothership never on sun

**[POLICY]** The mothership can never occupy `[:sun k]`. `[:shift-in]`
is illegal from silver (§5.2); flare-pull from silver keeps the
mothership on silver (§4.7.1). There is no `Sun → Silver` shift because
there is never a mothership on the sun to begin with. See §2.8.

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
:resources      → {}      ; no free-standing resources on the board yet
:energy-pool    → 89 - 15 = 74
:planets        → {silver [:orbit :silver 2]  ; 27 mod 5 = 2
                    green  [:orbit :green 3]   ; 27 mod 8 = 3
                    blue   [:orbit :blue 1]    ; 27 mod 13 = 1
                    purple [:orbit :purple 6]  ; 27 mod 21 = 6
                    void   [:orbit :void 27]}  ; 27 mod 34 = 27
:flame          → Sola
;; flame-space derived — Sola's mothership = :supply, so flame-space = nil.
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
;; derived flame-space = (front-space [:orbit :silver 0])
;;                     = [:orbit :silver ((0-1) mod 5)]
;;                     = [:orbit :silver 4]
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
→ :drawing-cards, cards-owed := 1
```

**`:drawing-cards`** — draw top card. Say it's `{:suit :blue, :value 4}`.
```
hands[Sola] += that card
:deck top popped
→ (cards-drawn = 1 = cards-owed) :orbit-planets, last-card = :blue
```

**`:orbit-planets`** — blue planet orbits forward 1.
```
:planets[:blue] : [:orbit :blue 1] → [:orbit :blue 0]      ; (1 - 1) mod 13 = 0
→ :advance-mothership
```

**`:advance-mothership`** — Sola's mothership advances one CCW step
(equivalent to "to derived flame-space").
```
motherships[Sola] := (front-space [:orbit :silver 0]) = [:orbit :silver 4]
→ :pass-flame
```

**`:pass-flame`** — discard Sola's hand into main discard, hand
`[{:suit :blue}]` → discard.
```
turn                    : 0 → 1
flame                   : Sola → Vega
;; derived flame-space  : nil    ; Vega's mothership still :supply
discard                 += [{:suit :blue}]
hands[Sola]             := []
→ :place-mothership (Vega's first turn)
```

### §7.2 Turn 2 — Vega

**`:place-mothership`** — Vega picks `[:place-mothership [:orbit :green 0]]`.
```
motherships[Vega] := [:orbit :green 0]
;; derived flame-space = (front-space [:orbit :green 0]) = [:orbit :green 7]
→ :choose-action-type
```

**`:choose-action-type`** — Vega has no on-board sundivers. Picks
`[:choose-move]`.

**`:moving`** — Vega considers her launch targets. Under the centered
indexing of §2.3, `[:orbit :green 0]` sits at center `0`, half-width
`1/16` — its span is `[-.0625, .0625]`. The adjacent silver spaces
(centered at `0`, `.2`, `.4`, `.6`, `.8`) are:

- Silver 0 (span `[-.1, .1]`): green 0 sits fully inside → overlap `.125` ✓
- Silver 1 (span `[.1, .3]`): starts at `.1`, green 0 ends at `.0625` → no overlap
- Silver 4 (span `[.7, .9]`): far away → no overlap

Only silver 0. Similarly on the outer side, green 0's outer neighbors
(blue: centers at `k/13`) are blue 0 (fully inside green 0), blue 1
(overlap `.024`), and blue 12 (overlap `.024`). All three are adjacent.
Frontmost (smallest CCW distance from green 0's midpoint `0`) is
blue 0 itself (distance `0`).

`launch-targets(state, Vega)`:
- Space of mothership: `[:orbit :green 0]`
- Front-space: `[:orbit :green 7]`
- Frontmost-inner (silver): `[:orbit :silver 0]`
- Frontmost-outer (blue):   `[:orbit :blue 0]`

Vega picks `[:launch [:orbit :green 7]]` (front-of-mothership).
```
habitat(Vega) : 8 → 7
sundivers[[:orbit :green 7]] += Vega
moves-left    : 5 → 4
```

Vega picks `[:launch [:orbit :silver 0]]` (frontmost-adjacent-inner).
```
habitat(Vega) : 7 → 6
sundivers[[:orbit :silver 0]] += Vega
moves-left    : 4 → 3
```

Vega picks `[:done-moving]`.

**`:drawing-cards`** — draw. Say `{:suit :silver}`.

**`:orbit-planets`** — silver planet orbits forward 1: `[:orbit :silver 2]
→ [:orbit :silver 1]`.

**`:advance-mothership`** — motherships[Vega] := (front-space
[:orbit :green 0]) = [:orbit :green 7].

**`:pass-flame`** — Lyra becomes flame; derived flame-space = nil
(Lyra hasn't placed yet).

### §7.3 Turn 3 — Lyra

Similar shape — places on `[:orbit :blue 0]`, launches, moves, draws.
Elided for space.

### §7.4 Turn 4 — Sola (2nd turn)

**`:resolve-mothership`** — Sola's mothership at `[:orbit :silver 4]`.
Legal: `[:shift-out]`, `[:stay]`. (`[:shift-in]` is *not* legal from
silver — §5.2, §6.13.)

Sola picks `[:shift-out]`:
```
Green spaces adjacent to silver 4? Silver 4: center .8, span [.7, .9].
Green space 6: center .75, span [6.5/8-1/16, 6.5/8+1/16] = [.6875, .8125]
  → overlap [.7, .8125] = .1125.
Green space 7: center .875, span [.8125, .9375]
  → overlap [.8125, .9] = .0875.
Green space 5: center .625, span [.5625, .6875] → no overlap.
Two green candidates. Frontmost (smallest CCW distance from silver 4 mid = .8):
   green 6 mid = .75  → (.8 - .75) mod 1 = .05
   green 7 mid = .875 → (.8 - .875) mod 1 = .925
   → green 6 is frontmost.

motherships[Sola] := [:orbit :green 6]
;; derived flame-space = (front-space [:orbit :green 6])
;;                     = [:orbit :green ((6-1) mod 8)]
;;                     = [:orbit :green 5]
→ :choose-action-type
```

**`:choose-action-type`** — Sola picks `[:choose-activate]`. Sola has a
sundiver on `[:orbit :silver 0]` (a *planet space* was previously silver
2, but the silver planet advanced forward to silver 1 on turn 2 — see
§7.2 — so silver 0 is *not* the planet space). Sola's other sundiver is
on `[:orbit :silver 4]` (from turn 1's launch).

So Sola's activation targets:
- Sun: no sundiver.
- Planets: silver planet at `[:orbit :silver 1]`. Sola has no sundiver
  there. → not available.
- Cities: no cities exist yet. → not available.

`[:choose-activate]` is illegal. Sola must `[:choose-move]`.

**`:moving`** — Sola flies her sundiver from `[:orbit :silver 4]` to
`[:orbit :green 7]` (adjacent, cross-ring). `[:fly [:orbit :silver 4]
[:orbit :green 7]]`.
```
sundivers[[:orbit :silver 4]] -= Sola-sundiver
sundivers[[:orbit :green 7]] += that sundiver
moves-left : 5 → 4
```

Sola picks `[:done-moving]`.

*(further turns not detailed)*

### §7.5 What the trace demonstrates

- Placement, launch, fly, done-moving are exercised.
- Front-of-mothership computation across a ring change (silver 4 → green 6)
  demonstrates §2.8 with the centered-orbit indexing of §2.3.
- Front-of-adjacent-inner (green 0 → silver 0, uniquely — no ambiguity
  because green 0 sits fully inside silver 0 and its off-beam silver
  neighbors don't overlap it at all) demonstrates §2.5 with the
  beam-centered orbit convention.
- Empty-hand pass at end of turn discards.
- Planet orbit resolves *before* mothership advance.
- Choose-activate legality gate (§6.2) suppresses illegal target selection.
- Flame-space is never stored — every "flame-space" line in the trace is
  a derivation from the current mothership position.
- `[:shift-in]` is *not* offered when the mothership is on silver (§7.4).

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
- Beam: bright yellow radial line at angle 0 (top of screen). The beam
  passes through the centers of the five index-`0` orbital spaces (§2.6)
  and along the boundary between `[:sun 0]` and `[:sun 1]` — never
  through the middle of a sun wedge.
- Planets: filled circle at planet-space's centroid, colored by ring
  color, with a subtle glow.
- Motherships: arrow-glyph at mothership's space, pointing CCW.
  Player-colored fill (silver / green / blue / purple / void per
  wedge-color).
- Flame-of-Justice: small flame icon at the derived flame-space
  (`(front-space (mothership (:flame state)))`) — one step CCW of the
  current player's mothership. If that player hasn't placed yet, no
  flame icon.
- Sundivers: circular tokens (color = owner). If carrying a resource,
  center dot of resource color. If `:on-planet?`, overlapping planet
  glyph.
- Cities: pentagonal glyph, color = city color, base outline = platform
  owner color.
- Links: thick line segments between space-centroids, colored by
  `:owner`'s wedge color. (Link records don't carry a `:color` field —
  the visual color is the owner's identity color; if desired, an
  auxiliary hue derived from `outbound-color(a)` per §5.9.3 can be shown
  as a tint or gradient.)
- Hover: highlight adjacent spaces + legal actions from hovered space.
- Click: submits action if unambiguous, else opens disambiguation
  popover.

**Canonical geometry vs. drawn geometry.** For visual clarity, the
drawn board *may* nudge intra-ring boundaries away from perfectly even
angles so that two spaces that are canonically adjacent (per §2.5) look
adjacent to a human eye — especially for tight cross-ring overlaps
(e.g., purple ↔ void, where the intended overlap is ≈ `.009`). The
canonical adjacency graph is nevertheless derived from the *even*
angular ranges of §2.4, not from the drawn positions. When rendering:

- Use canonical `angular-center+half` to compute adjacencies for click
  targeting, link legality, and highlighting.
- Adjust space-boundary drawing angles freely — they are cosmetic and
  do not feed back into `board/build-adjacency`.
- When drawing hover-hint "link" lines between the centroids of adjacent
  spaces, use the *drawn* centroids so the guides land where the player
  sees the space, not where the math places it.

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

### Board topology (fixed — good regression tests for §2)

- Every `[:sun k]` has exactly 4 neighbors: 2 in-ring
  (`[:sun (k-1) mod 5]`, `[:sun (k+1) mod 5]`) and 2 cross-ring
  (`[:orbit :silver (k-1) mod 5]`, `[:orbit :silver k]`).
- Every `[:orbit :silver n]` is adjacent to 2 silver in-ring neighbors,
  2 sun wedges (`[:sun n]` and `[:sun ((n+1) mod 5)]`), and its green
  neighbors: 3 for `n = 0` (green 7, green 0, green 1) and 2 otherwise.
- Every beam-aligned inner-ring space `[:orbit r 0]` (for `r` ≠ void) has
  exactly 3 outer-ring neighbors: the corresponding outer beam-space and
  one on either side of it.
- Every non-beam-aligned inner-ring space has 1–3 outer-ring neighbors
  (typically 2).
- Every void space has 0 outer-ring neighbors (void is the outermost ring).
- Adjacency is symmetric: `b ∈ neighbors(a) ⇔ a ∈ neighbors(b)`.
- No self-loops: `a ∉ neighbors(a)`.

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
