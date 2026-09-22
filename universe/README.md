# UNIVERSE — a 60 card deck on three axes

An ordinary deck varies on two axes: suit × rank. This one adds a third.

    3 colors  ×  4 shapes  ×  5 numbers  =  60 cards

Every combination appears exactly once, so the deck is a complete 3×4×5
lattice — the same structure that makes SET work, with one more value on the
number axis and one fewer on the shape axis.

| axis | values |
|---|---|
| color | purple · green · yellow |
| shape | eye · helix · pyramid · star |
| number | 1 · 2 · 3 · 4 · 5 |

## The card

Top-left and bottom-right carry the index — number, then shape — the second
copy turned 180° so the card reads either way up. The middle carries that many
copies of the shape on a ring, each turned by its own share of a full circle,
so the rosette has n-fold rotational symmetry: 1 sits big and alone, 2 face
head to foot, 3 make a triangle, 4 a cross, 5 a pentagon.

Three things are worth knowing about how the middle is built:

**The ring is packed, not guessed.** Placing copies on a circle whose radius is
`1/sin(π/n)` makes their bounding circles exactly tangent — always safe, but
wasteful for a shape as wide and flat as the eye. So the packer shrinks the
ring and rasterises the real outlines until they are about to touch, which
lets the eye nest to 0.74 of the circle bound and the pyramid to 0.70. Packing
to *mere* non-overlap turned out to be a mistake: four and five stars became
impossible to count at a glance, which is the one job the middle of the card
has. Copies now keep a clearance of 7.5% of their own radius. Verified: zero
overlap on all sixteen shape × count rosettes.

**Every rosette spans the same circle.** Whatever the shape or the count, the
art is scaled so its outermost ink touches a circle 636px across, centred on
the card. That is what makes a 1 and a 5 feel like the same deck.

**Stroke weights are evened out.** Measured as ink area over enclosing-circle
area the four marks sit at 0.33 (eye), 0.27 (star), 0.23 (pyramid) and 0.19
(helix); `WEIGHT` in `extract_shapes.py` nudges the helix up to close the gap.
The same dial keeps the finest strokes above what CMYK can hold, since the
shapes land on the card at roughly a fifth of their drawn scale.

Two of the four marks run off the edge of their 3000px canvas, and the two are
handled differently. The pyramid's four light rays are decoration, so they are
simply dropped. The star's lower-left arm is one of its five points, so it is
reconstructed — see below.

## Color, and reading it without color

The color axis runs **purple → green → yellow**, darkest to lightest, and it
is that order which actually carries it.

| | hex | L\* | C | h | as grey |
|---|---|---|---|---|---|
| purple | `#651694` | 28 | 75 | 316 | 66 |
| green | `#26883F` | 50 | 54 | 145 | 119 |
| yellow | `#E6AD24` | 74 | 72 | 82 | 182 |

Read as greys those are 66, 119, 182 against paper at 255 — three steps of 53,
63 and 73. Encoding the axis as brightness is what makes it survive any of the
three dichromacies: there is no hue pair left to confuse, because the
information is not in the hue. `proof/contact-grey.png` is the whole deck with
the color thrown away.

Within that ordering each color is pushed as far as it will go. The purple is
the most saturated violet that still sits clearly darkest — chroma peaks at
middling lightness, so being the dark one costs it something, and dropping it
further turns it to near-black or, at the hues where chroma is highest, to
indigo. The green sits at its chroma peak, hue 145, a solid forest green. The
yellow is untouched. Chroma is held at 88% of the sRGB gamut edge so CMYK has
somewhere to land.

The hues are **not** evenly spaced, and that is deliberate. A strict 120°
triad through yellow and purple puts its third leg at hue ~195, which is a
teal (`#2A7E7C`), not a green — real greens live at Lab hue 135–165. Nor can
the three be made to sum to a neutral grey, which would need the yellow at
about a third of its chroma. The regularity in this palette is in lightness,
where it is worth having; forcing it into the hue circle as well would cost
both the green and the yellow.

## Putting the star's fifth arm back

The star as drawn has one arm running off the left edge of the canvas, cut
flat at x = 0 over a 219px span. Reconstructing it rests on one observation:
**the brush is a circle**. All four intact arm tips measure the same radius,
76px, because each arm is two strokes of that brush converging, and the tip is
just the brush itself where they meet.

So `extend_clipped` reads the wedge's two edges inward from the border, pushes
each in by 76px to recover the stroke centreline it came from, carries those
two centrelines out past the edge until they cross, and sweeps the brush along
them. The tip rounds itself off. The new tip lands 1749px from the star's hub,
against 1502 / 1499 / 1635 / 1785 for the four arms that survived.

Three details that took a couple of passes to get right:

- **Straight lines are wrong.** A brush stroke bends; fitting the edges with
  quadratics halves the residual against a line (3.4 and 4.1px versus 6.6 and
  8.9), and the arm visibly curves — its slope runs from −0.17 to +0.08 across
  the reconstruction.
- **The fit has to be anchored.** An ordinary least-squares fit sat 13px off
  the ink at the seam and left a step, so the quadratic is constrained to pass
  through the measured edge exactly and weighted towards it, which also means
  the slope and curvature being extrapolated are the ones at the join rather
  than an average over the whole arm.
- **The sweep has to lead in.** A disc centred up to a diameter inside the
  border still reaches across it; without those the envelope pinched in just
  outside the seam. The sweep therefore starts well inside — and the result is
  then masked to the region beyond the original canvas, so not one pixel of
  the artist's own ink is touched. That is asserted, not hoped for.

## The back

Two concentric rings on a violet-black ground: four green eyes inside, and
twelve of the other three shapes outside, alternating yellow and purple, with
a thin gold ring between them. All three of the deck's colors appear.

The shapes repeat every three places around the outer ring and the colors
every two, so the whole pattern comes back around every six — half of twelve.
Turn the card end over end and every symbol lands on a copy of itself in the
same color. That is the symmetry that matters, a card being a rectangle:
there is no way to tell from the back which way up one is being held.

It holds exactly, not approximately, and the build checks it: the trimmed back
is pixel-for-pixel identical to itself rotated 180° in all three presets. Two
things had to be right for that. The ring is computed from a distance field
rather than drawn — PIL's ellipse has no antialiasing, and supersampling a
line four pixels wide still came back visibly stepped — and its raster is
forced to an even size, because an odd one makes the paste offset round a
half-integer and shifts the ring a pixel off centre. And the emblem is built
on the trimmed size, which is even both ways, then laid into the bleed: some
presets bleed by half a pixel (the Game Crafter trims 0.125" off 825), and
centring on that rounds symbols apart by one.

## Hands

`hands.py` enumerates all **5,461,512** five-card hands and classifies every
one, so the frequencies are counted rather than argued; `make_chart.py` draws
them as `out/universe-hands.png`, rarest first, with a real example hand
beside each. Nineteen patterns exist. The counts reproduce the closed-form
totals exactly (five of a kind 3,960; four 118,800; full house 290,400; trips
950,400; two pair 1,568,160; pair 2,280,960; straight 248,832), and the same
classifier run over a normal 52-card deck reproduces the published poker
numbers exactly, which is what makes the comparison below trustworthy.

Splitting the suit in two changes the shape of the game:

- **There is no high-card hand.** Only five numbers exist, so any hand without
  a repeat is already the whole run 1–2–3–4–5. Half of all poker hands are
  nothing; here none are.
- **A color-and-shape flush is always a straight.** A suit holds exactly five
  cards, so taking five takes them all. There are exactly 12 such hands — the
  rarest thing in the deck at 1 in 455,126.
- **Five of a kind can never be a flush**, and there is no shape four of a
  kind: a color holds only four shapes and a shape only three colors.
- **Every flush beats four of a kind.** With 20 cards in a color and 15 in a
  shape, even the commonest flush (a pair all in one shape, 1 in 843) is far
  rarer than four of a kind (1 in 46).

Matching numbers is much easier than in poker, because each number has twelve
cards rather than four:

| | UNIVERSE | poker | |
|---|---|---|---|
| one pair | 41.8% | 42.3% | about the same |
| two pair | 28.7% | 4.8% | 6× |
| three of a kind | 17.4% | 2.1% | 8× |
| straight | 4.6% | 0.4% | 12× |
| full house | 5.3% | 0.14% | 37× |
| four of a kind | 2.2% | 0.024% | 91× |
| five of a kind | 0.07% | — | |
| any flush | 1.07% | 0.20% | 5× |

(percentages over all hands of that number pattern, flushes included)

Curiously one pair lands in almost exactly the same place in both decks — but
in poker it is a below-average hand with 50% of hands beating nothing at all,
while here it is the floor.

## The shape of the ranking

Three drawings, each answering a different question.

`out/universe-hands.png` (`make chart`) is the reference: nineteen rows, rarest
first, with a real example hand beside each.

`out/universe-hand-lattice.png` (`make lattice`) shows the families. Seven rows,
one per number pattern, rarity running left to right so position is the ranking;
each row branches up into its color flush and down into its shape flush. Color
and shape are **incomparable** — neither implies the other — so the suit axis is
a diamond, not a ladder.

`out/universe-hand-space.png` (`make space`) shows why any of it is true.

### Why the third axis collapses

The deck is a 3 × 4 × 5 block of cards, and a hand's suit grade is nothing more
than **the smallest axis-aligned sub-block it fits inside**:

| grade | block | cards | to a number | caps at | hands |
|---|---|---|---|---|---|
| any suits | 3×4×5 | 60 | 12 | five of a kind | 7 |
| all one color | 1×4×5 | 20 | 4 | four of a kind | 6 |
| all one shape | 3×1×5 | 15 | 3 | full house | 5 |
| one color + shape | 1×1×5 | 5 | 1 | straight | 1 |

That single column — *cards to a number* — decides everything. A color slab is
four shapes thick, so nothing in it can beat four of a kind. A shape slab is
three colors thick, so it stops at a full house. A single suit is one card
thick, so the only hand in it is the straight, which is why the rarest hand in
the deck is the only one of its kind. **The nine impossible cells are not a
quirk; they are the blocks being too thin to hold those patterns.**

So the hand space is drawn as four columns, one per sub-block, with rarity as
the height: every color flush in the deck stands in one column, every shape
flush in another, and the number patterns are the links across. The columns
climb and shorten to the right, ending in a single hand.

It is deliberately **not** drawn in perspective. A projected floor adds depth to
screen height, which would wreck the one comparison the picture exists to make.
The three dimensions are drawn as what they are instead — four solid blocks
along the bottom.

Two consequences worth knowing:

- **The straight's diamond closes exactly.** 244,800 ÷ 80 = 3,060 ÷ 255 = 12,
  and 244,800 ÷ 255 = 960 ÷ 80 = 12. Both routes land on 20,400, so for a
  straight the two constraints are exactly independent.
- **Across the other rows they compound.** A color flush costs a pair 98×, two
  pair 120×, trips 164×, a full house 200× and four of a kind 494×. The
  better your numbers, the dearer the flush on top, because a strong number
  pattern has already spent your shapes.

## Naming the hands

Poker's names carry no structure. These do: a group of matching cards is named
for its size, and a hand made of two groups is *split*.

| pattern | name | in a color | in a shape |
|---|---|---|---|
| one pair | **dyad** | color dyad | shape dyad |
| two pair | **split tetrad** | color split tetrad | shape split tetrad |
| three of a kind | **triad** | color triad | shape triad |
| full house | **split pentad** | color split pentad | shape split pentad |
| four of a kind | **tetrad** | color tetrad | — |
| five of a kind | **pentad** | — | — |
| all five numbers | **sequence** | color sequence | shape sequence |
| … and both at once | **singularity** | | |

So every hand in the deck is said in two words, and the words tell you what it
is: a *shape split pentad* is three and two, all of one shape.

## The key

`out/universe-key.svg` (`make key`) is the deck stated in one card: the title,
then three colors, four shapes, five numbers, then all sixty of them.

The grid is six rows of ten, and it is laid out so the lattice shows. Color
bands the rows two at a time, number bands the columns two at a time, and the
shape steps on by one with every color *and* every number, which sets the whole
field shimmering diagonally. Every one of the 3 × 4 × 5 combinations lands
exactly once — the build asserts it rather than trusting the arithmetic.

## The figure

`out/universe-pyramid.svg` (`make pyramid`) is the whole deck as one figure:
shape down the left, any-suits up the middle, color down the right, the three
leaning in towards the singularity at the top. Arrows run from the middle out
to either side — that is the constraint being applied — and the two sequences
sweep up to the apex, the only place both constraints can hold at once.

Each node carries a glyph for its number pattern built from one rule: **dots
are cards, and a ring joining them means they share a number.** A dyad is two
joined dots, a triad three, a tetrad four, a pentad five; a split pattern is
two rings side by side; and the sequence is five loose dots, because nothing
matches. Under the glyph are the name, the odds, the rank, and a real example
hand.

Each axis stops at its own best hand rather than running past it, and that
hand is drawn a size up — bigger glyph, bigger name, bigger cards — so the
three pinnacles read as pinnacles: **shape split pentad**, **color tetrad**,
and **pentad**, the most any one sub-block can hold. Two of them are the same
hand twice over: both land on 1 in 22,756, which is why the figure has two
shoulders at one height. The singularity above them is shown as the purple eye,
the deck's own mark; any of the twelve suits would serve equally.

**Vertical position is probability.** Every hand sits above every hand more
likely than it, across all three axes, so the figure can be read straight off:
higher beats lower, wherever it stands. Spacing is logarithmic in rarity, but
opened out to a floor wherever two hands sit so close together that their
blocks would collide — the *order* is exact everywhere, and only the spacing
gives, and only where it must. That leaves the nodes unevenly spaced along any
one axis, which is the honest result: hands do not come at regular intervals.
A rule is drawn at each distinct probability, and the two that are exactly tied
— color tetrad and shape split pentad, both 1 in 22,756 — share one.

It is **vector throughout**. `trace_shapes.py` traces the four inked mattes
with marching squares, simplifies them, and normalises each to the unit
enclosing circle — the same frame the rosette packer works in — so the example
cards are placed with the identical geometry the printed deck uses, at about
12 KB for all four outlines. An example on the figure *is* the card.

The silhouette, for the record, is a **frustum**: a pyramid with its point cut
off. Three axes converging on a circle rather than a point.

## Playing it online

`/universe` on the ORGANISM server is UNIVERSE hold'em: two cards to each
player, three to the table, turned one at a time, four betting rounds,
no-limit, everyone starting on the same stack until one player holds them all.
The rules are `src/cljc/universe/`, and `make web` emits the deck's geometry to
`resources/public/universe/deck.json` so the browser draws the printed card
rather than a second approximation of it — the rosette packing and the back's
emblem are computed by `deck.py` either way.

### Why the hand is five cards and the board is three

Hold'em deals seven and keeps the best five. That does not survive this deck,
and the reason is the number axis being only five wide. Measured over the best
five of N, dealt at random:

| N | ranks still reachable | inversions | the worst hand that can exist |
|---|---|---|---|
| 5 | 17/18 | 0 | dyad, 41.3% |
| 6 | 17/18 | 0 | split tetrad, 41.0% |
| 7 | 16/18 | 2 | split tetrad, 15.2% |
| 8 | 16/18 | 9 | split tetrad, 2.6% |
| 9 | 15/18 | 6 | split pentad, 13.0% |

An *inversion* is a pair where the better-ranked hand ends up the more common
one — zero means the table is still playing the chart. By seven cards the
pigeonhole has eaten two rows outright: `triad` and `dyad` cannot be made at
all, because seven cards over five numbers always contain two pairs or better.
The split pentad, which the chart calls 1 in 19, arrives 38% of the time.

**A draw round is worse, not better.** Each number has twelve cards rather than
four, so drawing to a pair improves about 46% of the time against 12.5% in a
real deck, while drawing to a flush is as hard as ever. All the improvement
runs along the number axis and the sequence — which nobody draws toward —
falls behind hands it is supposed to beat: 7 to 12 inversions at every discard
cap, including a cap of one.

Shortening the hand instead is worse still, and for a reason the deck already
states. At three cards 42% of hands are `mixed all distinct` and at four cards
20% are — pure nothing. *There is no high-card hand* only at five, because five
is where having no repeat means holding all five numbers. Three- and four-card
charts exist and are perfectly countable (34,220 and 487,635 hands, 10 and 15
kinds); they simply give the deck back the junk hand it does not currently have.

So the hand is five cards and nobody selects, which leaves only the question of
how many of the five are shared. More shared cards make split pots; more
private cards make decisions:

| split | chop, 2 / 3 / 6 players | spread of starting-hand equity |
|---|---|---|
| 5+0 | 0.07 / 0.13 / 0.23% | sd 28.9%, 10–90 band 1–76% |
| 4+1 | 0.18 / 0.25 / 0.35% | sd 17.9%, 18–64% |
| 3+2 | 0.36 / 0.43 / 0.76% | sd 12.3%, 21–48% |
| **2+3** | **1.06 / 1.38 / 2.68%** | **sd 8.67%, 23.2–45.0%** |
| 1+4 | 5.3 / 7.6 / 14.2% | sd 9.4%, 22–47% |
| *real Texas hold'em* | *3.78 / 4.58 / 7.39%* | *sd 9.01%, 23.1–45.2%* |

Two hole cards and three shared is Texas hold'em's game to within a third of a
percent on starting-hand spread, with a third of the split pots — because with
five cards total no best-of-seven selection ever collapses two different
holdings onto the same board hand. Every card you hold counts. And each player's
five cards are a uniform five-card hand, so **all nineteen rows occur at exactly
the frequency printed against them**: the chart is the game, not a summary of it.

### Breaking ties

Chart rank first. Then the numbers, the way poker reads them — group size, then
number, five high. Then the colors, purple over green over yellow.

Numbers alone cannot carry it: five numbers is too few, and a numbers-only
kicker splits 18% of heads-up pots and 32% of six-handed ones, against 4–7% for
a real deck. Color brings that to 6% and 11%, which is where poker lives.

Shape never breaks a tie, and that is the point. The deck orders its three
colors by lightness and gives its four marks no order at all, so using exactly
the axis that *is* ordered is what lands the chop rate in the right place —
adding shape as well would drive it to 0.6%, which is fewer split pots than
poker wants and a ranking the deck deliberately does not have.

One consequence worth knowing: the chart's single tie — a color tetrad and a
shape split pentad, both 1 in 22,756 — is broken in play by the group-size rule,
so the tetrad takes it. The cascade has to do something, and comparing the
bigger group first is what it does everywhere else.

## Building

    make all                    # everything, MPC preset
    make faces PRESET=tgc       # faces at Game Crafter size
    make sheets PAPER=a4        # print-at-home imposition
    make proof                  # contact sheets, color and greyscale

| | |
|---|---|
| `inputs/` | the four inked source PNGs, 3000² |
| `extract_shapes.py` | → `shapes/*.png` alpha mattes + `shapes.json` (enclosing circle, centroid) |
| `deck.py` | palette, layout, rosette packer, face and back renderers |
| `make_cards.py` | CLI: faces, back, sheets, proofs |
| `out/<preset>/faces/` | 60 PNGs + `manifest.csv` |
| `out/print-at-home/` | 3×3 imposed sheets, PNG + PDF (fronts / backs / duplex) |
| `proof/` | contact sheet, greyscale contact sheet, palette |

## Printing

**60 cards is a completely ordinary deck size** — 54 is just the poker
standard, not a constraint. Nobody will make you pad the deck or split off a
48-card set.

**Order from [MakePlayingCards](https://www.makeplayingcards.com/design/custom-blank-card.html).**
Their Custom Game Cards product takes 18–612 cards per deck, has no minimum
order (one deck, ~$17), and every card can differ front and back. Upload
`out/mpc/faces/` as the fronts and `out/mpc/back.png` as the shared back;
`manifest.csv` gives the order. BoardGamesMaker and PrinterStudio are the same
factory if their stock options suit better.

Worth knowing about the alternatives:

- **[The Game Crafter](https://www.thegamecrafter.com/make/custom-playing-cards)** —
  US-based, any deck size, around $10 a deck, and it doubles as a storefront if
  you ever want to sell the game print-on-demand. Files are in `out/tgc/`.
- **[DriveThruCards](https://www.drivethrucards.com/joincards.php)** — US, one
  deck minimum, strongest if you want to sell through the DriveThru marketplace.
- **PrintNinja / Ad Magic** — offset, only worth it at 500+ decks. This is where
  to go if the game ever runs a campaign, not now.

### Specs

| | cut | with bleed | bleed each side |
|---|---|---|---|
| MPC poker | 750 × 1050 px | **822 × 1122 px** | 36 px (0.12″) |
| Game Crafter poker | 750 × 1050 px | **825 × 1125 px** | 37.5 px (0.125″) |

All at 300 DPI, 2.5″ × 3.5″ finished. MPC also asks for 36px of safe margin
inside the cut line; the tightest margin anywhere in the deck is 50px, checked
across all 60 faces.

The Game Crafter figure is the commonly-quoted one — worth checking against the
template you download from them before ordering, since it is a one-line change
in `PRESETS` if it differs.

Two things to expect from CMYK: the purple is a vivid RGB value and will come
back a little duller and less blue on press, and the star's finest tapers are
around 0.1 mm on a 5-card. Order one deck before ordering ten.

### Printing at home

`out/print-at-home/universe-letter-fronts.pdf` is seven pages of nine cards,
butted edge to edge so one cut serves two cards, with crop marks out in the
margins. Print at 100% — *not* "fit to page" — on the heaviest stock the
printer takes. `-backs.pdf` is the matching backs and `-duplex.pdf` interleaves
the two for a duplex printer. A4 versions alongside.
