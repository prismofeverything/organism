# OGF v2 view records: ring coordinates and palette

Implemented for the Clojure site exporter, Rust training recordings, Python
recordings, and the shared dashboard viewer. This is the **view** profile: semantic
replay/actions remain the proposal in `ogf-v2-proposal.md`. Version 2 does not by
itself claim legal replay verification.

## Coordinates

`board.coordinates` is `rings-clockwise-30deg-v1`:

- Ring zero is `A`, ring one `B`, then `C` through `Z`, `AA`, `AB`, etc.
- The center is `A0`. Ring level r > 0 has `symmetry * r` possible indices.
- Index zero is the common ray 30 degrees clockwise from screen-right (down/right
  in SVG coordinates), matching the existing site's layout. Indices increase
  clockwise, interpolating along polygon edges between radial axes.
- Removed/notched spaces are omitted. Remaining spaces retain their indices.
  There is no renumbering to close the gaps.
- Ring labels are geometric identity, never CSS colors or neural tensor indices.
  Fivefold and sevenfold symmetry use the same convention as sixfold symmetry.
- `spaces`, `adjacencies`, `center`, `homes`, frame element locations and free-food
  map keys all use these IDs. Explicit topology remains authoritative.

A four-ring fivefold board has A0, B0..B4, C0..C9 and D0..D14 before notches.
This naming supports additional rings and symmetry orders; it does not add new
player counts to the native trainer or change the site's supported game variants.

The dashboard applies a further 60-degree clockwise presentation rotation to the
whole board, so its zero column points straight down. Radial labels and highlights
rotate with the board. Elements and food are counter-rotated within their spaces
to retain their upright orientation. This does not rename spaces or change the stored
coordinate convention.

## Palette

`board.ring-colors` is an ordered array of actual CSS colors, center outward.
In v1 this field misleadingly contained ring *labels*, not the actual palette.

For n players, take the first n palette colors and reverse their order; assign
those colors in seat order. This is the site's existing `find-player-colors`
behavior. For two players on `[yellow, red, blue]`, player one is red and player
two yellow. New records omit the independent top-level `colors` map.

If a setup has more players than rings, optional `board.palette-tail` preserves
just the extra palette entries needed by the existing derivation. These are not
additional board rings. Ordinary training setups do not need this field.

The site exporter saves its actual configured palette. Native and Python training
generate a random palette for each game using the creation page's hue/saturation/
lightness recipe (lighter inner rings, darker outer rings). Presentation randomness
is derived from the game identity, independently of search/training randomness.
Native checkpoints persist the palette; older unfinished episodes acquire a stable
palette on resume. Completed recordings keep their saved colors. The viewer's Change colors button uses the existing site
palette generator, changes the recording's palette and updates its download.
Changing colors cannot change space identity, geometry, rules or actions.

## Compact example

```json
{
  "format": "organism",
  "version": 2,
  "profile": "view",
  "name": "example",
  "symmetry": 6,
  "players": ["p0", "p1"],
  "board": {
    "coordinates": "rings-clockwise-30deg-v1",
    "center": "A0",
    "ring-colors": ["#fff88c", "#da6558", "#849cd5"],
    "spaces": ["A0", "B0"],
    "adjacencies": {"A0": ["B0"], "B0": ["A0"]}
  },
  "frames": [{"turn": 0, "round": 0, "player": "p0", "elements": [], "food": {}, "captures": {"p0": 0, "p1": 0}}]
}
```

The abbreviated graph illustrates field spelling only, not a legal Organism setup.
The structural schema is `ogf-view-v2.schema.json`; legality/topology/reference
verification requires further checks, not merely schema validation.

## Compatibility

The viewer reads v1 and converts locations to v2 ring IDs. Because v1 did not save
the board palette, it generates one and marks `board.palette-origin` as
`generated-on-legacy-import`. It does not claim to recover original colors.
A changed palette is marked `viewer-selection`. Legacy files on disk are untouched;
the download contains the normalized v2 view record and its chosen palette.

Clojure/Python readers accept both coordinate syntaxes. Old standalone consumers
that only understand v1 must be upgraded; the version bump makes that incompatibility
explicit. Unknown coordinate systems and non-view profiles are rejected by this
viewer rather than guessed. Numeric Rust/Python action IDs remain implementation
trace metadata, not portable v2 semantic actions.

Native checkpoint frames retain their previous internal representation; export
translates them at the boundary. Resuming an older checkpoint therefore does not
mix old and new coordinate IDs in an exported recording. The ablation experiment
retains its frozen executable and continues emitting v1, which the viewer can read.

Validation covers letter rollover, palette-derived seats, five/six/sevenfold
clockwise geometry, omitted spaces, duplicate display colors, Clojure export/read,
Python recording/layout, native export from legacy checkpoint frames, and browser
palette change/download/reload plus legacy import.
