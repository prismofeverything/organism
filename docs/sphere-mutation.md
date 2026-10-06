# SPHERE mutation

The game on a globe. A lobby option like FLOW or RAIN; the rules are the
ordinary game's, played on a different board.

## The boards

A sphere cannot be hexagonal everywhere. Euler's formula fixes the shortfall:
summed over every cell, `6 - neighbours` is always 12. Each board is a
Goldberg-style geodesic grid, hexagonal except for a few symmetric cells with
four or five neighbours.

| players | cells | grid | cores | open spaces between players | open spaces to each core |
|---|---|---|---|---|---|
| 2 | 38  | octahedral GP(3,0)          | 1 | 3 | 2 |
| 3 | 86  | octahedral GP(4,1)          | 1 | 3 | 3 |
| 4 | 78  | octahedral GP(3,2)          | 1 | 3 | 3 |
| 5 | 97  | pentagonal bipyramid GP(3,2) | 2 | 3 | 3 / 3 |
| 6 | 110 | octahedral GP(3,3)          | 2 | 3 | 4 / 2 |

A home is three mutually adjacent cells. The spacing rule is the flat boards'
own: at least three open spaces between neighbouring players.

**Fairness.** Every board is symmetric about its core axis, so a rotation
carries each player's home to the next. Every player sees the same board. The
export checks this for every home.

**Why two cores from five players.** With one core, the only arrangements
that are fair to every player put all the players on a single ring around
that core. The symmetry that keeps the core fixed is a rotation about its
axis. So from five players there is a choice between even spacing, fairness,
and one core, and only two can be had. SPHERE chooses two cores, at
opposite poles:

- **5 players:** five homes around the equator with a core at each pole. This
  is the most even arrangement of seven points. Only a grid with a 5-fold axis
  can hold it. The icosahedral grid needs 192 cells for this; a grid built on a
  pentagonal bipyramid needs 97. That grid's two five-neighbour cells are the
  cores, and its five four-neighbour cells sit round the equator.
- **6 players:** the homes are at the six corners of an octahedron, with the
  cores at two opposite faces. Each player has one near core and one far core.
  A half-turn swaps the cores and keeps the board the same for every player.

Each core pays its holder a capture at the start of the holder's turn
(`game/centers`, `game/award-center`). A board counts as locked only while no
core is held (`game/stalemate?`).

## Where it lives

- `src/cljc/organism/sphere_boards.cljc` holds the generated board data.
  Rebuild it with
  `.venv/bin/python dev/sphere/export_sphere.py src/cljc/organism/sphere_boards.cljc`.
  The search code is in `dev/sphere/`.
- `src/cljc/organism/sphere.cljc` makes the game through `game/initial-game`,
  with `:centers` added, and draws the 2D map.
- Spaces keep the flat boards' `[ring index]` names. Ring A holds the core(s),
  and each later ring is one step further out. Ring colours, labels and the
  saved-game format all carry over.
- The 2D map is distance from the core, drawn true: one disc for one core,
  with the far side of the globe at the rim. With two cores it is one disc per
  core. Thin lines show adjacency, since the map stretches toward the rim.
- In the 3D view (`view3d.cljs`), a sphere space is an `[x y z]` point on the
  globe, where a flat space is `[x z]`. Everything is placed through `place!`
  and `place-between!`, which stand objects up along the surface. Pieces,
  food, targets and plasma are the flat board's own code. A head light lights
  the side of the globe that faces away from the sun.
- The trained (native) bot declines mutated games, so a sphere game falls
  back to OBO.

## Not done

- **Training:** the native engine and network only know flat boards.
- **Exported files:** an exported game file (`format.clj`) still labels its
  board with the flat `rings-clockwise-30deg-v1` coordinates. The adjacencies
  are correct, but the separate replay viewer would draw a sphere game flat.
- **SPHERE with RAIN:** ticking one unticks the other.
