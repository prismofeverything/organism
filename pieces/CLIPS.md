# ORGANISM — short gameplay clips

A small, data-driven pipeline for rendering short (~5 s, 1080²) gameplay clips that
explain individual mechanics, plus the score-track and box/title shots. Separate from
the full-game replay video (`build_play_real.py` / `make video`), but uses the same
3D piece grafts, board layout, and food tokens.

Outputs land in `scene/clip_<name>.mp4` (and `scene/organism_title.png`). `*.mp4` and
`renders/` are gitignored; the **scripts + `clip_assets/` are the durable source** — every
clip is fully regenerable from them.

## Render

```bash
./make_clips.sh move eat grow circulate conflict perish two_org three_org board glow   # board clips
# power + box have their own scripts (different backgrounds):
SAMPLES=24 ~/Downloads/blender-5.1.1-linux-x64/blender -b --threads 4 --python build_power.py && \
  ffmpeg -y -framerate 24 -i renders/clips/power/f%04d.png -c:v libx264 -pix_fmt yuv420p scene/clip_power.mp4
SAMPLES=24 ~/Downloads/blender-5.1.1-linux-x64/blender -b --threads 4 --python build_box.py && \
  ffmpeg -y -framerate 24 -i renders/clips/box/f%04d.png   -c:v libx264 -pix_fmt yuv420p scene/clip_box.mp4
```

**Fast iteration:** `CLIP=move FRAME=70 blender -b --python build_clip.py` renders a single
still to `renders/clips/move/single_0070.png` (skip the full animation while you tune).
`./make_contact.sh` builds a montage of one frame per clip for reviewing them all at once.

## The clips

| # | name | script / where to edit | shows |
|---|------|------------------------|-------|
| 6 | `eat`        | `storyboards.py → eat`        | EAT eats food on a space (1 + 1 created) — bite arcs onto the eater, ends with 2 |
| 7 | `move`       | `storyboards.py → move`       | MOVE relocates a fellow element (grower) onto center; plasma highlights the move |
| 8 | `grow`       | `storyboards.py → grow`       | two growers spend a food → new element appears |
| 9 | `circulate`  | `storyboards.py → circulate`  | half a food stack glides to a non-adjacent element |
| 10| `conflict`   | `storyboards.py → conflict`   | adjacent enemies resolve (move beats eat) → loser becomes food |
| 11| `perish`     | `storyboards.py → perish`     | an organism with no EAT dies → piles of food |
| 3 | `three_org`  | `storyboards.py → three_org`  | last element completes the 3rd organism; all glow |
| 5 | `two_org`    | `storyboards.py → two_org`    | one player, two organisms — highlight each in turn |
| 2 | `board`      | `storyboards.py → board`      | full real game state (`from_ogf: 144`), slow drift |
| 12| `glow`       | `storyboards.py → glow`       | same board, one element highlighted (`glow_space`) |
| 4 | `power`      | `build_power.py`              | score track: 3 tokens/player, score = SUM of ring positions; 4 staggered advances (orange 4→5, purple 4→5, green 0→1, purple 0→1) → green 4 / orange 5 / purple 6; each move carries a player-tinted plasma bell |
| 1 | `box`        | `build_box.py`                | 3D box product shot (cover + hero pieces) → also `organism_title.png` |

## Editing a board clip (`storyboards.py`)

A clip is a list of **beats** the board passes through; the engine glides between them
(Bezier-eased), pops elements in/out by scale, stacks/glides food, and runs a slow
drifting camera. One beat:

```python
{"t": 90, "pos": {"m": "C:0"}, "food": {"m": 1}, "free": {"B:0": 2}, "glow": ["m"]}
#  frame   element id -> space   food on element   loose food on board   highlighted ids
```

- `actors = {id: (color, type)}` — `color` ∈ green/red/purple/blue/yellow/dark, `type` ∈ eat/move/grow.
- `food_actors = [{"id":.., "k": stack_idx, "keys": [(t, space), ...]}]` — a food token that
  glides (e.g. the EAT bite, the CIRCULATE transfer). Lands on a piece's stack if one occupies
  that space at that beat, else rests on the board.
- `cam = {az, el, dist, lens, targz, drift:{az,el,dist,targz}}` (degrees / mm) — each clip a
  different angle + a small eased drift.
- `from_ogf: <turn>` builds a static state from a real frame of `../ogf/zach-dan-ryan.json`;
  `glow_space` highlights one element and frames the camera on it.

Spaces are real board ids; central adjacencies are listed at the top of `storyboards.py`.

## Look knobs (env vars)

`SAMPLES` (48) · `RES` (1080) · `BOARD` (override board image) · `BOARDSAT` (1.55) /
`BOARDVAL` (1.12, counter mipmap wash) · `BLOOMTHRESH` (2.5) / `BLOOMSTR` (2.0) /
`BLOOMSIZE` (0.9) · `BLOOM=0` to disable · `FRAME=<n>` single-still mode.

**Plasma highlight** — the golden "fire" marking a glowing/moving element (one smooth bell
that brackets the element's move; `PLASMA=0` reverts to the old flat ring): `PL_R` width ·
`PL_H` height · `PL_OPAC` transparency · `PL_STR` glow · `PL_NSCALE` shape size (low=big) ·
`PL_SPEED`/`PL_MORPH` rise/shimmer speed · `PL_LEAD` frames the bell leads/trails the move.

## Dependencies (regenerable)

- **Blender 5.1.1** at `~/Downloads/blender-5.1.1-linux-x64/` and **ffmpeg**.
- Piece grafts `out/{EAT,MOVE,GROW}_graft.obj` → `make grafts`; food `renders/food/FOOD_slip.obj`
  → `make food`. Both gitignored but rebuilt from tracked source (`meshlib/`, `inputs/*.svg`, `sor.py`).
- `clip_assets/` (tracked): `board_hex.png` (current 27_HEX board, 3072²), `power_board.png`
  (ScoreCard), `box_top.png` + `box_wrap.png` (box cover; re-crop the wrap if needed),
  `player_aid.png`. Sourced from the current game-asset drop (`~/Downloads/current version-*.zip`).

Rendering/API gotchas (EEVEE 5.1, Standard transform, shadeless board, ring glow, Glare
sockets) are documented inline and in the auto-memory `reference_eevee_51_gotchas`.
