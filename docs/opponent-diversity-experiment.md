# Opponent-diversity experiment — September 13, 2026

This tests whether current-model-only self-play is limiting improvement. It does
not assume a training fix has already been demonstrated. Production recipes and
models remain unchanged while four isolated branches train.

| Pair | Initial checkpoint | Both branches' replay settings |
| --- | --- | --- |
| 2p / 3 rings | 421 | Capacity 5000, no per-game cap, neutral cutoff values |
| 3p / 4 rings | 842 | Capacity 32768, cap 256/game, masked length-cutoff values |

**The 2p pair is held after round 7 and its results should not be read.** Both
2p branches inherited the uncapped production replay recipe, and both collapsed
to a mean of 5 (`history_mix`) and 7 (`self_play`) distinct games in the buffer
with mean game lengths of 1448 and 1805 choices. Whatever opponent diversity
does in 2p is swamped by that; see `docs/two-player-replay-collapse-20260913.md`.
The 3p pair is unaffected — 172 and 156 distinct games — and continues. Advance a
subset with `--models`, for example `--models 3p`. Relaunch the 2p pair from a
fresh root once the corrected 2p recipe has settled; `prepare` now caps every
pair's per-game sampling at 256.

Within each pair, `self_play` and `history_mix` begin with identical weights,
Adam optimizer, replay, RNG, and unfinished games. Both run 50 iterations of 100
updates. Branch order rotates each round; one experiment child runs at a time,
nice +10, two search threads, 35% work duty and 20% CUDA memory limit. The separate
production trainer and fixed benchmark continue to share the GPU.

The treatment assigns roughly half of games to historical opponents using bits
from the persisted random game identity. The learner seat and historical model
are also derived from that identity. Thus assignments persist without extra draws
from the search RNG. In historical 3p games both other seats use the selected
historical network. Other games remain ordinary self-play. Assignments are
approximately balanced, not a quota within each training iteration. Inherited
unfinished games acquire the treatment on first resume, equally preserving their
pre-existing history in both branches.

2p training opponents are frozen iterations 540 and 785 from the displaced
production branch. 3p training opponents are iterations 345 and 605. Their weight
hashes are recorded. Only learner-seat decisions from historical games enter new
replay: the learner does not imitate frozen opponent policy targets. The full
sequence still counts for cutoffs and remains in OGF recordings. Consequently
sample yield differs between arms; equal gradient updates do not imply equal
positions or compute. Existing replay is not discarded.

Search batches are grouped by the acting seat's network. Tree reuse is invalidated
when network ownership changes. As in the existing evaluation engine, each actor
searches with its own network throughout its search; this is not explicit
opponent-aware planning inside the search tree. Opponent identity is not a new
network input, and all rules, rewards, search budgets and exploration settings
remain unchanged within each pair.

After 20 and 50 iterations, each frozen endpoint plays the held-out fixed panel:
2p versus 379 and control-399, 32 games each; 3p versus 385 and 625, 48 games each.
These weights are disjoint from the training pool. Both branches use seed 830013,
64 simulations, balanced candidate seats, neutral horizon values, and separate
win/loss/cutoff reporting. These are new opening seeds relative to the live
benchmark, so compare experiment arms within their own reports. No automatic
model promotion occurs. One trajectory per arm is a screen, not evidence of a
small general improvement; larger or repeated-seed confirmation may be needed.

Run/resume:

```sh
python3 -u native/run-diversity-experiment.py
```

Artifacts: `checkpoints/organism-diversity-20260913/experiment.json` records initial
hashes and recipes; `status.json` identifies active work; `progress.json` records
completed rounds; each model/arm has native checkpoints, training logs and
`evaluation-020` / `evaluation-050` reports. Initial generation files are pinned
with hard links because native snapshots are immutable, avoiding redundant disk
copies. Training keeps the native two-generation retention policy.

Touch `checkpoints/organism-diversity-20260913/STOP` to gracefully stop its child;
remove STOP and rerun to resume. A lock prevents duplicate orchestration. Less
than 3 GiB available space also requests a graceful experiment stop, leaving
production untouched and reserving room for checkpoint writes.

Validation: 26 native unit tests; 2p/3p historical routing, learner-only replay,
frozen-weight and iteration-restart determinism integration checks; graceful
mid-game resume check; tiny four-branch experiment verifies equal updates,
held-out evaluations and idempotent orchestration restart.
