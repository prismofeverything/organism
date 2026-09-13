# Two-player regression check

The protocol-3 switch combined a search correctness fix, a replay-distribution
change, and a cutoff-training/search change. Frozen-opponent evaluation itself
does not train the model; it changes measurement and can affect scheduling and
curriculum promotion. Mechanical tests did not establish improved learning.

## Preserved evidence

`checkpoints/organism-comparison-20260912/old` preserves the iteration-379 model,
Adam state and replay. `current` preserves iteration 407. Weight SHA-256 hashes
and generation origins are recorded beside the comparison manifests. These are
immutable snapshots independent of the continuing production run.

The first diagnostic is eight head-to-head games per treatment, with each model
in each seat. Both receive 16 simulations per choice and the same game rules,
opening sampling and seed schedule. Mask and draw cutoff treatments run
sequentially. This is a bounded screening experiment, below production's 64
simulations, not enough games to establish a small strength difference or prove
which training change caused a regression. In particular, a model can be stronger
against its predecessor while its mirror self-play becomes less decisive.

Run or resume either with:

```sh
export LD_LIBRARY_PATH="$PWD/.venv-training/lib/python3.12/site-packages/torch/lib"
nice -n 10 native/target/release/organism-train compare "$PWD/checkpoints/organism-comparison-20260912/mask/manifest.json"
```

Use `draw/manifest.json` for the other treatment. Each directory records its
manifest, resumable session, and report including whether all games are complete.
A STOP file in that directory stops only the comparison. The production training
process and its checkpoints are separate. The comparison does not feed gradients
or promotion decisions into production.

## Movement audit

The 30 games selected by most recent *completion time* contain 4 victories and
26 repetition cutoffs. Movement was executable at 3,867 mode decisions and Move
was chosen at 416 of those (10.8%). Recorded action selections were 1,697 Eat,
485 Circulate, 297 Move, 87 Grow, and 2,070 Pass. Only one pass had no executable
alternative. One game kept layout and captures unchanged for 293 rounds.

These counts substantiate stagnation. They do not establish a universal action
ratio: completed-game sampling excludes ongoing long games, availability is
state-dependent, and mode selection and completed movement are different events.
The older audit selected recordings by game identity, so its aggregate percentages
are not a rigorously matched before/after comparison. Full current counts and game
identities are in `action-audit-latest-completions.json` beside the comparison.

## Next learning experiments

Keep the verified first-simulation/tie correctness fix. Establish an isolated
control from iteration 379 with the former replay policy (`--buffer 5000
--replay-game-cap 0`) and former cutoff treatment (`--cutoff-value draw`). Compare:

1. That control versus the same starting checkpoint with only the replay policy
   changed (`--buffer 32768 --replay-game-cap 256`).
2. That control versus the same starting checkpoint with only cutoff treatment
   changed (`--cutoff-value mask`).

Use independent directories, the same initial optimizer state and seed schedule,
and matched gradient-update budgets. Report generated positions, elapsed compute,
rule-victory/repetition/length-cutoff rates, and movement conditional on legality.
Evaluate frozen endpoints against the same frozen opponent pool under identical
search settings, rotating seats; include more than one seed before claiming a
learning improvement. Do not select changes solely on loss or self-play wins.

The head-to-head screening completed with one win, one loss and six repetition
cutoffs in each treatment. The controlled training arms are now running; see
[replay-cutoff-ablation.md](replay-cutoff-ablation.md) for the fixed budgets,
evaluation schedule and stop/resume commands. Production has not been rolled back
or given any new rewards or action restrictions during this investigation.
