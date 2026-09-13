# Replay/cutoff ablation

Started from the preserved two-player, three-ring iteration-379 checkpoint.
Production training continues independently. Experiment files live in
`checkpoints/organism-ablation-20260912`.

| Branch | Replay capacity | Maximum positions per game | Length cutoff value |
| --- | ---: | ---: | --- |
| control | 5,000 | unlimited | draw |
| replay_only | 32,768 | 256 | draw |
| cutoff_only | 5,000 | unlimited | masked; search bootstraps |

Every branch retains search protocol 3's verified prior/tie correction. The replay
arm tests the replay-policy bundle (capacity and per-game cap), not those two
parameters independently. The cutoff arm includes both supervision and search
horizon treatment, including conservative masking of unidentified legacy neutral
samples. These component comparisons are narrower than the original combined
production change, but do not isolate each internal mechanism.

The initial model, Adam state, replay, unfinished games, and RNG state are identical
across arms. Files and the native executable are frozen and SHA-256 hashes recorded
in `experiment.json`. Inherited games may finish under the new arm settings; the
same inherited state is supplied to every arm. Curriculum expansion and background
evaluation are disabled in these training branches.

Each branch completes 20 further iterations, each with 100 gradient updates, for
2,000 updates. The orchestrator runs one iteration of each branch at a time and
rotates branch order each round. Every arm uses 64 simulations per choice, 64
concurrent games, two search threads, 50% training duty and a 20% CUDA memory limit.
Only one experimental native process runs at once alongside production.

After 5 and 20 iterations (500 and 2,000 updates), each frozen endpoint plays 16
games against the same preserved iteration-379 opponent: eight in each seat, 64
simulations per choice, identical opening seed schedules and draw horizon handling
for both sides of every evaluation. Evaluation runs at normal GPU service speed
with nice +10 and two CPU threads. Results are saved under each branch's
`evaluation-005/report.json` and `evaluation-020/report.json`. No experiment result
automatically replaces or promotes a production model.

This is one initial training trajectory per arm. The comparisons can identify
large problems but are not sufficient to establish a small improvement. Training
loss, self-play completion, and opponent win rates measure different things. Equal
update budgets do not imply equal generated positions or wall-clock compute; those
quantities remain recorded in branch metrics and should be reported alongside
results. Multiple training seeds and additional frozen opponents would be the next
confirmation if one treatment looks better.

## Running and stopping

Start or resume with:

```sh
python3 -u native/run-ablation.py >> checkpoints/organism-ablation-20260912/orchestrator.log 2>&1
```

The orchestrator locks its root, skips completed work and resumes native checkpoints
and evaluation sessions. `status.json` names the active stage/branch;
`progress.json` records completed rounds. During training, the active branch's
`2p-r3/status.json` supplies live choice progress. The main production dashboard
continues showing production, not the experimental branches.

To stop this experiment gracefully:

```sh
touch checkpoints/organism-ablation-20260912/STOP
```

Remove that experiment-level STOP file before resuming. The orchestrator forwards
it to its active native process. Production has its own independent STOP file.
To free the GPU for a render, stop both processes using their respective STOP
files and wait for their saved/stopped statuses.

Validation: `tests/check_native_ablation.py` completed all three tiny CPU branches,
checked identical initial hashes and equal optimizer-update budgets, ran both
frozen evaluation milestones, exercised STOP/resume, and verified that a rerun
does not add training iterations or repeat completed evaluations.
