# Fixed-opponent benchmark

The rolling self-play chart measures game endings, not model strength. This
benchmark freezes candidate weights and evaluates them against an unchanged panel,
with a fixed executable, search budget, seeds, board size and cutoff policy.

- 2p/3 rings: initial iteration 379 and control iteration 399; 32 games against
  each opponent, 16 in each candidate seat (64 total).
- 3p/4 rings: historical iterations 385 and 625; 48 games against each opponent,
  16 in each candidate seat (96 total). Both other seats use that opponent's
  weights. This does not yet test mixed opponent pairings or general multiplayer
  robustness.
- 64 simulations per choice, ten opening sampling rounds, 4000-choice limit,
  repetition threshold 3, neutral horizon values for every participant.
- Four shards per opponent; every shard balances candidate seats. Seeds are fixed
  across candidates. Do not rank candidates using partial results: short games
  finish first. Cutoffs stay separate from losses and wins.

`python3 -u native/run-benchmarks.py --watch` freezes each model's next available
completed checkpoint once its previous benchmark finishes and it has advanced
at least 100 iterations. Candidates may skip intervening checkpoints to keep a
bounded queue. Evaluation is sequential in one nice +10, two-thread native
process alongside production. It consumes GPU time, so training throughput may
fall while it runs. Each model's seed/opponent panel remains unchanged.
The executable and weights are copied into the benchmark root and SHA-256 hashes
recorded. Model replacement or curriculum changes require a new benchmark series;
iteration numbers alone cannot distinguish training branches.

Files live under `checkpoints/organism-benchmark-20260913`; `protocol.json` is the
fixed configuration, `status.json` identifies the active batch, and each batch
has its manifest, resumable session and report. The runner publishes an atomic
`checkpoints/organism-native/benchmarks.json`, served at `/api/benchmarks` and
rendered under “Fixed-opponent benchmark” in the viewer. Rows show separate
opponent results rather than merging unlike tests into an overall rating.

Touch `checkpoints/organism-benchmark-20260913/STOP` to save and stop evaluation
without stopping training. Remove STOP and rerun the command to resume. A lock
prevents concurrent runners. A process interrupted without graceful STOP can
resume from the latest persisted batch session.

This panel can detect changes on known opponents, but is not a broad skill rating
or proof of a recipe's causal benefit. Subsequent controlled recipe experiments
should start from identical checkpoints, use equal update budgets, and eventually
include repeated training seeds and a held-out opponent/opening panel.
