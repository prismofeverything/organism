# Training orchestration measurements

Measured on the Ryzen 9 3900X (12 cores / 24 hardware threads) and RTX 5060 Ti
16 GB. The production trainer remained running during these short benchmarks;
there is interference from its GPU work and other desktop processes. These are
screening measurements, not isolated hardware capacity limits.

The fixture freezes the three-player iteration-5 checkpoint and samples current
unfinished positions plus positions distributed through replay. Every case
searches the same 64-position workload with the same weights, 64 simulations,
no root noise, and two libtorch CPU threads. Each batch shape is warmed up; two
passes use opposite batch-size order. Rates below aggregate both passes.

| Search workers | Batch | Choices/sec | Prepare leaves | Pack input | Inference path | Backup |
|---:|---:|---:|---:|---:|---:|---:|
| 4 | 8 | 120.1 | 8.4% | 3.6% | 85.8% | 0.9% |
| 4 | 16 | 198.7 | 11.2% | 5.5% | 79.8% | 1.5% |
| 4 | 32 | 290.0 | 13.9% | 8.7% | 72.4% | 2.1% |
| 4 | 64 | 321.1 | 14.0% | 11.1% | 68.7% | 2.7% |
| 8 | 8 | 95.7 | 5.9% | 2.3% | 90.3% | 0.6% |
| 8 | 16 | 167.6 | 7.4% | 3.6% | 86.4% | 1.0% |
| 8 | 32 | 307.1 | 9.8% | 6.5% | 78.6% | 1.9% |
| 8 | 64 | 343.0 | 9.4% | 9.7% | 74.7% | 2.6% |

Inference time is host wall time around input construction, transfers, forward
inference and result readback. Readback synchronizes with CUDA. It is **not** a
kernel-only GPU time measurement, and cannot separate launch overhead from
GPU execution or contention. The remaining fraction includes tree creation,
control calls, noise and policy construction. Sampling with Linux perf was
unavailable under the host's existing performance-monitoring restrictions.

Increasing the batch clearly helps in these samples. More workers alone does
not consistently help; small-batch results are particularly sensitive to
concurrent GPU activity. Production remains at 16 actors / 4 search workers.

A future inference service can accept requests from independently progressing
games, coalesce compatible requests into bounded batches, and send results back
to their games. CPU workers can then prepare other games during GPU execution.
The queue needs a maximum wait, backpressure, explicit model/version ownership,
and graceful shutdown/checkpoint handling. Requests for the differently shaped
two- and three-player networks must be separated. Initial experiments should
retain one outstanding leaf per game to avoid silently changing MCTS through
parallel searches within the same tree. Increased throughput must be checked
against game outcomes and fixed-opponent performance.

Also corrected unbuffered JSON writing and reading in checkpoint/telemetry code.
An earlier eight-second production sample made 703,629 writes for 1,164,522 bytes;
after buffered writes, a sample made 82 writes for 1,239,649 bytes. Flush and fsync
remain before checkpoint publication. Reads now use BufReader, avoiding one
system call per tiny JSON read during model switching and restart.

At the time of the behavior check, 2p self-play had 96 repetition cutoffs and no
wins. 3p self-play had 20 wins, 54 repetition cutoffs and 6 length cutoffs. The
iteration-5 three-player frozen-opponent evaluation had 6 cutoffs and no wins.
These are different populations; self-play wins do not establish improvement
against the frozen opponent. Recent full recordings confirm turns pass among
all players, with frequent explicit passes, eating and circulation.

Reproduce a profile on a model directory (use a frozen copy for comparisons):

```sh
native/target/release/organism-train benchmark CHECKPOINT_ROOT/3p 4
native/target/release/organism-train benchmark CHECKPOINT_ROOT/3p 8
```

The benchmark emits JSON lines including checkpoint identity and phase timings.
It does not update model weights or alter production settings.


## September 12 follow-up

Production now uses `--actors 16 --concurrent-games 32`: 32 live self-play
actors, still at least 16 finished games before 100 gradient updates. This keeps
the update quota independent of GPU batch size (simultaneous finishes can still
overshoot the quota). Input packing now reuses its allocation across MCTS
simulations. Persistent `search_timings` record preparation, packing, inference,
and backup in iteration metrics; old partially completed iterations only have
profiling coverage after the upgrade.

The updated frozen benchmark with production paused measured 108, 110, 270,
and 320 choices/sec for batches 8, 16, 32, and 64 respectively. Batch 16 was
noisy; other applications were active. Do not interpret this as an isolated
before/after speedup for input-buffer reuse. At batch 32, search time was 15.0%
leaf preparation, 5.7% packing, 73.3% inference including transfer/readback, and
2.3% backup. These measurements exclude checkpoint serialization.

The first new 2p iteration recorded 159 choices/sec: 4.20 seconds search out of
6.04 seconds iteration time, including 3.05 seconds inference, 0.85 preparation,
0.10 packing, and 0.11 backup. Iteration time excludes its final checkpoint save
and model switching; the remainder is not a serialization-only measurement.
During a later production sample the machine became 99.1% busy while the trainer
received 1.11 logical CPUs, with GPU utilization mostly 1–24%. This is CPU
contention, not evidence that the larger batch inherently slows the GPU. The
trainer retains nice +10 to yield to interactive work. An earlier evaluation
sample used approximately one CPU; evaluation had shrunk to one remaining game.

Quality is not yet demonstrated: the last 80 completed games for each active
model (2p-r3 and 3p) had zero rule victories. The 2p iteration-5 and 3p iteration-10
fixed-opponent tests were all cutoffs. Three-player evaluation took 195 seconds,
during which self-play was blocked. Low value loss largely reflects zero-valued
cutoff targets.

Highest-priority follow-ups:

1. Align MCTS loop/horizon handling with the real game runner; currently only
   the outer runner tracks repetition. Audit exploration measured in full turns:
   the existing 30-choice sampling window counts menu-level decisions.
2. A bounded inference queue to overlap CPU work with GPU execution and schedule
   evaluation alongside training, with explicit frozen model/version ownership.
3. Retain/reuse searched subtrees and avoid regenerating all legal children just
   to apply the selected action. Verify root-noise/visit semantics before reuse.
4. Profile checkpoint save/load separately; current model switching reparses
   JSON into a generic Value then typed state and reloads replay each iteration.
   Consider persistent model state or direct typed streaming before changing
   the durable format. Live history is already bounded to 64 frames.

The 32-actor/16-quota scheduling change passed exact interrupted-vs-continuous
resume comparison (test uses four active games/two-game quota), and the native
unit and board-size weight-transfer tests passed.


## Pipelined search, retained trees, and background evaluation

All four follow-ups above now have an implementation. The production launcher
uses 64 active games, fixed inference cohorts of 32, four Rust search workers,
and the existing 16-finished-game update threshold. The bounded queue overlaps
CPU leaf preparation with GPU inference while preserving one outstanding leaf
per tree. It does not yet combine requests from different network versions into
a single inference call. Background evaluation receives bounded service slices
between self-play searches and optimizer steps; its own candidate and opponent
snapshots remain immutable. Only one evaluation per model is pending at a time.

A consecutive frozen-workload comparison, with production paused, measured:

| Active positions per search | Previous single batch, choices/sec | Pipeline (two half-size cohorts), choices/sec |
|---:|---:|---:|
| 8 | 125.9 | 71.7 |
| 16 | 211.1 | 130.1 |
| 32 | 321.6 | 220.5 |
| 64 | 349.9 | 347.8 |

Small inference cohorts are counterproductive on this network. Consequently the
production trial uses 64/32, not 32/16. Against the previous 32-position production
layout the frozen result is about 8% faster, but a single 64-position batch is
about equally fast. This does not establish a throughput advantage of pipelining
over every alternative. Queue support also enables CPU/GPU overlap and independent
cohort progress. CPU timings overlap inference now; do not sum phase fractions
as if they were disjoint wall time. Raw files: `/tmp/organism-pipeline-baseline.jsonl`
and `/tmp/organism-pipeline-queued.jsonl`. Other desktop work was still present.

After activation, a production sample used 2.70 logical CPUs, with search workers
at roughly 22–35% each and the machine 27% busy. GPU utilization often reached
50–66%, with dips, and total GPU memory was about 863 MiB. Trainer resident RAM
was about 2.1 GiB. A completed three-player iteration recorded 245 choices/sec
and 5 victories out of 16 endings; a two-player iteration had 1/16 victories.
These are early observations with carried games, not a controlled quality result.

Direct typed checkpoint load took approximately 0.14 seconds for 2p and 1.24
seconds for 3p. Saves took 0.25 seconds for 19 MiB and 1.14 seconds for 126 MiB
respectively. Resident model/replay bundles remove these loads from normal
alternation. These byte counts include pending games and can grow; at most two
training generations and two full completed evaluation fixtures are retained.

Quality changes: exact repetition and choice horizons are now recognized inside
MCTS as well as the outer runner. Selected subtrees are reused until weights
change, with fresh root noise derived from clean priors. Sampling spans ten board
rounds instead of thirty menu choices. Food remains in the repetition key:
unchanged piece layout/captures across rounds is diagnostic only, not a cutoff
or reward penalty. Decay now has a learning-rate floor of 1e-4; at the observed
300+ iteration counts the previous unbounded schedule had nearly disabled
learning. New evaluation results use the new search protocol for both sides.

Validation passed: 19 native unit tests, board-size spatial weight transfer,
exact interrupted training (including retained trees and multiple GPU cohorts),
resumable evaluation that overlaps self-play and preserves frozen weights, a
small CUDA training/evaluation smoke run, and browser live-follow/seek/pause checks.
The fixture-dependent Python/CUDA parity test was skipped in this pass; it passed
earlier and the network/optimizer math was not changed here. The live dashboard
also handles recordings pruned between directory enumeration and metadata reads.

A final worker-failure test confirms preparation panics propagate as errors rather
than leaving the GPU coordinator waiting indefinitely. After enabling the learning
rate floor, subsequent iterations recorded roughly 242–249 choices/sec (one 3p
victory and two 2p victories out of 16 endings each); these remain early samples.
