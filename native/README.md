# Native Organism training

Rust owns the rules, legal moves, encoding, batched MCTS, replay and training loop.
The network and Adam tensor operations call C++ libtorch/CUDA through `tch`.
Python is needed only for the existing reference tests, optional weight import,
and the lightweight local dashboard server. It does not execute native self-play
or training.

Large training artifacts are stored on `/mnt/data/archive/organism-training`,
with symlinks preserving the paths below. See [storage details](../docs/training-storage.md)
and the [opponent-diversity experiment](../docs/opponent-diversity-experiment.md).

## Run, pause and resume

From the repository root:

```sh
bash native/build-gpu.sh --offline
bash native/start-training.sh >> checkpoints/organism-native/training.log 2>&1
```

The installed runtime is `.venv-training`'s PyTorch 2.9.1+cu128. `tch` 0.22
nominally expects 2.9.0; the build explicitly bypasses the maintenance-version
check. CPU and CUDA numerical comparisons against this installed runtime pass.
The small CUDA memory-control shim also uses headers from
`nvidia-cuda-runtime-cu12` and `nvidia-cuda-nvcc-cu12`. A different installation
must provide compatible headers, libraries and C++ ABI; rebuild and rerun checks
after changing those dependencies. CUDA is retained explicitly in the link so
its kernels register even when the linker would otherwise drop the library.

Defaults: separate 2/3-player networks, a three-ring two-player curriculum and
a four-ring three-player board, 4 residual blocks × 64 filters,
64 MCTS simulations per choice, 64 concurrent games (GPU cohorts of 32), four Rust search workers,
two libtorch CPU threads, 100 gradient updates per iteration, batch 64, replay
capacity 5,000 positions per model. Each iteration completes at least 16 games.
Finished games are replaced while that quota is pending. Unfinished games carry
across updates; their search targets may therefore come from several recent
network versions. This is deliberate recent-policy replay, not synchronized
fixed-model episodes. Two- and three-player iterations alternate.

The process runs at nice +10 and caps its CUDA caching allocator at 35% of VRAM
(about 5.6 GiB on this 16 GiB card; other CUDA allocations can be additional).
It does not reserve CPU cores or automatically detect rendering. Stop before a
large render or when desktop latency matters:

```sh
touch checkpoints/organism-native/STOP
# Wait for “Saved model, optimizer, replay and unfinished games; exiting.”
tail -f checkpoints/organism-native/training.log
```

Then remove the marker and run the same launcher again:

```sh
rm checkpoints/organism-native/STOP
bash native/start-training.sh >> checkpoints/organism-native/training.log 2>&1
```

SIGINT/SIGTERM also save and exit. Search checks for stop between simulations.
The unfinished search is recomputed on resume; completed choices, pending games,
repetition counts, replay, Adam moments/step and RNG state are preserved.
Checkpoints are written every two minutes of self-play, on iteration completion,
and on graceful stop. Two immutable generations are retained and selected by an
atomic `latest.json` pointer. A killed/crashed process resumes from the latest
saved generation. CPU resume is tested deterministically; GPU floating-point
execution is not promised bit-for-bit identical on every driver/device.

For reduced load, append `--duty 0.5` or `--threads 2` to the launcher. These and
the memory fraction can change on resume. Model/search/replay configuration is
checked against the checkpoint; use a separate checkpoint root for experiments.
`--forever` lasts while the process runs. There is no reboot service installed.

## Dashboard and game replay

```sh
python3 -m alphazero.dashboard --checkpoint checkpoints/organism-native
```

Open http://127.0.0.1:8765 on this machine, or keep the existing SSH forward to
port 8765. The viewer uses the site's board renderer, SVG pieces, food placement
and generated board/player colors. Choose **Live now** to follow an active self-play or evaluation game across both
player counts. Positions publish and refresh up to four times per second. The
live feed retains the most recent 64 frames to keep updates small; completed
self-play recordings contain full histories. Scrubbing pauses live following;
check **Follow live position** to catch up. Up to 100
completed OGF v1 recordings per player count are retained. OGF is for viewing;
full native training recovery uses the checkpoint state, not OGF.

The dashboard reports ended games/hour, **rule victories/hour**, decisions/sec,
gradient updates/hour, losses and separate cutoff counts. Rates use time spent
inside recorded self-play/training iterations, including throttling; they exclude
evaluation, model loading, downtime and final checkpoint writes. Counts cover the
last 500 recorded iterations. They are not 24-hour machine-wide utilization rates.
Update counts and falling loss do not establish stronger play.

After the first iteration and every five completed iterations, the candidate plays frozen starting weights
at the same simulation budget. Every seat gets two games (4 games for 2p, 6 for
3p). All opponent seats in a three-player game use the frozen network. Search
root noise is off; the first 30 decisions sample visit policies with fixed
per-game seeds to vary openings. Wins, losses and repetition/length cutoffs are
reported separately in `evaluation.json` and `evaluations/`. These are small
screening samples, not statistical proof of improvement or an Elo rating.
Evaluation games never enter replay. Increase `--eval-games-per-seat` for stronger
evidence, or use `--eval-every 0` to disable these checks. The baseline is not
automatically promoted; this keeps the comparison stable across iterations.

Games stop on a rule victory, three occurrences of the same decision state, or
4,000 choices. Cutoffs receive neutral value targets and retain search-policy
targets. A high cutoff fraction is a learning diagnostic, not a victory.

## Board-size curriculum

The launcher enables `--rings-2p 3 --curriculum-2p`. Two-player training starts on
19 spaces in `2p-r3`; the original 37-space `2p` checkpoint remains a saved
comparison. Three-player training continues in `3p` on four rings.

The curriculum considers a one-ring expansion only when each of the last three
iterations has at least 60% rule victories and those iterations contain at least
48 completed games. At a scheduled evaluation, it then plays at least 32 games
against frozen starting weights, balanced across both seats. Expansion requires
no more than 25% cutoffs and a 95% Wilson lower bound for wins/all games above
50% (at least 22 wins in a 32-game test). These are initial engineering screening
thresholds; repeated tests are not independent and do not provide a formal
95% guarantee of strategic competence. Lower loss alone never triggers expansion.

On qualification, the next two-player iteration uses the next ring count, up to
`--curriculum-max-rings 7`. Progress is stored in `curriculum.json`; every stage
has its own directory (`2p-r3`, `2p-r4`, ...), frozen opponent, optimizer, replay,
and histories. Previous stages and unfinished games remain saved. Spatial
convolutions and batch-normalization parameters transfer; all dense policy/value
layers are newly initialized for the larger board. This preserves learned local
features, not guaranteed playing strength. New-stage optimizer/replay start fresh.

This curriculum currently applies only to two-player board size. A future
three-player curriculum can use the same approach. Transfer to five players also
changes input channels and value outputs, and is not implemented by this helper;
it needs explicit remapping and separate evaluation. The long-term five-player,
seven-ring objective is not evidence that small-board success will transfer intact.

## Migration and verification

The Python checkpoints remain in `checkpoints/organism`. Migration imports only
network weights; native replay and optimizer state start fresh. The source model
iteration is recorded alongside each exported file:

```sh
.venv-training/bin/python native/import_python_weights.py checkpoints/organism checkpoints/organism-native/imported
```

Native warm start is used only when no native `latest.json` exists. The frozen
baseline is copied from those starting weights and remains unchanged.

```sh
bash native/test-gpu.sh --offline --lib
.venv-training/bin/python tests/check_native_parity.py
python3 tests/check_native_resume.py
.venv-training/bin/python tests/native_network_fixture.py /tmp/native-network-cpu
ORGANISM_NETWORK_FIXTURE=/tmp/native-network-cpu bash native/test-gpu.sh --offline --test network_parity -- --ignored
.venv-training/bin/python tests/native_network_fixture.py /tmp/native-network-cuda cuda
ORGANISM_NETWORK_FIXTURE=/tmp/native-network-cuda ORGANISM_TEST_CUDA=1 bash native/test-gpu.sh --offline --test network_parity -- --ignored
```

Parity covers 1,185 sampled positions (2p on three/four rings and 3p on four rings), every legal child board, legal
indices and encoded inputs. Dedicated regressions cover introduction clearing
all three homes while preserving adjacent food, circulation transferring half
rounded up, capture ties and cyclic multiplayer value backup. Network tests
compare inference and every parameter after two Adam updates, with native save/
reload between updates. CPU is compared with CPU and CUDA with CUDA: backend
rounding near activation boundaries can change tiny gradients, so CPU-vs-GPU
parameter identity is not an appropriate optimizer check.

Reproduce the small search benchmark:

```sh
.venv-training/bin/python tests/benchmark_native.py
native/target/release/organism-train benchmark /tmp/organism-search-benchmark/fixture.json
```

On the RTX 5060 Ti, the same four positions and weights at 64 simulations yielded
8.4 choices/sec in Python, 17.9 in native batch 1, 66.7 in batch 4, and 208 in
batch 16. These are warmed-up search measurements, not completed games/hour or
learning-speed claims. Real game lengths and outcomes are measured by the live
training dashboard. The former serial Python production run, at 35% duty,
completed four 2p games in 2,680 seconds (three rule wins and one repetition).
That is a useful historical reference, not a controlled comparison with native
full-duty training and its different sampling/replay behavior.

`--concurrent-games` controls the active self-play pool independently of the
`--actors` finished-game quota (defaults to that quota when omitted). Production
uses 64 active games and a 16-finished-game threshold, with the existing 100
updates per iteration. Carried games are preserved on restart; changing pool
size changes scheduling and RNG consumption, not model/checkpoint dimensions.
Iteration metrics now include `search_timings` (leaf preparation, input packing,
inference including transfers/synchronization, and backup) and pool size. Old
partial checkpoints have no earlier phase timings, so their first resumed
iteration has only partial profiling coverage.


The production search pipeline uses `--gpu-batch 32 --exploration-rounds 10`.
Exploration samples visit targets for the first ten full board rounds; zero
selects the older 30-choice window for comparisons. `--legacy-search` and
`--no-tree-reuse` support ablation runs. These flags are recorded in metrics and
saved state. Changing settings invalidates retained trees but preserves weights
and replay. No food-accumulation penalty or additional cutoff was introduced:
`unchanged_layout_rounds` is only a diagnostic and exact repetition includes food.

Search evaluates the same historical/path repetition and choice horizon as the
outer game runner, with victories taking precedence. The CPU/GPU pipeline uses
fixed cohorts and a bounded channel, one pending leaf per game, and one owning
thread for network inference. Cohorts preserve per-game ordering and batch
composition. Phase durations can overlap, so their sum is no longer a partition
of total wall time. Root noise is regenerated from clean priors each decision.
Selected subtrees are pruned/reindexed and reused; trees larger than 4096 retained
nodes are discarded. Trees are persisted for exact resume and invalidated before
optimizer updates. Search completes transactionally before episode/RNG commit.

In `--forever` mode, evaluation is resumable background work: one evaluation
slice after eight self-play batches and after each twenty gradient updates.
Each job owns immutable candidate/opponent snapshots, so later training cannot
change its matchup. A pending job is completed before scheduling another for
that model. Evaluation progress appears separately in the dashboard; the live
board follows self-play. Finite runs drain their pending evaluation for validation.
Evaluation jobs persist alongside training checkpoints. Two full completed
fixtures are retained per model; lightweight reports remain in `evaluations/`.
Curriculum promotion transfers the tested frozen candidate, not newer untested
weights. Evaluation reports identify the new search/exploration protocol; they
are not directly comparable with earlier protocol results.

Both model/replay/optimizer bundles remain resident between iterations, within
the existing shared CUDA allocator cap. Checkpoints are read directly into typed
state through a buffered stream. Save/load durations and serialized bytes are
written to `checkpoint-timing.json` and `load-timing.json`; the dashboard shows
save cost. Pre-upgrade model snapshots were preserved under
`checkpoints/organism-native-archives/pre-pipeline-20260912`.

Validation: `python3 tests/check_native_resume.py` and
`python3 tests/check_native_background.py` cover exact interrupted training and
durable evaluation that overlaps self-play. Native unit tests cover search
cutoffs, cohort ordering, tree reuse, root noise, cancellation and frozen weights.

Learning-rate decay now has a floor of `1e-4` (initial rate remains `1e-3`).
Previously, hundreds of mostly cutoff iterations reduced the rate below `1e-8`
before competence. Metrics record the effective learning rate. The floor keeps
updates active; it is not evidence of improved playing strength.
