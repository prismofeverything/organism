# Organism self-play training

The active GPU trainer now lives in [native/](../native/README.md): Rust rules,
batched search and training through C++ libtorch. Its checkpoints are under
`checkpoints/organism-native`. This Python implementation remains the rule
reference and supports the read-only dashboard. The commands below describe
the original Python trainer unless stated otherwise.


The repo already had a residual policy/value network, multiplayer MCTS, a replay
buffer, Journey and Organism Python game ports, and a Journey ruleset-search
experiment (`oroboros`). Existing checkpoints were only under `checkpoints/journey`.
Those weights do not fit Organism.

## Machine and installation

Verified September 11, 2026: RTX 5060 Ti, 16,311 MiB VRAM, NVIDIA driver
580.178.04. The desktop used about 545 MiB before testing. Sandboxed `nvidia-smi`
failed, while the host check succeeded; no driver repair was needed.

Keep training separate from the piece-generation environment:

```bash
uv venv --python 3.12 .venv-training
uv pip install --python .venv-training/bin/python -r alphazero/requirements.txt
```

The requirements target Linux x86_64/Python 3.12 and pin the official PyTorch
2.9.1 CUDA 12.8 wheel. PyTorch and its runtime libraries are several GB.
[Official installation matrix](https://pytorch.org/get-started/previous-versions/).

## Start both models

From the repository root:

```bash
.venv-training/bin/python -u -m alphazero.train_organism --forever
```

Two-player and three-player iterations alternate, with independent models,
optimizers, replay buffers and random states under `checkpoints/organism/2p`
and `3p`. Only one model trains at a time. Without `--forever`, the command runs
one iteration per model and exits. Use `--players 2` or `--players 3` for one model.
Restarting resumes automatically from `latest.pt`.

The CLI defaults to four rings, sixfold symmetry, no notches, five captures or
three living organisms to win. This matches the app's initial two-player setup
(`board/empty-invocation`); four rings also remain legal when selecting three
players. The older trainer inherited seven rings from its five-player setup.
Preliminary random-play probes reached wins on four rings; seven-ring probes
were much slower. These probes establish that wins are reachable, not that the
network is strong. Completion rates must be rechecked as the rules evolve.

To experiment with seven rings, use a separate checkpoint root:

```bash
.venv-training/bin/python -u -m alphazero.train_organism --forever --rings 7 --checkpoint checkpoints/organism-seven --stop-file checkpoints/organism-seven/STOP
```

Board size changes the policy/network dimensions, so four-ring weights cannot
directly initialize a seven-ring model. Architecture, board setup, player count,
replay capacity and target policy are checked against `config.json`; changing
those settings requires a new checkpoint root.

Background launch with a log:

```bash
mkdir -p checkpoints/organism
nohup .venv-training/bin/python -u -m alphazero.train_organism --forever > checkpoints/organism/training.log 2>&1 &
tail -f checkpoints/organism/training.log
```

## Keep the desktop usable

Defaults: two CPU threads, CPU niceness +10, four residual blocks of 64 filters,
batch 32, replay capacity 5,000, 64 MCTS simulations per choice, four games per
iteration, and a 35% approximate work/sleep duty cycle. Self-play is serial;
Python rule enumeration can be the bottleneck even when a GPU is available.

The 35% CUDA allocator limit is about 5.6 GiB on this card. It is not a GPU
utilization cap or a reservation; CUDA context and other allocations can exceed
that figure. Replay lives in system RAM: about 250 MiB of raw sample arrays for
a four-ring three-player model, rising to about 1 GiB on seven rings, plus Python
and training overhead. Snapshots can reach similar sizes. Five recent weight
exports and one resume snapshot are retained per model; leave several GB free
for atomic snapshot replacement.

If interaction feels slow, try `--duty 0.2 --threads 1 --batch-size 16`. Increase
`--duty` towards 1 for unattended runs. CPU niceness does not prioritize GPU work,
and yielding happens between operations. Check responsiveness under your actual
browser workload. [PyTorch allocator limit documentation](https://docs.pytorch.org/docs/stable/generated/torch.cuda.memory.set_per_process_memory_fraction.html).

## Stop for rendering and resume

```bash
touch checkpoints/organism/STOP
```

Wait for `Saved training state; exiting to release GPU memory.` in the log and
confirm with `nvidia-smi` that training has exited before a large render.
Ctrl-C or SIGTERM also requests a cooperative save and exit. The current
incomplete game is discarded; completed games, model updates and optimizer state
are preserved. An interrupted iteration is counted, and the next run starts a
fresh iteration. A stop between models simply exits. SIGSTOP retains GPU memory.

After rendering:

```bash
rm checkpoints/organism/STOP
.venv-training/bin/python -u -m alphazero.train_organism --forever
```

The stop file defaults to `STOP` inside the checkpoint root; `--stop-file` can
override it. A file lock prevents concurrent writers in one checkpoint root.

## Live dashboard and OGF viewer

Build the shared site renderer once with `npx shadow-cljs release ogf-viewer`.
From the repository root, run `python3 -m alphazero.dashboard`, then open
http://127.0.0.1:8765. It uses the Python standard library, binds only to localhost,
and reads JSON observations rather than loading networks or taking GPU memory.
Training and the dashboard are separate processes; neither auto-starts on reboot.

The viewer calls `organism.board/render-game`, the same SVG renderer as the site:
element shapes, held/free food, board coordinates and gradient backgrounds are
shared source, not a Python/JavaScript redraw. `organism.viewer` adapts OGF display
snapshots without initializing the multiplayer app. V2 records preserve ring colors;
player colors are derived from that palette. The creation page's palette generator
supplies colors for legacy imports and the Change colors button. Change colors regenerates the board, pieces and score
legend together; the palette stays fixed while stepping and receiving live updates. Move animations from `organism.play` are not included yet.
Rebuild `ogf-viewer` after changes to shared rendering code.
The shared-renderer browser checks cover element SVG paths, food-circle totals,
playback, palette regeneration, stable colors across steps, and player/ring linkage.

For remote access, run `ssh -N -L 8765:127.0.0.1:8765 USER@TRAINING_HOST`
on the computer running your browser, keep that SSH connection open, and visit
http://127.0.0.1:8765 there. VS Code Remote SSH can forward port 8765 through its
Ports panel too. No public dashboard listener or firewall opening is needed.

The dashboard polls every three seconds and shows current self-play decisions,
iteration losses, replay size, and rule victories versus cutoffs. Charts summarize
the last 500 logged iterations. Losses only appear after an optimizer update.
An old timestamp means observations are stale, not proof of ongoing activity.

The viewer supports live following, first/previous/next/last positions, a scrubber,
play/pause, playback speed, turn jumps, changed-space highlights, local OGF file
opening, and OGF downloads. The repository's existing `ogf/` exports are listed too.
New self-play games are saved as OGF v2 view-profile JSON in each model's `games/` directory;
the latest 100 are retained. `live.json` is replaced at most every two seconds
during decisions. Interrupted recordings are explicitly marked `interrupted`.
Games played before recording was enabled cannot be reconstructed from tensors.

OGF v1 is a viewing snapshot format, not a lossless engine state or a portable
move log. Training exports preserve its existing board, frame, element-tuple,
food and capture-count fields. Optional `source`, `frame-unit`, home spaces,
decision phase/index, iteration and result metadata distinguish self-play
decisions from whole turns. The numeric action index is internal to this encoder;
it is not a stable semantic move notation. Existing readers ignore these extras.
Exact resumable training still uses `latest.pt`. Rules versioning, complete setup,
semantic actions and lossless game-state import need a future format specification.

## Long games, loops, and learning from scratch

There is no reliable generic threshold for distinguishing patient strategy from
pointless play. We use explicit episode limits and measure their effect:

* Real rule victories receive +1 for the winner and -1/(players-1) for others.
* An episode stops on the third occurrence of the same decision state
  (`--repetition 3`; `0` disables). The key includes food, captures, player,
  pending actions and partial growth payments. It excludes only the round
  counter, which the Python base rules do not use. Board occupancy alone would
  wrongly flag food accumulation as a loop.
* A 4,000-choice limit (`--max-steps`) bounds games that keep changing without
  winning. Choices include substeps such as choosing a source or food donor;
  they are not whole turns.
* By default, repetition and length cutoffs get neutral value targets
  (`--truncation draw`). These are training-horizon draws, not new official
  Organism rules. Evaluation reports them separately from real wins/losses.
  Search does not adjudicate repetition using the full episode history; the
  cutoff is enforced by the episode runner.

Watch `metrics.jsonl` and per-game logs: each game records `win`, `repetition`,
`max_steps`, or an unexpected no-action/no-player exit. Iteration metrics include
losses, replay size, timings and peak CUDA allocation. Null losses mean no update
occurred. A rising real-win completion rate matters more than decreasing loss.
If everything is a cutoff draw, value training has no signal separating winners
from losers; extend the horizon and investigate exploration before spending days
on that run. Neutral draw targets also mean values reflect the training horizon,
not the theoretical outcome of an unbounded game.

Competent demonstrations are not a prerequisite: [AlphaZero learns from rules
and self-play](https://deepmind.google/research/alphazero-and-muzero/). It does need
a tractable environment where exploration reaches informative outcomes. Starting
on the app's four-ring board gives us that opportunity. Completion rates with
MCTS still need measuring; random-play completion alone is not a guarantee.

`--truncation discard` learns only from completed games, but can starve the replay
buffer and bias it toward easy-to-finish positions. `--truncation shaped` opts
into the old progress heuristic; rewarding food may teach hoarding, so this is
an explicit experiment rather than the default. A separate run can import
matching model exports with `--warm-start PATH_TO_CHECKPOINT_ROOT`, resetting
replay and optimizer state. No hand-written competence is required by default.

## Tests and evaluation

Validated on this GPU with PyTorch 2.9.1+cu128: all 22 Python tests passed,
including live Clojure parity checks. Both player counts completed GPU optimizer
updates and resumed model, optimizer and replay state. A stop-file check saved
and exited successfully. These short checks validate operation, not strength.

With recording enabled, all 24 Python tests passed, including compatibility with
the existing OGF reader, and GPU update/resume/stop checks passed again. Firefox
checks covered live-board loading, an existing OGF export, playback, scrubbing and
turn navigation. The updated site history controls compile with four existing
dependency redefinition warnings; they have not been deployed to the public site.

```bash
.venv-training/bin/python -m unittest discover -s tests -v
.venv-training/bin/python -u -m alphazero.train_organism --players 2 3 --iters 1 --sims 2 --games 1 --max-steps 8 --blocks 1 --filters 8 --batch-size 4 --min-buffer 1 --train-steps 1 --buffer 32 --truncation draw --duty 1 --checkpoint /tmp/organism-gpu-smoke --stop-file /tmp/organism-gpu-smoke/STOP
```

The short smoke test checks GPU self-play, backpropagation and checkpoints;
it deliberately uses cutoff draws and does not measure playing strength. Run
it again to verify resume.

Measure strength with seat-balanced evaluation and search noise disabled:

```bash
.venv-training/bin/python -u -m alphazero.evaluate_organism checkpoints/organism/2p --games-per-seat 10
.venv-training/bin/python -u -m alphazero.evaluate_organism checkpoints/organism/3p --games-per-seat 10
```

Default opponents choose uniformly among legal choices. Copy an early weight
export outside the retention directory, then use `--opponent path/to/frozen.pt`
to compare against it at the same search budget. In three-player games, both
other seats use that opponent. Results distinguish wins, losses and cutoffs.
Small samples do not establish improvement. Evaluation defaults to CPU to avoid
GPU contention. There is no automatic promotion gate: training continues from
the latest weights and can regress, so use these evaluations to check progress.

## Changes and remaining scope

GPU inference now uses the model's device. Legal priors are renormalized, and
expanded nodes reuse their value predictions. Organism value heads predict each
player's outcome in cyclic seat order, rather than assuming both opponents have
equal prospects. Journey retains its scalar head.

Automatic transitions complete before children receive their next player;
winners are retained, integrity resolves first, and tie-breaking follows the
current Clojure rule (causing a tie loses). Capture chains resolve downstream
first; integrity awards are per opponent, and sacrifice captures are retained.
Encodings include the whole board
and decision context. Food encoding preserves differences above ten food,
which matter for half-food circulation. Simulation copies mutable state without repeatedly copying
immutable board geometry.

Growth-food allocation selects donors one food at a time, making every allocation
reachable without an exponential list or a ten-choice cutoff. Organism selection
uses distinct occupied spaces instead of IDs modulo ten. Eat-source enumeration
matches the current Clojure choice rule. Circulation transfers half the source
food, rounded up. Introduction destroys free food on its three home spaces,
preserves adjacent free food, and starts each new piece with one food.

`tests/test_clojure_parity.py` executes the live Clojure engine and compares 42
reference cases: board geometry, all four actions, introduction, scoring, and
legal-move predicates on generated positions. It uses cached JVM dependencies
and explicitly skips when those are absent. `test/clj/organism/introduction_test.clj`
also checks both Clojure introduction entry points against the home-food rule.
This is representative base-rule coverage, not proof of equivalence for every
reachable game. Mutations are outside this training configuration. Web-app
model serving is not implemented. Strong play must be established through
completed games and seat-balanced evaluations, not loss curves alone.

OGF ring coordinates and saved palette: see [the v2 view contract](../docs/ogf-view-v2.md). Legacy recordings remain readable; their missing palettes are generated on import.
