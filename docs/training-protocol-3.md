# Training protocol 3

Implemented September 12, 2026 following the action-selection audit. Game legality,
terminal victory rewards, and the exact-repetition rule are unchanged.

## Search

PUCT uses `sqrt(max(parent_visits, 1))`, so the first simulation respects the
network prior. Equal search scores are selected uniformly with a persisted seeded
RNG, after sorting candidates by action identity. Maximum-visit action ties are
also sampled uniformly. No movement bonus, passing penalty, or Grow-mode mask was
added. Old retained trees are discarded at migration; model weights and Adam
state are retained. Root noise and opening sampling remain as before.

The frozen three-player audit's deliberately Eat-favoring first-simulation probe
changed from Eat 0 / Pass 1 to Eat 1 / Pass 0. This verifies the ordering fix; it is
not a measurement of improved playing strength.

## Replay

The production launcher uses `--buffer 32768 --replay-game-cap 256`. Each completed
game contributes at most 256 positions, sampled across equal segments spanning
its entire trajectory. Short games contribute all their positions. This limits
long games' influence without oversampling winners, but is deliberately a change
to the sampling distribution: it is neither uniform over all generated positions
nor strictly uniform over games. Gradient batches sample uniformly from replay.

Samples retain game identity, termination reason, and a value-loss weight. Metrics
and the dashboard report identified game coverage and value-supervised fraction.
The buffer can grow or shrink on resume, retaining the newest available positions.
Other incompatible checkpoint configuration changes still fail.

Legacy samples have no game identities or termination reasons. Existing all-zero
legacy targets are conservatively excluded from value supervision under mask mode;
this also excludes some genuine repetition outcomes until fresh replay replaces
them. Their policy targets remain usable. Coverage initially undercounts the
legacy experience. New repetition samples retain full value supervision.

## Historical-opponent evaluation

Each model/board size keeps a bounded archive: the oldest anchor plus seven recent
candidate snapshots. An evaluation chooses one archived version by a deterministic
rotation, and freezes it alongside the baseline and candidate. Three-player tests
place the candidate in every seat and swap the baseline/historical opponent order.
Two-player tests rotate baseline and historical opponents at every candidate seat.
Mixed tests round the requested games-per-seat up to an even number (minimum two).

Each game records seat-to-model assignments; reports record opponent identities.
Frozen weights and resumable game/search/RNG state remain in the evaluation job.
The first evaluation of a wholly new model may only have a baseline; an existing
model can seed its archive from an earlier frozen evaluation candidate. Incomplete
pre-upgrade evaluation jobs are explicitly marked interrupted and excluded rather
than combining results produced by different search protocols.

Evaluation remains background work, with inference grouped by acting model. This
is a practical population check, not PSRO training, exhaustive opponent-pair
coverage, an equilibrium guarantee, or a single universal strength rating. The
existing conservative two-player curriculum gate now uses these mixed-opponent
results; its numerical thresholds are unchanged. Compare reports with their
recorded search protocol and opponent identities.

## Unresolved cutoffs

Production uses `--cutoff-value mask`; `--cutoff-value draw` is available for
controlled comparisons. A max-step cutoff still contributes policy examples but
has zero value-loss weight in mask mode. Value MSE is normalized by the number of
supervised rows; a batch with no supervised rows has zero value loss. At search's
length horizon, mask mode backs up the network value estimate, caching that leaf
estimate in the current tree. Real victories and exact repetitions retain their
own terminal values. Draw mode backs up zero at the horizon and supervises cutoff
positions with zero targets, matching the previous treatment.

This does not declare food-changing stagnation to be repetition. Horizon values
can themselves be inaccurate, and masking does not create missing victory data.
Longer-run comparisons of rule victories, cutoff rates, and frozen-opponent
performance are needed to establish which treatment learns better. Value-loss
numbers across the two treatments are not directly comparable as strength scores.

## Validation and operation

- 24 native unit tests plus spatial-transfer test passed; Python-fixture parity
  test remains explicitly ignored unless its fixture is supplied.
- Mid-game stop/resume preserved exact replay, search targets, and RNG state.
- Background evaluation progressed alongside self-play and resumed frozen weights.
- `tests/check_native_training_protocol.py` runs same-seed small mask/draw training,
  checks cutoff supervision, replay game caps, buffer growth without losing old
  samples, and complete mixed three-player seat assignments.
- Dashboard JavaScript syntax check passed.

The controlled smoke comparison validates mechanics, not learning strength. It
used tiny CPU networks and 20-choice horizons, isolated from production.

Before restart, checkpoint/optimizer generations and evaluation fixtures were
archived under `checkpoints/organism-native-archives/pre-protocol3-20260912-131436`.
The production command remains `bash native/start-training.sh`; it retains 64
concurrent games, four search threads, nice +10, and the 35% CUDA memory limit.
Touch `checkpoints/organism-native/STOP` for a durable graceful stop before a render;
remove it and run the launcher to resume.
