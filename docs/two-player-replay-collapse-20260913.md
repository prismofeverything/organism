# Two-player replay collapse — September 13, 2026

## What was wrong

The 2p recipe adopted with this morning's checkpoint replacement paired a 5000
sample replay buffer with no per-game sample cap (`--replay-game-cap-2p 0`).
Two-player games average around 700 choices and regularly reach the 4000-choice
limit, so every game pushed its full length into the buffer:

| | 2p before this fix | 2p after | 3p (unchanged) |
| --- | --- | --- | --- |
| samples pushed per iteration | 8191 — 164% of the buffer | ~3100 — 10% | ~3100 — 10% |
| largest single game's share | median 42%, peak 80% | 0.8% | 0.8% |
| distinct games in the buffer | 2 – 32, mean 11 | ~170 expected | 150 – 206 |

The entire replay was replaced about 1.6 times per iteration, and 100 optimizer
steps of batch 64 then drew 6400 samples from roughly eleven games. Reported
policy loss drifted from 0.823 to 0.940 across the run and its spread doubled
against the previous recipe. Value loss oscillated between 0.0010 and 0.8385:
when `replay_distinct_games` fell to 2 the buffer held a single long drawn game,
every value target was zero and the loss collapsed, then recovered as varied
games returned. That is refitting, not learning.

The reported loss is not a valid progress metric under an uncapped buffer. Its
variance tracks buffer composition, not model quality — over this stretch
policy loss correlates **negatively** with mean game length (-0.357), because
the loss falls when one long game crowds the buffer.

Independent confirmation came from the fixed-opponent panel. At iteration 420
the 2p model scored 0 wins, 3 losses and 13 cutoffs over 16 completed games
against `control-399`, the checkpoint it had been seeded from twenty iterations
earlier, and 2 – 4 – 10 against `initial-379` where the seed checkpoint had
scored 6 – 2 – 8. Small samples, but both point the same way.

## What changed

- `--buffer-2p 32768 --replay-game-cap-2p 256`, matching the 3p recipe.
  `--cutoff-value-2p draw` is deliberately unchanged so the buffer fix can be
  attributed on its own. Growing a buffer on resume is handled by the existing
  capacity reconciliation in `load`; no samples are discarded.
- Evaluation was starved rather than slow. `service_evaluation` advanced each
  game by one choice per call and was called once per eight search batches,
  giving about 0.28 choices per second per game. A 4000-choice game therefore
  needed four hours, and because a new job only starts when none is in flight,
  every scheduled evaluation between iterations 405 and 540 was skipped. The
  2p `opponent-archive` stayed at its single seed entry for 142 iterations and
  `evaluations/` stayed empty. Three changes: `--eval-service-ticks` performs
  several ticks per call, evaluations are scheduled from the last one that
  actually started rather than a modulus, and `--eval-max-steps` bounds
  evaluation games without touching training games.
- `--eval-games-per-seat 4`. `tick_many` advances every active game together,
  so more games per evaluation costs GPU per tick but very little wall clock.
  The previous default of 2 gave four games for 2p; the 3p in-training
  evaluation reported `0 W / 0 L / 6 cutoffs` for 335 consecutive iterations,
  which carries no information at all.
- Evaluation reports now carry `decisive_games`, `decisive_win_rate` with a
  Wilson 95% interval, `cutoff_fraction` and a draw-aware `score`. These panels
  routinely cut off more games than they decide; a bare ratio reads as far more
  precise than the evidence supports.
- `replay_distinct_games` is joined by `replay_largest_game_fraction`, the
  trainer logs a warning when a full buffer holds fewer than 50 distinct games,
  and the dashboard marks the same condition. This collapse was visible in
  telemetry the whole time and nothing said so.
- `run-benchmarks.py` pins termination rules in `protocol.json` and writes them
  into every manifest. Previously each candidate inherited whatever `max_steps`
  and cutoff settings training happened to be using, so a recipe change would
  have silently made later candidates play a different game from earlier ones.
  Series recorded before this pinning keep the original rules.

## Curriculum readiness screen

While the above was being verified, the trainer spent a 32-game promotion test —
roughly 45% of two-player self-play throughput for half an hour — on a model that
scored 0 wins and 4 losses in its first six test games. `curriculum::ready_to_test`
had gated on the fraction of recent **self-play** games ending in a win. Both
seats share one network, so that measures how decisively the model beats itself;
a model losing to everything else satisfies it just as easily as a strong one.

The screen now pools recent completed reports from `evaluations/` and asks whether
a 32-game test reproducing that record would pass. The expansion criteria were
factored into one function over raw counts, which both the screen and the gate
call, so the screen cannot drift looser than the test it is deciding to pay for.
The self-play condition is kept only as an activity precondition — at least 48
games over three iterations — and no longer looks at how those games ended.

Two failure modes are closed deliberately. With no completed evaluation in the
recent window the screen declines, rather than treating absent evidence as
permission; this is why starved evaluations previously left it running on
self-play alone. And reports from iterations above the model's current one are
ignored, because iteration numbers restart when a checkpoint is replaced and
`evaluations/` travels with the copied directory.

## Implemented but left off

- `--stall-limit` ends a game as `no_progress` after N rounds of completely
  unchanged layout, addressing games that shuffle forever because food keeps the
  exact repetition key changing. It is off by default, and it is off in
  production, because the measurement does not support enabling it for 2p:
  among 2p games that end in a **win**, the longest unchanged-layout stretch has
  median 14 rounds, p90 173 and p95 262. A limit of 150 rounds would truncate
  12% of decisive games; even 300 costs 3.8%. Long stalls are apparently part of
  real 2p play, not only of stuck ones. With the per-game cap in place a long
  game no longer damages the buffer, so the rule is available to enable
  deliberately rather than needed.
- `--lr-anneal` / `--lr-floor` were initially left off for the same reason, then
  enabled at the user's request once the replay recovery was established. See
  "Learning-rate anneal" below.

## Learning-rate anneal

The original schedule decays 1e-3 to 1e-4 over the first eighty iterations and
then holds 1e-4 forever; 2p was past 58,000 optimizer steps and 3p past 100,000
at that rate. The launcher now runs `--lr-anneal 500 --lr-floor 0.000025`, which
halves the post-warm-up rate every 500 iterations down to a floor of 2.5e-5:

| | iteration when enabled | rate before | rate after |
| --- | --- | --- | --- |
| 2p / 3 rings | 580 | 1e-4 | 5e-5 |
| 3p / 4 rings | 1000 | 1e-4 | 2.5e-5 (at the floor) |

Both models converge on the same floor; the more heavily trained one simply
arrives first, which is the intent. The warm-up is untouched — a fresh
curriculum stage still starts at 1e-3 — and the floor means the rate can never
anneal to zero during the long drawn stretches the masked value loss was written
for.

This was deliberately not enabled alongside the replay fix, so that the first
post-fix benchmark would measure one change. That reading is now partly spent:
the next benchmark carries both. The replay fix has its own unconfounded
evidence — `replay_distinct_games` 13 to 168, largest game 42% to 0.8%, and the
two-player seat split moving from 67/33 to 42/58 — none of which a learning rate
explains.

## What to watch

`replay_distinct_games` for 2p should climb from roughly 11 toward 170 within
about ten iterations as the buffer grows to 32768, and the warning line should
stop appearing. `checkpoints/organism-native/2p-r3/opponent-archive` should gain
entries again. The fixed-opponent benchmark against `control-399` is the
measurement that matters; the loss curve is not.

## Reverting

Restore the three 2p options in `native/start-training.sh` to `--buffer-2p 5000
--replay-game-cap-2p 0`, and drop the four `--eval-*` options. The saved
checkpoint tolerates either recipe: `load` treats these as operational tuning
and adopts the launcher's values rather than refusing to resume.
