# Training review — September 21, 2026

Read-only audit of running jobs, current configs, metrics, evaluation reports,
recordings, and evaluation/curriculum code. No running recipe was changed.

## Current system

Production is active: 2p at approximately iteration 2096, 3p at 2453. Both use
8 residual blocks / 128 filters, replay capacity 32768 with a 256-position game
cap, 96 concurrent games, GPU batches of 96, and 100 gradient updates per
iteration. 2p searches 128 simulations per decision on 3 rings (19 spaces);
3p searches 64 on 4 rings (37 spaces). Both sample their first 30 decisions.
The updated rules require useful actions, limit eating at held food 5, remove
self-sacrifice yield, and cut off 15 stalled rounds. These runs are not directly
comparable to the September 13 experiment under previous rules. The two-player
curriculum has not promoted beyond 3 rings.

Both current opponent pools are empty: historical networks are evaluated against,
but are not currently used as training opponents. The old diversity experiment
is stopped at round 31 (30 complete). Its round-20 3p reports were inconclusive:
against older-385 both treatments had 36W/0L/12C; against older-625 pure self-play
had 15W/26L/7C and history mix 11W/23L/14C. These precede the corrected rules.

## Evidence

Last 100 completed self-play iterations at inspection:

| Model | Iterations | Victory | Repetition | Stall | Length |
| --- | --- | ---: | ---: | ---: | ---: |
| 2p | 1996–2095 | 1403 | 170 | 37 | 1 |
| 3p | 2354–2453 | 1615 | 3 | 0 | 0 |

These are 87.1% and 99.8% decisive endings, not opponent win rates.
3p's first 100 iterations had 899 victories in 1606 games (56.0%); its recent
self-play completion is a real behavioral change, but is now a saturated metric.
2p's first 100 iterations had 1520 victories in 1613 games (94.2%); completion
has not increased monotonically.

Last ten completed built-in evaluations:

| Model | Candidate iterations | Wins | Losses | Cutoffs |
| --- | --- | ---: | ---: | ---: |
| 2p | 1853–2059 | 55 | 41 | 64 |
| 3p | 2247–2432 | 66 | 172 | 2 |

Opponent archives rotate and the baseline can advance. These aggregates describe
recent tests, not a fixed-opponent trend or independent identically distributed
samples. 3p should not be judged using a 50% equal-strength reference: with
identical policies and balanced seats, the decisive reference is one third.
Heterogeneous opponents and cutoffs complicate that reference further.

Recent saved games: in a sample of 100 per model, every observed winner had at
least 5 power. A second pass reconstructing winner connected components found
no winner with three complete organisms. Files rotate while training, so the
exact winning sample count differed by one between passes. This suggests narrow
observed victory mechanisms, not proof that organism victories are impossible.

The benchmark summary published to the dashboard was last updated at Unix time
1789842248 (over two days before this audit). No separate benchmark or diversity
runner was alive. The latest 2p big-network candidate in that benchmark is 1136,
with only its first two batches complete; current production is around 2096.
Built-in evaluation remains active and much more current. The existing fixed
benchmark panel covers 2p only, so it cannot establish a current 3p strength trend.

## Prioritized next work

1. Restore current-rules fixed benchmarks for both models. Pin same-architecture,
   same-rules old/medium/recent opponents and full identity/hash metadata. Use
   balanced seats and a sufficiently large fixed seed panel; show sample size,
   cutoffs and data age. Label stopped/stale work. Keep adaptive-baseline tests
   separate from the fixed trend. Audit the 3p ratchet: `record_against` currently
   includes every mixed game containing baseline slot 1, and its promotion gate
   uses a >50% lower bound for both player counts. This is a stringent policy,
   not a neutral three-player equal-strength test.
2. Run an isolated 2p/4-ring pilot alongside retained 3-ring checkpoints, with the
   same rules/reward/search recipe. Board sizes require different dense heads;
   feature transfer and startup cost must be acknowledged. Benchmark each board
   separately rather than comparing raw self-play victory percentages. The
   board-density hypothesis is plausible but not established by 2p-vs-3p data.
3. Track winning mechanism (power versus complete organisms), seat balance,
   strategic movement/splitting, and repeated patterns. They diagnose what is
   learned, without turning those diagnostics into extra reward bonuses.
4. Revisit opponent diversity only in a new controlled, current-rules experiment
   if fixed tests expose forgetting or narrow opponent competence. League
   training has a research precedent, but the old local screen did not establish
   a benefit: https://www.nature.com/articles/s41586-019-1724-z

## Throughput and storage

Last-100-iteration aggregate decision rates: 2p 39.8/s, 3p 50.9/s. These are not
like-for-like because search budgets and board sizes differ. Measured self-play
search accounts for 35.3% and 58.1% of recorded iteration time respectively;
the remainder includes updates/evaluation/other work, not merely serialization.
Inference-path wall time is 68.8% / 83.3% of search time and includes transfers
and synchronization. It is not GPU-kernel-only timing. One host sample showed
58% GPU utilization and 1357/16311 MiB VRAM; it does not establish sustained GPU
saturation. More VRAM allocation or CPU threads alone is not an evidenced fix.
/mnt/data had approximately 65 GiB available. Runtime writes remain on that mount.
