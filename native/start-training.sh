#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
# Fresh run under the corrected rules, started 2026-09-14. The previous model is
# archived at checkpoints/organism-native-prerules-20260914; what it had learned
# was strength at a game permitting deliberate passing and sacrifice farming.
#   --require-useful-action    passing is what is left when nothing can be done
#   --eat-threshold 5          an eater at 5 circulates it out before eating again
#   --sacrifice-yields-nothing a self-inflicted wipe takes its food with it
#   --stall-limit 15           rounds with no change in layout or food held; 8 cut
#                              26% of 3p games and sat inside legitimate play
#   --max-steps 1500           was 4000, which bought nothing: completed games
#                              run to a median of 259 (2p) / 279 (3p) choices and
#                              a p99 of 1324 / 1234, so 1500 truncates 0.6% of
#                              real games and the other 2500 only ever paid for
#                              games that were never going to finish. Matches
#                              --eval-max-steps, so self-play and evaluation now
#                              abandon a game at the same point.
#   --repetition-value-2p -0.2 2026-10-05: a repetition was a neutral draw, and
#                              2p self-play learned to force one -- 62/100 games
#                              ended by repetition (median 57 choices), mostly by
#                              both sides self-wiping and re-introducing back to
#                              the opening. A small loss for both makes stalling
#                              worse than playing on. 3p left at 0 (3% repetition).
#   --sims 256 --sims-3p 128   2026-10-05: doubled both; both lineages plateaued
#                              and the GPU sat ~half idle at 128/64.
#   --curriculum-max-rings 4   2026-10-17: 2p moved to 4 rings by hand, starting from
#                              the 3-ring model's spatial layers (curriculum.json
#                              records it). On 3 rings, 31-39% of games between
#                              different models never resolved; 3p on 4 rings
#                              resolves ~99%. Capped at 4 because the automatic
#                              gate asks for a win-rate lower bound over 50%
#                              against a ratcheting copy of itself, which it never
#                              met in 5653 iterations; a larger board is a choice.
#   --blocks-3p 12 --filters-3p 192
#                              2026-10-17: 3p sat at ~0.40 against prebugfix-4617
#                              from iteration ~1000 to 5649, and doubling its sims
#                              did not move it. The larger net starts as an exact
#                              copy of the 8x128 one (--grow-from-3p, used only
#                              when 3p/ is fresh); the old lineage is archived at
#                              checkpoints/organism-native-archives/3p-8x128-20261017.
exec native/target/release/organism-train train \
  --forever --players 2,3 --rings-2p 3 --curriculum-2p \
  --buffer 32768 --replay-game-cap 256 --cutoff-value mask --cutoff-value-2p draw \
  --require-useful-action 1 --eat-threshold 5 --sacrifice-yields-nothing 1 --stall-limit 15 \
  --actors 16 --concurrent-games 96 --gpu-batch 96 --exploration-rounds 0 --exploration-choices 30 --threads 8 \
  --curriculum-max-rings 4 \
  --sims 256 --sims-3p 128 --repetition-value-2p -0.2 --baseline-ratchet 1 \
  --blocks 8 --filters 128 --blocks-3p 12 --filters-3p 192 \
  --grow-from-3p checkpoints/organism-native-archives/3p-8x128-20261017/final-5649.ot \
  --max-steps 1500 \
  --eval-every 20 --eval-games-per-seat 8 --eval-service-ticks 6 --eval-max-steps 1500 \
  --duty 1 --vram-fraction 0.60 \
  --checkpoint checkpoints/organism-native "$@"
