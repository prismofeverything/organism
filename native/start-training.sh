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
exec native/target/release/organism-train train \
  --forever --players 2,3 --rings-2p 3 --curriculum-2p \
  --buffer 32768 --replay-game-cap 256 --cutoff-value mask --cutoff-value-2p draw \
  --require-useful-action 1 --eat-threshold 5 --sacrifice-yields-nothing 1 --stall-limit 15 \
  --actors 16 --concurrent-games 96 --gpu-batch 96 --exploration-rounds 10 --sims 64 --threads 8 \
  --eval-every 20 --eval-games-per-seat 4 --eval-service-ticks 6 --eval-max-steps 1500 \
  --duty 1 --vram-fraction 0.60 \
  --checkpoint checkpoints/organism-native "$@"
