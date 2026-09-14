#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
# 2p replay: an uncapped 5000-sample buffer held 2-32 distinct games because a
# single long game could occupy 80% of it. Capped sampling restores ~170.
# Evaluation: one tick per eight search batches left a 4000-choice game needing
# four hours, so evaluations never finished and the opponent archive never grew.
exec native/target/release/organism-train train \
  --forever --players 2,3 --rings-2p 3 --curriculum-2p \
  --buffer 32768 --replay-game-cap 256 --cutoff-value mask \
  --buffer-2p 32768 --replay-game-cap-2p 256 --cutoff-value-2p draw \
  --actors 16 --concurrent-games 64 --gpu-batch 32 --exploration-rounds 10 --sims 64 --threads 4 \
  --eval-every 20 --eval-games-per-seat 4 --eval-service-ticks 6 --eval-max-steps 1500 \
  --lr-anneal 500 --lr-floor 0.000025 \
  --lr-anneal-2p 0 --lr-floor-2p 0 \
  --duty 1 --vram-fraction 0.35 \
  --checkpoint checkpoints/organism-native "$@"
