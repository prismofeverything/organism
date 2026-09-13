#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
exec native/target/release/organism-train train \
  --forever --players 2,3 --rings-2p 3 --curriculum-2p --buffer 32768 --replay-game-cap 256 --cutoff-value mask --buffer-2p 5000 --replay-game-cap-2p 0 --cutoff-value-2p draw --actors 16 --concurrent-games 64 --gpu-batch 32 --exploration-rounds 10 --sims 64 --threads 4 \
  --duty 1 --vram-fraction 0.35 \
  --checkpoint checkpoints/organism-native "$@"
