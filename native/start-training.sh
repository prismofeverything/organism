#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
exec native/target/release/organism-train train \
  --forever --players 2,3 --actors 16 --sims 64 --threads 4 \
  --duty 1 --vram-fraction 0.35 \
  --checkpoint checkpoints/organism-native \
  --warm-start checkpoints/organism-native/imported "$@"
