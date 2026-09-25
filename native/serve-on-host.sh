#!/usr/bin/env bash
# Run the move server out of this directory.
#
# The web app spawns the binary itself (see organism.native-bot), so nothing
# depends on this script; it is here to check by hand that the binary runs, the
# libraries resolve and the weights load on a box that has no Rust and no CUDA.
#
#   echo '{"actions":[]}' | ./serve.sh        → one move, with its cost in seconds
set -euo pipefail
cd "$(dirname "$0")"
exec env LD_LIBRARY_PATH="$PWD/lib" ./organism-train serve \
  --players 3 --rings 4 --blocks 8 --filters 128 \
  --sims "${ORGANISM_BOT_SIMS:-32}" --threads "${ORGANISM_BOT_THREADS:-2}" \
  --cpu --weights "$PWD/serve.ot" "$@"
