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

# The same budget the site plays at. Reading settings.edn rather than carrying a
# default of its own: a check by hand that quietly used a different number than
# the running bot is worse than no check, and this script reported 32 while the
# site was playing 16.
from_settings() { grep -oE ":$1[[:space:]]+[0-9]+" settings.edn 2>/dev/null | grep -oE "[0-9]+" | head -1; }
SIMS="${ORGANISM_BOT_SIMS:-$(from_settings sims)}"
THREADS="${ORGANISM_BOT_THREADS:-$(from_settings threads)}"

exec env LD_LIBRARY_PATH="$PWD/lib" ./organism-train serve \
  --players 3 --rings 4 --blocks 8 --filters 128 \
  --sims "${SIMS:-32}" --threads "${THREADS:-2}" \
  --cpu --weights "$PWD/serve.ot" "$@"
