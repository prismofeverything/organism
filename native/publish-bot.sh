#!/usr/bin/env bash
# Put the current trained weights in front of the website's bot.
#
# The website serves a copy rather than the checkpoint itself: training keeps
# only its recent snapshots and rotates the rest away, which would pull the
# file out from under a running move server. Copying is also what makes this a
# decision — the bot changes when someone publishes, not whenever a training
# iteration happens to land.
set -euo pipefail
cd "$(dirname "$0")/.."

seat=${1:-3p}
checkpoint=checkpoints/organism-native/$seat
generation=$(python3 -c "import json,sys;print(json.load(open('$checkpoint/latest.json'))['generation'])")
weights=$checkpoint/snapshots/$generation/model.ot

if [ ! -f "$weights" ]; then
  echo "no weights at $weights; training may have just rotated that snapshot away" >&2
  exit 1
fi

# Write beside the destination and rename, so a server reading the old file
# never sees a half-copied one.
cp "$weights" "$checkpoint/serve.ot.incoming"
mv "$checkpoint/serve.ot.incoming" "$checkpoint/serve.ot"
echo "published $seat generation $generation to $checkpoint/serve.ot"
echo "restart the web process to pick it up: the move server loads weights once."
