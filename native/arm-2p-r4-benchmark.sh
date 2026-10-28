#!/usr/bin/env bash
# Start the 2p/4-ring fixed-opponent benchmark once that lineage has opponents
# worth freezing. Run every five minutes by watchdog.sh; each run does at most
# the next step, so a reboot in between loses nothing.
#
# 2p moved to 4 rings on 2026-10-17 with no 4-ring model to measure against.
# The anchors are therefore its own early snapshots: the first saved at or after
# iteration 100 and 300. The trainer keeps only the two newest snapshots, so each
# is copied out as soon as it exists. Once both are held, the series is created
# with them; from then on the watchdog's benchmark glob supervises it and this
# script does nothing.
set -euo pipefail
cd "$(dirname "$0")/.."

ROOT=checkpoints/organism-benchmark-2p-r4
HOLD=checkpoints/organism-native-archives/2p-r4-anchors
MODEL=checkpoints/organism-native/2p-r4

[ -d "$ROOT" ] && exit 0   # created only at launch; the watchdog supervises it after
[ -f "$MODEL/latest.json" ] || exit 0
mkdir -p "$HOLD"

generation=$(python3 -c "import json,sys;print(json.load(open(sys.argv[1]))['generation'])" "$MODEL/latest.json")
iteration=${generation%%-*}

for target in 100 300; do
  if ! compgen -G "$HOLD/r4-$target-*.ot" > /dev/null && [ "$iteration" -ge "$target" ]; then
    cp "$MODEL/snapshots/$generation/model.ot" "$HOLD/r4-$target-$iteration.ot.tmp"
    mv "$HOLD/r4-$target-$iteration.ot.tmp" "$HOLD/r4-$target-$iteration.ot"
    printf '%s  2p-r4 anchor frozen: iteration %s (target %s)\n' "$(date -Is)" "$iteration" "$target" >> checkpoints/watchdog.log
  fi
done

early=$(compgen -G "$HOLD/r4-100-*.ot" || true)
late=$(compgen -G "$HOLD/r4-300-*.ot" || true)
[ -n "$early" ] && [ -n "$late" ] || exit 0

name() { basename "$1" .ot | sed 's/^r4-[0-9]*-/r4-iteration-/'; }
python3 - "$HOLD/anchors.json" "$PWD/$early" "$(name "$early")" "$PWD/$late" "$(name "$late")" <<'EOF'
import json, sys
out, a, an, b, bn = sys.argv[1:]
json.dump({'2p-r4': [{'identity': an, 'weights': a, 'blocks': 8, 'filters': 128},
                     {'identity': bn, 'weights': b, 'blocks': 8, 'filters': 128}]},
          open(out, 'w'), indent=2)
EOF
mkdir -p "$ROOT"
# Same search budget as the 3-ring 2p series.
nohup bash -c "python3 -u native/run-benchmarks.py --root $ROOT --watch --anchors $HOLD/anchors.json --protocol-sims 128 >> $ROOT/orchestrator.log 2>&1" > /dev/null 2>&1 9>&- &
printf '%s  2p-r4 benchmark series started at %s\n' "$(date -Is)" "$ROOT" >> checkpoints/watchdog.log
