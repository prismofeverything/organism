#!/usr/bin/env bash
# Send one command to the persistent claude-seat harness and print the
# resulting decision block (everything new up to the next ===END===/
# ===GAMEOVER===). Usage: seat.sh start4 | seat.sh 2 | seat.sh state
start=$(wc -l < /tmp/eridu_out)
printf '%s\n' "$1" >> /tmp/eridu_in
for i in $(seq 1 150); do
  if tail -n +$((start+1)) /tmp/eridu_out 2>/dev/null | grep -qE '===END===|===GAMEOVER===|===READY==='; then
    break
  fi
  sleep 0.2
done
tail -n +$((start+1)) /tmp/eridu_out
