#!/usr/bin/env bash
# Restart the long-running training processes if they have died.
#
# Nothing else supervises them. The dashboard died on 2026-09-16 and the
# benchmark watcher on 2026-09-19; both were killed by a signal, left no
# traceback, and went unnoticed for days, costing three days of benchmark data.
#
# A service that keeps dying is NOT restarted on a tight loop: each consecutive
# failure waits twice as long as the last, up to BACKOFF_MAX. A restart loop
# hides the fault, wastes the GPU, and can corrupt state through repeated
# partial writes — but giving up outright is worse, because the usual cause is
# temporary. Training died on 2026-09-23 when the system disk filled; a cleanup
# job freed it nine minutes later, by which time the watchdog had spent its
# three attempts and stayed down for seven hours. Backing off keeps the retry
# cheap enough to leave running and slow enough to be honest about.
#
# Run from cron. Safe alongside a deliberate stop: a service whose STOP file is
# present is left alone, and checkpoints/watchdog-off disables everything.
set -uo pipefail
cd "$(dirname "$0")/.." || exit 1

HEALTHY_AFTER=600    # up this long after a restart counts as recovered
BACKOFF_FIRST=300    # wait before the second attempt; each one doubles
BACKOFF_MAX=3600     # never wait longer than this between attempts
COMPLAIN_AFTER=3     # consecutive failures before every attempt is logged loudly

exec 9>/tmp/organism-watchdog.lock
flock -n 9 || exit 0
[ -f checkpoints/watchdog-off ] && exit 0
mkdir -p checkpoints/watchdog-state

log() { printf '%s  %s\n' "$(date -Is)" "$*" >> checkpoints/watchdog.log; }

# Match the executable name, never the full command line: shell wrappers carry
# the pattern in their own arguments and report false positives.
alive() { ps -eo comm,args | awk -v c="$1" -v p="$2" '$1==c && $0 ~ p {f=1} END{exit !f}'; }

# 9>&- is essential: without it the service inherits the lock descriptor and
# holds it for life, so every later run exits at the flock and stops watching.
#
# The wrapper's own output goes to a log, not /dev/null. Each command redirects
# its own output already, so only what escapes lands here — which is exactly
# what a launch that never starts leaves behind. A malformed command used to
# fail silently: the trainer was "restarted" all afternoon while bash was
# really being handed the argument `--forever` to run as a program.
launch() { nohup bash -c "$1" >> checkpoints/watchdog-launch.log 2>&1 9>&- & }

# Seconds to wait before attempt number `fails + 1`. The first retry is
# immediate; after that each wait doubles until it reaches BACKOFF_MAX.
backoff() {
  local fails=$1 delay=$BACKOFF_FIRST n=1
  [ "$fails" -le 0 ] && { printf '0'; return; }
  while [ "$n" -lt "$fails" ] && [ "$delay" -lt "$BACKOFF_MAX" ]; do
    delay=$((delay * 2)); n=$((n + 1))
  done
  [ "$delay" -gt "$BACKOFF_MAX" ] && delay=$BACKOFF_MAX
  printf '%s' "$delay"
}

supervise() {
  local name=$1 comm=$2 pat=$3 cmd=$4
  local state="checkpoints/watchdog-state/$name" fails=0 last=0 now
  now=$(date +%s)
  [ -f "$state" ] && read -r fails last < "$state"
  fails=${fails:-0}; last=${last:-0}

  if alive "$comm" "$pat"; then
    if [ "$fails" -gt 0 ] && [ $((now - last)) -ge "$HEALTHY_AFTER" ]; then
      printf '0 %s\n' "$now" > "$state"
      log "$name: healthy for $((now - last))s; failure count cleared"
    fi
    return
  fi

  local delay waited
  delay=$(backoff "$fails")
  waited=$((now - last))
  if [ "$waited" -lt "$delay" ]; then
    # Still serving out the wait from the last failure. Say so once per hour, so
    # a service that is down for a long time is not silently down.
    if [ $((waited % 3600)) -lt 300 ] && [ "$waited" -ge 3600 ]; then
      log "$name: still down after $fails attempts; next try in $((delay - waited))s"
    fi
    return
  fi

  launch "$cmd"
  printf '%s %s\n' "$((fails + 1))" "$now" > "$state"
  if [ "$fails" -ge "$COMPLAIN_AFTER" ]; then
    log "$name: STILL FAILING - attempt $((fails + 1)), waiting $(backoff $((fails + 1)))s before the next. Check checkpoints/watchdog-launch.log"
  else
    log "$name: not running; restarted (consecutive failure $((fails + 1)))"
  fi
}

[ -f checkpoints/organism-native/STOP ] || supervise trainer organism-train '--forever' \
  'bash native/start-training.sh >> checkpoints/organism-native/training.log 2>&1'

for root in checkpoints/organism-benchmark-2p-bignet; do
  [ -d "$root" ] || continue
  [ -f "$root/STOP" ] && continue
  supervise "benchmark-$(basename "$root")" python3 "run-benchmarks.py --root $root" \
    "python3 -u native/run-benchmarks.py --root $root --watch >> $root/orchestrator.log 2>&1"
done

supervise dashboard python3 'alphazero[.]dashboard' \
  'python3 -m alphazero.dashboard --checkpoint checkpoints/organism-native > /tmp/organism-dashboard.log 2>&1'
