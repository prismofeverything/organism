#!/bin/bash
# Build locally and optionally deploy to the server.
#
# Usage:
#   ./deploy.sh              — build JS + uberjar locally
#   ./deploy.sh build        — same as above
#   ./deploy.sh clean        — clean everything first, then build
#   ./deploy.sh ship         — THE deploy: JS + jar + NEURON's move server and
#                              weights, uploaded, then one restart and a check
#   ./deploy.sh push         — skip build, just upload the jar + restart
#   ./deploy.sh model        — rebuild and ship only NEURON (new weights)
#   ./deploy.sh model-check  — time one move on the box
#   ./deploy.sh reap         — what the nightly reaper would delete (dry)
#   ./deploy.sh reap-now     — actually delete it
#   ./deploy.sh tail         — follow the remote journal (Ctrl-C to quit)
#   ./deploy.sh log [N]      — print last N journal lines (default 100)
#   ./deploy.sh status       — systemctl status of the remote service
#   ./deploy.sh sync-bot --game journey --owner prismofeverything --name OBO
#                            — push a bot from local MongoDB to remote
#
# Deploy target from $DEPLOY_HOST (default prism@elephantlaboratories.com).
# The server needs only Java and the shipped libtorch — all Node, ClojureScript
# and Rust compilation happens locally.

set -e
cd "$(dirname "$0")"

# Deploy target. Defaults to the domain (correct once DNS points at the new box);
# override for the pre-cutover droplet:  DEPLOY_HOST=prism@tetrahedron.world ./deploy.sh ship
# The app runs as the 'prism' user on the new box (created by the migration's 02 script).
REMOTE_HOST="${DEPLOY_HOST:-prism@elephantlaboratories.com}"
SERVICE="organism"          # systemd unit: HTTP + WebSocket on :11551
REMOTE_DIR="~/organism"
REMOTE_JAR_NAME="organism.jar"
REMOTE_JAR="$REMOTE_DIR/$REMOTE_JAR_NAME"
LOCAL_JAR="target/uberjar/organism.jar"

# NEURON, the trained-model bot. Its move server is a Rust process the web app
# spawns and talks to over a pipe, so the box needs the binary, a CPU libtorch
# for it to link against, and the weights it serves. All three are built here and
# shipped — the box needs no Rust toolchain and no CUDA, which is how the
# tetrahedron deploys on the same machine do it. See
# docs/playing-a-trained-model.md.
#
# (Not to be confused with `sync-bot`, which pushes a hand-built flowchart bot
# through MongoDB. This one is a network.)
MODEL_DIR="$REMOTE_DIR/bot"
MODEL_BIN="native/target-serve/release/organism-train"
MODEL_WEIGHTS="checkpoints/organism-native/3p/serve.ot"
MODEL_TORCH="${LIBTORCH_SERVE:-$(echo "$PWD"/.venv-serve/lib/python*/site-packages/torch)}"
# The box has 2 cores, shared with the web app, so it thinks less per move than
# the trainer does. `./deploy.sh model-check` reports what a decision really
# costs there rather than leaving it a guess.
MODEL_SIMS="${MODEL_SIMS:-32}"
MODEL_THREADS="${MODEL_THREADS:-2}"

# Deleting a game marks it and waits out a grace period; the reaper is what
# actually removes one once the window closes with nobody objecting. It only
# runs if something runs it, and nothing ever did — games sat marked for weeks.
# `ship` installs the schedule, so a deploy cannot leave it unset again.
REAP_AT="${REAP_AT:-17 4 * * *}"

build() {
  [ -d node_modules ] || npm install

  echo "=== Building ClojureScript (shadow-cljs release) ==="
  npx shadow-cljs release organism journey journey-bots oroboros eridu future universe

  echo "=== Building uberjar ==="
  lein uberjar

  echo "=== Done: $LOCAL_JAR ==="
  ls -lh "$LOCAL_JAR"
}

build_model() {
  echo "=== Building the move server (CPU libtorch) ==="
  bash native/build-serve.sh

  echo "=== Publishing the weights the site will serve ==="
  bash native/publish-bot.sh 3p
}

ship() {
  if [ ! -f "$LOCAL_JAR" ]; then
    echo "ERROR: $LOCAL_JAR not found — run ./deploy.sh build first"
    exit 1
  fi

  echo "=== Verifying local jar ==="
  # A jar that is already damaged locally must never reach the server.
  if command -v unzip >/dev/null 2>&1; then
    unzip -qt "$LOCAL_JAR" >/dev/null 2>&1 || {
      echo "ERROR: $LOCAL_JAR is not a valid archive — rebuild before shipping"
      exit 1
    }
  else
    echo "(unzip not found — skipping local archive check)"
  fi
  LOCAL_SIZE=$(stat -c%s "$LOCAL_JAR")
  LOCAL_SUM=$(md5sum "$LOCAL_JAR" | cut -d' ' -f1)
  echo "$LOCAL_SIZE bytes, md5 $LOCAL_SUM"

  # Upload BESIDE the live jar, never over it. scp has been seen to exit 0 on a
  # short write, and overwriting in place then leaves a truncated jar that
  # systemd happily starts: every page renders, every classpath resource throws
  # "invalid LOC header", and the site looks blank. Staging plus a checksum
  # means a bad transfer costs nothing — the running deployment is untouched.
  echo "=== Uploading jar to $REMOTE_HOST ==="
  scp "$LOCAL_JAR" "$REMOTE_HOST:$REMOTE_JAR.incoming"

  echo "=== Verifying upload ==="
  ssh "$REMOTE_HOST" "bash -lc '
    set -e
    cd $REMOTE_DIR
    remote_size=\$(stat -c%s $REMOTE_JAR_NAME.incoming)
    remote_sum=\$(md5sum $REMOTE_JAR_NAME.incoming | cut -d\" \" -f1)
    if [ \"\$remote_size\" != \"$LOCAL_SIZE\" ] || [ \"\$remote_sum\" != \"$LOCAL_SUM\" ]; then
      echo \"ERROR: upload does not match the local jar — NOT restarting $SERVICE\"
      echo \"  local:  $LOCAL_SIZE bytes  $LOCAL_SUM\"
      echo \"  remote: \$remote_size bytes  \$remote_sum\"
      rm -f $REMOTE_JAR_NAME.incoming
      exit 1
    fi
    echo \"upload verified — \$remote_size bytes, \$remote_sum\"
    mv -f $REMOTE_JAR_NAME.incoming $REMOTE_JAR_NAME
  '"
}

restart() {
  echo "=== Restarting $SERVICE via systemd ==="
  # systemd owns the process now (unit installed by the migration's 02 script);
  # NOPASSWD sudoers (also from 02) lets this restart run non-interactively.
  # This comes after everything is in place: the move server loads its weights
  # once at startup, so new weights only take effect on a restart.
  ssh "$REMOTE_HOST" "bash -lc '
    sudo systemctl restart $SERVICE
    sleep 2
    if systemctl is-active --quiet $SERVICE; then
      echo \"$SERVICE is active (port 11551)\"
    else
      echo \"ERROR: $SERVICE failed to start — recent logs:\"
      journalctl -u $SERVICE -n 30 --no-pager
      exit 1
    fi
  '"
}

remote_home() {
  # systemd Environment= needs absolute paths, and $REMOTE_DIR is a tilde.
  ssh "$REMOTE_HOST" 'echo $HOME'
}

ship_model() {
  if [ ! -x "$MODEL_BIN" ]; then
    echo "ERROR: $MODEL_BIN not built — run ./deploy.sh build (or bash native/build-serve.sh)"
    exit 1
  fi
  if [ ! -f "$MODEL_WEIGHTS" ]; then
    echo "ERROR: no weights at $MODEL_WEIGHTS — run bash native/publish-bot.sh 3p"
    exit 1
  fi
  if [ ! -d "$MODEL_TORCH/lib" ]; then
    echo "ERROR: no CPU libtorch at $MODEL_TORCH — see native/build-serve.sh"
    exit 1
  fi

  local home
  home=$(remote_home)

  # Exactly the libraries the binary resolves, which ldd knows and a guess
  # would not: three files out of a torch tree that is mostly Python bindings
  # the move server never loads.
  local libs
  libs=$(LD_LIBRARY_PATH="$MODEL_TORCH/lib" ldd "$MODEL_BIN" \
         | awk -v root="$MODEL_TORCH/lib/" 'index($3, root) == 1 {print $3}')
  if [ -z "$libs" ]; then
    echo "ERROR: $MODEL_BIN resolves nothing out of $MODEL_TORCH/lib"
    echo "  it was probably built against a different libtorch — rebuild with native/build-serve.sh"
    exit 1
  fi

  # rsync, not scp: the libraries are the bulk of this and change only when
  # libtorch does, so every deploy after the first sends the binary and the
  # weights and skips the rest.
  echo "=== Shipping the move server to $REMOTE_HOST ==="
  echo "    $(du -shc $libs | tail -1 | cut -f1) of libraries (sent once), $(du -h "$MODEL_BIN" | cut -f1) binary, $(du -h "$MODEL_WEIGHTS" | cut -f1) weights"
  ssh "$REMOTE_HOST" "mkdir -p $MODEL_DIR/lib"
  rsync -az $libs "$REMOTE_HOST:$MODEL_DIR/lib/"
  rsync -az "$MODEL_BIN" "$REMOTE_HOST:$MODEL_DIR/organism-train"
  rsync -az "$MODEL_WEIGHTS" "$REMOTE_HOST:$MODEL_DIR/serve.ot"
  rsync -az native/serve-on-host.sh "$REMOTE_HOST:$MODEL_DIR/serve.sh"

  # A file the app reads for itself, not the service's environment: this user
  # may restart the three services and nothing else, so editing the unit would
  # need a root the deploy does not have and should not want.
  echo "=== Telling $SERVICE where it is ==="
  local conf
  conf=$(mktemp)
  cat > "$conf" <<CONF
;; Written by deploy.sh — read by organism.native-bot at the first move asked of
;; NEURON. Absolute paths, because the service's working directory is not this
;; one. `sims` is how hard it thinks per decision: this box has 2 cores shared
;; with the web app, so it searches less than the trainer does.
{:serve   "$home/organism/bot/organism-train"
 :torch   "$home/organism/bot/lib"
 :weights "$home/organism/bot/serve.ot"
 :sims    $MODEL_SIMS
 :threads $MODEL_THREADS}
CONF
  rsync -az "$conf" "$REMOTE_HOST:$MODEL_DIR/settings.edn"
  rm -f "$conf"
}

reap() {
  # Dry by default, always: deletion does not come back. `reap-now` means it.
  local mode="${1:---dry-run}"
  echo "=== Reaping games whose grace period ran out (${mode:-for real}) ==="
  ssh "$REMOTE_HOST" "bash -lc '
    cd ~/organism
    /usr/bin/java -cp organism.jar clojure.main -m organism.scripts.reap-games $mode 2>&1 |
      grep -vE \"^SLF4J|^WARNING|^Warning\"
  '"
}

install_reaper() {
  local home
  home=$(remote_home)
  # Out of the same uberjar the service runs: the box has no Leiningen and no
  # source tree, so `lein run -m` — the way the script documents itself — is not
  # available there.
  local job="$REAP_AT cd $home/organism && /usr/bin/java -cp organism.jar clojure.main -m organism.scripts.reap-games >> $home/organism/reap.log 2>&1"

  echo "=== Scheduling the reaper ($REAP_AT) ==="
  # Rewrite only our own line and leave every other entry alone, so this is safe
  # to re-run and safe on a schedule somebody else also edits.
  printf '%s\n' "$job" | ssh "$REMOTE_HOST" "bash -lc '
    set -e
    kept=\$(crontab -l 2>/dev/null | grep -v \"organism.scripts.reap-games\" || true)
    mine=\$(cat)
    printf \"%s\n%s\n\" \"\$kept\" \"\$mine\" | grep -v \"^\$\" | crontab -
    echo \"scheduled:\"
    crontab -l | sed \"s/^/    /\"
  '"
}

model_check() {
  echo "=== Asking the move server for one move on the box ==="
  # Proves the binary runs there, the libraries resolve and the weights load —
  # and reports what a decision actually costs on this hardware, rather than
  # leaving it a guess from a much faster machine.
  #
  # The request goes over ssh's stdin so neither it nor the reply has to survive
  # a round of shell quoting.
  local answer
  answer=$(echo '{"actions":[]}' | ssh "$REMOTE_HOST" "$MODEL_DIR/serve.sh" 2>/dev/null | head -1)

  printf '%s\n' "$answer" | python3 -c '
import json, sys
line = sys.stdin.readline().strip()
if not line:
    print("no answer from the move server.")
    print("  the binary, its libraries or the weights are missing or broken;")
    print("  ./deploy.sh model ships all three, and")
    print("  ssh <host> ~/organism/bot/serve.sh shows what it says for itself.")
    sys.exit(1)
answer = json.loads(line)
print("move %s at %s — %.2fs for this decision"
      % (answer["action"], answer["phase"], answer["seconds"]))
print("a three-player turn is about six decisions, so roughly %.1fs per turn"
      % (answer["seconds"] * 5.6))
print("turn it down with:  MODEL_SIMS=16 ./deploy.sh model")
'
}

sync_bot() {
  shift  # consume "sync-bot"
  BOT_GAME=""
  BOT_OWNER=""
  BOT_NAME=""
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --game)  BOT_GAME="$2";  shift 2 ;;
      --owner) BOT_OWNER="$2"; shift 2 ;;
      --name)  BOT_NAME="$2";  shift 2 ;;
      *) echo "Unknown option: $1"; exit 1 ;;
    esac
  done
  if [ -z "$BOT_NAME" ]; then
    echo "Usage: ./deploy.sh sync-bot --game journey --owner prismofeverything --name OBO"
    exit 1
  fi

  BOT_GAME="${BOT_GAME:-journey}"

  echo "=== Exporting bot '$BOT_NAME' (game=$BOT_GAME) from local MongoDB ==="
  TMPFILE=$(mktemp /tmp/bot-sync-XXXXXX.json)
  mongoexport --quiet --db organism --collection game-bots \
    --query "{\"game-type\": \"$BOT_GAME\", \"name\": \"$BOT_NAME\"}" \
    --out "$TMPFILE" --jsonArray 2>/dev/null

  # Check we got something
  COUNT=$(python3 -c "import json; d=json.load(open('$TMPFILE')); print(len(d))" 2>/dev/null || echo 0)
  if [ "$COUNT" = "0" ]; then
    echo "ERROR: bot '$BOT_NAME' (game=$BOT_GAME) not found in local database"
    rm -f "$TMPFILE"
    exit 1
  fi

  # Apply overrides: strip _id, set owner/game-type
  python3 -c "
import json, sys
docs = json.load(open('$TMPFILE'))
doc = docs[0]
doc.pop('_id', None)
doc['game-type'] = '$BOT_GAME'
owner = '$BOT_OWNER'
if owner:
    doc['owner'] = owner
json.dump(doc, open('$TMPFILE', 'w'))
"

  echo "=== Pushing bot '$BOT_NAME' to $REMOTE_HOST ==="
  scp -q "$TMPFILE" "$REMOTE_HOST:/tmp/bot-sync.json"
  rm -f "$TMPFILE"

  ssh "$REMOTE_HOST" "bash -lc '
    mongoimport --db organism --collection game-bots \
      --file /tmp/bot-sync.json --upsert \
      --upsertFields game-type,name 2>&1
    rm -f /tmp/bot-sync.json
  '"
  echo "=== Done ==="
}

help() {
  cat <<EOF
./deploy.sh — build the organism uberjar locally and manage the prod server.

Commands:
  build            (default) shadow-cljs release + lein uberjar
                   → $LOCAL_JAR
  clean            wipe .shadow-cljs, target, resources/public/js, node_modules,
                   reinstall npm, then build from scratch
  ship             THE deploy, everything in one go: shadow-cljs release,
                   uberjar, NEURON's move server and its weights, uploaded,
                   then a single restart and a timed move on the box
  push             skip build; upload the existing local jar and restart
                   (redeploy the same artifact)
  model            rebuild and ship only NEURON — the move server, the CPU
                   libtorch it links, and the current weights — then restart.
                   Use after training has produced a model worth publishing;
                   the site keeps serving the old one until you do.
  model-check      run the move server on the box and report what one decision
                   costs there. Also the quickest way to tell whether the
                   binary, the libraries and the weights are all in place.
  reap             list the games whose deletion grace period ran out, without
                   removing any. \`ship\` schedules this to run nightly; run it
                   by hand to see what the schedule is about to do.
  reap-now         actually remove them. Deletion does not come back, so look
                   at \`reap\` first.
  tail             ssh + tail -f the remote organism.log (Ctrl-C to quit)
  log [N]          print last N lines of the remote log (default 100)
  status           report whether the remote server process is alive
  bugs [args...]   fetch /eridu/bug-report/dump from playorganism.io over
                   HTTPS into ~/Documents/eridu-bug-reports.jsonl, then run
                   ~/bin/eridu_bug_watch.py --reset --once on it. Extra args
                   pass through (e.g. --spawn-claude). Requires an exported
                   cookie at ~/.config/eridu-cookie.txt — see file header.
  sync-bot --game G --owner O --name N
                   mongoexport a bot from local Mongo, mongoimport it into
                   the prod DB (upsert on game-type + name)
  help, -h, --help show this message

Deploy target:
  $REMOTE_HOST:$REMOTE_DIR
  (server needs only Java and the shipped libtorch; all Node, ClojureScript
   and Rust compilation is local)

NEURON's search budget, if the box turns out too slow or too idle:
  MODEL_SIMS=16 MODEL_THREADS=2 ./deploy.sh model
EOF
}

case "${1:-build}" in
  help|-h|--help)
    help
    ;;
  build)
    build
    ;;
  clean)
    echo "=== Cleaning ==="
    rm -rf .shadow-cljs target resources/public/js node_modules
    npm install
    build
    ;;
  ship)
    build
    build_model
    ship
    ship_model
    install_reaper
    restart
    model_check
    ;;
  push)
    ship
    restart
    ;;
  model)
    build_model
    ship_model
    restart
    model_check
    ;;
  model-check)
    model_check
    ;;
  reap)
    reap --dry-run
    ;;
  reap-now)
    reap ""
    ;;
  tail)
    echo "=== Following journal for $SERVICE on $REMOTE_HOST (Ctrl-C to quit) ==="
    ssh -t "$REMOTE_HOST" "journalctl -u $SERVICE -f"
    ;;
  log)
    lines="${2:-100}"
    ssh "$REMOTE_HOST" "journalctl -u $SERVICE -n $lines --no-pager"
    ;;
  status)
    ssh "$REMOTE_HOST" "systemctl status $SERVICE --no-pager || true"
    ;;
  sync-bot)
    sync_bot "$@"
    ;;
  *)
    echo "Unknown command: $1"
    echo "Run ./deploy.sh help for available commands."
    exit 1
    ;;
esac
