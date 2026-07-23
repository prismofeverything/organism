#!/bin/bash
# Build locally and optionally deploy to the server.
#
# Usage:
#   ./deploy.sh              — build JS + uberjar locally
#   ./deploy.sh build        — same as above
#   ./deploy.sh clean        — clean everything first, then build
#   ./deploy.sh ship         — build + upload jar + restart systemd service
#   ./deploy.sh push         — skip build, just upload + restart
#   ./deploy.sh tail         — follow the remote journal (Ctrl-C to quit)
#   ./deploy.sh log [N]      — print last N journal lines (default 100)
#   ./deploy.sh status       — systemctl status of the remote service
#   ./deploy.sh sync-bot --game journey --owner prismofeverything --name OBO
#                            — push a bot from local MongoDB to remote
#
# Deploy target from $DEPLOY_HOST (default ryan@elephantlaboratories.com).
# The server only needs Java — all Node/ClojureScript compilation
# happens locally.

set -e
cd "$(dirname "$0")"

# Deploy target. Defaults to the domain (correct once DNS points at the new box);
# override for the pre-cutover droplet:  DEPLOY_HOST=ryan@NEW_IP ./deploy.sh ship
REMOTE_HOST="${DEPLOY_HOST:-ryan@elephantlaboratories.com}"
SERVICE="organism"          # systemd unit: HTTP + WebSocket on :11551
REMOTE_DIR="~/organism"
REMOTE_JAR="$REMOTE_DIR/organism.jar"
LOCAL_JAR="target/uberjar/organism.jar"

build() {
  [ -d node_modules ] || npm install

  echo "=== Building ClojureScript (shadow-cljs release) ==="
  npx shadow-cljs release organism journey journey-bots oroboros eridu future

  echo "=== Building uberjar ==="
  lein uberjar

  echo "=== Done: $LOCAL_JAR ==="
  ls -lh "$LOCAL_JAR"
}

ship() {
  echo "=== Uploading jar to $REMOTE_HOST ==="
  scp "$LOCAL_JAR" "$REMOTE_HOST:$REMOTE_JAR"

  echo "=== Restarting $SERVICE via systemd ==="
  # systemd owns the process now (unit installed by the migration's 02 script);
  # NOPASSWD sudoers (also from 02) lets this restart run non-interactively.
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
  ship             build, then scp jar to the server and restart the process
  push             skip build; upload the existing local jar and restart
                   (redeploy the same artifact)
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
  (server only needs Java; all Node/ClojureScript compilation is local)
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
    ship
    ;;
  push)
    ship
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
