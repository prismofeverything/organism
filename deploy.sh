#!/bin/bash
# Build locally and optionally deploy to the server.
#
# Usage:
#   ./deploy.sh              — build JS + uberjar locally
#   ./deploy.sh build        — same as above
#   ./deploy.sh clean        — clean everything first, then build
#   ./deploy.sh ship         — build + upload jar + restart server
#   ./deploy.sh push         — skip build, just upload + restart
#   ./deploy.sh tail         — tail the remote log file (Ctrl-C to quit)
#   ./deploy.sh log [N]      — print last N lines of remote log (default 100)
#   ./deploy.sh status       — show whether the remote server is running
#   ./deploy.sh sync-bot --game journey --owner prismofeverything --name OBO
#                            — push a bot from local MongoDB to remote
#
# The server only needs Java — all Node/ClojureScript compilation
# happens locally.

set -e
cd "$(dirname "$0")"

REMOTE_HOST="ryan@elephantlaboratories.com"
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

  echo "=== Restarting server ==="
  ssh "$REMOTE_HOST" "bash -lc '
    cd $REMOTE_DIR
    if [ -f organism.pid ]; then
      kill \$(cat organism.pid) 2>/dev/null || true
      rm -f organism.pid
      sleep 2
    fi
    nohup java -jar organism.jar > organism.log 2>&1 &
    echo \$! > organism.pid
    sleep 3
    if kill -0 \$(cat organism.pid) 2>/dev/null; then
      echo \"Server started (pid \$(cat organism.pid)), port 11551\"
    else
      echo \"ERROR: server failed to start. Check organism.log\"
      tail -20 organism.log
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

case "${1:-build}" in
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
    echo "=== Tailing $REMOTE_HOST:$REMOTE_DIR/organism.log (Ctrl-C to quit) ==="
    ssh -t "$REMOTE_HOST" "tail -f $REMOTE_DIR/organism.log"
    ;;
  log)
    lines="${2:-100}"
    ssh "$REMOTE_HOST" "tail -n $lines $REMOTE_DIR/organism.log"
    ;;
  status)
    ssh "$REMOTE_HOST" "bash -lc '
      cd $REMOTE_DIR
      if [ -f organism.pid ] && kill -0 \$(cat organism.pid) 2>/dev/null; then
        echo \"running (pid \$(cat organism.pid))\"
      else
        echo \"not running\"
      fi
    '"
    ;;
  sync-bot)
    sync_bot "$@"
    ;;
  *)
    echo "Unknown command: $1"
    echo "Commands: build, clean, ship, push, tail, log, status, sync-bot"
    exit 1
    ;;
esac
