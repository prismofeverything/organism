#!/bin/bash
# Build locally and optionally deploy to the server.
#
# Usage:
#   ./deploy.sh              — build JS + uberjar locally
#   ./deploy.sh --clean      — clean everything first, then build
#   ./deploy.sh --ship       — build + upload jar + restart server
#   ./deploy.sh --ship-only  — skip build, just upload + restart
#   ./deploy.sh --tail       — tail the remote log file (Ctrl-C to quit)
#   ./deploy.sh --log [N]    — print last N lines of remote log (default 100)
#   ./deploy.sh --status     — show whether the remote server is running
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
  # Ensure node_modules exist
  [ -d node_modules ] || npm install

  echo "=== Building ClojureScript (shadow-cljs release) ==="
  npx shadow-cljs release organism journey oroboros future

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
    # Stop existing server (if running)
    if [ -f organism.pid ]; then
      kill \$(cat organism.pid) 2>/dev/null || true
      rm -f organism.pid
      sleep 2
    fi
    # Start new server
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

case "${1:-}" in
  --clean)
    echo "=== Cleaning ==="
    rm -rf .shadow-cljs target resources/public/js node_modules
    npm install
    build
    ;;
  --ship)
    build
    ship
    ;;
  --ship-only)
    ship
    ;;
  --tail)
    echo "=== Tailing $REMOTE_HOST:$REMOTE_DIR/organism.log (Ctrl-C to quit) ==="
    ssh -t "$REMOTE_HOST" "tail -f $REMOTE_DIR/organism.log"
    ;;
  --log)
    lines="${2:-100}"
    ssh "$REMOTE_HOST" "tail -n $lines $REMOTE_DIR/organism.log"
    ;;
  --status)
    ssh "$REMOTE_HOST" "bash -lc '
      cd $REMOTE_DIR
      if [ -f organism.pid ] && kill -0 \$(cat organism.pid) 2>/dev/null; then
        echo \"running (pid \$(cat organism.pid))\"
      else
        echo \"not running\"
      fi
    '"
    ;;
  *)
    build
    ;;
esac
