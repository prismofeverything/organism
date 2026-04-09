#!/bin/bash
# Build and package the application.
#
# shadow-cljs caches aggressively — only changed files get recompiled.
# First build: ~60-80s for all 4 targets
# Subsequent:  ~15-30s (only recompiles changed namespaces)
#
# Usage: ./deploy.sh          — build JS + uberjar
#        ./deploy.sh --clean  — clean everything first

set -e
cd "$(dirname "$0")"

if [ "$1" = "--clean" ]; then
  echo "=== Cleaning ==="
  rm -rf .shadow-cljs target node_modules
  npm install
fi

# Ensure node_modules exist
[ -d node_modules ] || npm install

echo "=== Building ClojureScript (shadow-cljs release) ==="
npx shadow-cljs release organism journey oroboros future

echo "=== Building uberjar ==="
lein uberjar

echo "=== Done: target/uberjar/organism.jar ==="
ls -lh target/uberjar/organism.jar 2>/dev/null || echo "(check target/ for jar)"
