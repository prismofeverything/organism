#!/bin/bash
# Faster deploy: pre-build CLJS with caching, then uberjar skips CLJS.
#
# The `:advanced` Closure Compiler builds are the slow part (~1-2 min each).
# This script:
#   1. Checks if each CLJS build's output exists and sources haven't changed
#   2. Only rebuilds CLJS targets that need it
#   3. Runs uberjar with prep-tasks that skip CLJS (already built)
#
# Usage: ./deploy.sh          — incremental (skips unchanged CLJS)
#        ./deploy.sh --clean  — full rebuild

set -e
cd "$(dirname "$0")"

if [ "$1" = "--clean" ]; then
  echo "=== Clean build ==="
  lein clean
  lein uberjar
  exit 0
fi

CHECKSUM_DIR=".build-checksums"
mkdir -p "$CHECKSUM_DIR"

# Checksum all CLJS/CLJC sources
sources_checksum() {
  find src/cljs src/cljc env/prod/cljs -name '*.cljs' -o -name '*.cljc' -o -name '*.clj' 2>/dev/null \
    | sort | xargs sha256sum 2>/dev/null | sha256sum | cut -d' ' -f1
}

CURRENT=$(sources_checksum)
PREVIOUS=""
[ -f "$CHECKSUM_DIR/cljs" ] && PREVIOUS=$(cat "$CHECKSUM_DIR/cljs")

# Check if all three JS outputs exist
ALL_EXIST=true
for js in target/cljsbuild/public/js/organism.js \
          target/cljsbuild/public/js/journey.js \
          target/cljsbuild/public/js/oroboros.js; do
  [ -f "$js" ] || ALL_EXIST=false
done

if [ "$CURRENT" = "$PREVIOUS" ] && [ "$ALL_EXIST" = true ]; then
  echo "=== CLJS unchanged — skipping JS builds ==="
else
  echo "=== Building CLJS (advanced optimization) ==="
  # Build all three in one cljsbuild invocation (shares JVM startup)
  lein with-profile uberjar cljsbuild once organism journey oroboros
  echo "$CURRENT" > "$CHECKSUM_DIR/cljs"
fi

echo "=== Compiling CLJ + packaging uberjar ==="
# Use a profile that skips CLJS prep-tasks
lein with-profile uberjar-nocljs uberjar

echo "=== Done: target/uberjar/organism.jar ==="
