#!/usr/bin/env bash
# Build the move server for a host with no GPU.
#
# `build-gpu.sh` links the CUDA libtorch — 1.6 GB of libraries plus a CUDA
# runtime, and a driver to load them. A web server needs none of that: it only
# plays a trained model, one position at a time, on the CPU. The `serve` feature
# links libtorch and leaves the trainer (and its CUDA shim) out entirely.
set -euo pipefail
cd "$(dirname "$0")/.."

# Whichever python built .venv-serve — not a hardcoded version, since the
# interpreter that makes the venv here need not be the one on the next machine.
LIBTORCH_ROOT="${LIBTORCH_SERVE:-$(echo "$PWD"/.venv-serve/lib/python*/site-packages/torch)}"
if [ ! -d "$LIBTORCH_ROOT/lib" ]; then
  echo "no CPU libtorch at $LIBTORCH_ROOT" >&2
  echo "create one with:" >&2
  echo "  python3 -m venv .venv-serve" >&2
  echo "  .venv-serve/bin/pip install --index-url https://download.pytorch.org/whl/cpu torch==2.9.1" >&2
  exit 1
fi

export LIBTORCH="$LIBTORCH_ROOT"
export LIBTORCH_CXX11_ABI=1
# tch 0.22 targets 2.9.0; our runtime is the 2.9.1 maintenance release.
export LIBTORCH_BYPASS_VERSION_CHECK=1
export LD_LIBRARY_PATH="$LIBTORCH/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# A target dir of its own: the CPU and GPU builds differ by feature, and sharing
# one would make each rebuild the whole tree after the other.
cargo build --manifest-path native/Cargo.toml --target-dir native/target-serve \
  --no-default-features --features serve --release -j 2 "$@"

echo "built native/target-serve/release/organism-train"
