#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
# Link directly to the installed C++ runtime; no Python process runs in training.
export LIBTORCH="$PWD/.venv-training/lib/python3.12/site-packages/torch"
export LIBTORCH_CXX11_ABI=1
# tch 0.22 targets 2.9.0; our runtime is the 2.9.1 maintenance release.
export LIBTORCH_BYPASS_VERSION_CHECK=1
export LD_LIBRARY_PATH="$LIBTORCH/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
cargo build --manifest-path native/Cargo.toml --target-dir native/target --features gpu --release -j 2 "$@"
