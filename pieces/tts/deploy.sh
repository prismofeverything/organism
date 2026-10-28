#!/usr/bin/env bash
# Copy the built mod (make tts) to a machine that runs Tabletop Simulator: the
# assets beside TTS's own data, and the save into its Saves folder with its
# file:// URLs pointed at that copy. A save built with BASE=https://... keeps
# its URLs as they are.
#
# Steam installed as a snap keeps TTS under ~/snap/steam/common and can only
# read files there, so the assets go wherever TTS's data is.
#
#   tts/deploy.sh pattern@orbit
set -euo pipefail
host=${1:?usage: deploy.sh user@host}
here=$(cd "$(dirname "$0")" && pwd)
root=$(ssh "$host" 'for d in "$HOME/snap/steam/common" "$HOME"; do
                      [ -d "$d/.local/share/Tabletop Simulator" ] && { echo "$d"; exit; }; done; echo "$HOME"')
saves="$root/.local/share/Tabletop Simulator/Saves"
assets="$root/organism-tts/assets"
ssh "$host" "mkdir -p '$saves'"
if grep -q "file://$here/out/assets/" "$here/out/Organism.json"; then
  ssh "$host" "mkdir -p '$assets'"
  rsync -a --delete "$here/out/assets/" "$host:$assets/"
else
  assets="(hosted; none copied)"
fi
sed "s|file://$here/out/assets/|file://$assets/|g" "$here/out/Organism.json" \
  | ssh "$host" "cat > '$saves/Organism.json'"
ssh "$host" "cat > '$saves/Organism.png'" < "$here/out/Organism.png"
echo "deployed to $host:$saves/Organism.json (assets in $assets)"
