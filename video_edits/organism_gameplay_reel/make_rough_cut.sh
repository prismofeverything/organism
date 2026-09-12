#!/usr/bin/env bash
set -euo pipefail

# Builds: main 01, gameplay 01, main 02, gameplay 02 ... main 05.
# It only reads source media and writes this project's interstitials/ and exports/.

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$PROJECT_DIR/../.." && pwd)"
INCOMING_DIR="$PROJECT_DIR/incoming"
INSERT_DIR="$PROJECT_DIR/interstitials"
EXPORT_DIR="$PROJECT_DIR/exports"
WORK_DIR="$PROJECT_DIR/.work"
# Kickstarter's Zach/Dan/Ryan replay and its selected soundtrack pairing.
GAMEPLAY_SOURCE="${ORGANISM_GAMEPLAY_SOURCE:-$HOME/Downloads/ORGANISM videos/zach-dan-ryan-play.mp4}"
MUSIC_SOURCE="${ORGANISM_MUSIC_SOURCE:-$HOME/prismofeverything/jhokolabaluptalinstograrain/jhokolabaluptalinstograrain/jhokolabaluptalinstograrain.mp3}"
GAMEPLAY_STARTS=(4 18 32 46 60 74)
# 0 is left/top; 0.5 is the centre; 1 is right/bottom.  Use these to move the
# rectangular gameplay crop if a particular game position deserves the focus.
GAMEPLAY_FOCUS_X=(0.5 0.5 0.5 0.5 0.5 0.5)
GAMEPLAY_FOCUS_Y=(0.5 0.5 0.5 0.5 0.5 0.5)
# Phone movement at the beginning/end is treated more conservatively.  The
# two treatments overlap for a sigmoid crossfade, so there is no hard jump.
# The ramp starts after this brief edge treatment and is complete at 2*EDGE,
# just before speech begins in the phone recordings.
EDGE_SECONDS=2.5
EDGE_AUDIO_FILTER='highpass=f=80,afftdn=nr=15:nf=-45:tn=0,acompressor=threshold=-18dB:ratio=2:attack=20:release=250:makeup=0dB,alimiter=limit=0.95'
VOICE_AUDIO_FILTER='highpass=f=80,afftdn=nr=10:nf=-45:tn=1,acompressor=threshold=-32dB:ratio=3:attack=20:release=250:makeup=8dB,loudnorm=I=-16:LRA=11:TP=-1.5'

mkdir -p "$INSERT_DIR" "$EXPORT_DIR" "$WORK_DIR"

for index in 01 02 03 04 05; do
  if [[ ! -s "$INCOMING_DIR/$index.mp4" ]]; then
    echo "Missing $INCOMING_DIR/$index.mp4. Add and number all five main clips first." >&2
    exit 1
  fi
done
for source in "$GAMEPLAY_SOURCE" "$MUSIC_SOURCE"; do
  if [[ ! -s "$source" ]]; then
    echo "Missing source: $source" >&2
    exit 1
  fi
done

# The first main clip supplies the master canvas.  This keeps the reel in the
# same shape as the footage the viewer is meant to focus on, not in the shape
# of the square gameplay source.
mapfile -t FRAME_SIZE < <(ffprobe -v error -select_streams v:0 \
  -show_entries stream=width,height -of default=noprint_wrappers=1:nokey=1 \
  "$INCOMING_DIR/01.mp4")
TARGET_W="${FRAME_SIZE[0]}"
TARGET_H="${FRAME_SIZE[1]}"
TARGET_W=$((TARGET_W - TARGET_W % 2))
TARGET_H=$((TARGET_H - TARGET_H % 2))
if (( TARGET_W < 2 || TARGET_H < 2 )); then
  echo "Could not read a usable video frame size from $INCOMING_DIR/01.mp4" >&2
  exit 1
fi
NORMALIZE_FILTER="scale=${TARGET_W}:${TARGET_H}:force_original_aspect_ratio=increase:flags=lanczos,crop=${TARGET_W}:${TARGET_H}:(in_w-out_w)/2:(in_h-out_h)/2,fps=24,setsar=1"

# Render six 24-fps inserts that fill the first clip's rectangular canvas. Each
# starts at a different moment in the Zach/Dan/Ryan game and takes its
# corresponding three-second music section.
for i in 0 1 2 3 4 5; do
  number=$(printf '%02d' "$((i + 1))")
  start="${GAMEPLAY_STARTS[$i]}"
  focus_x="${GAMEPLAY_FOCUS_X[$i]}"
  focus_y="${GAMEPLAY_FOCUS_Y[$i]}"
  ffmpeg -y -hide_banner \
    -ss "$start" -t 3 -i "$GAMEPLAY_SOURCE" \
    -ss "$start" -t 3 -i "$MUSIC_SOURCE" \
    -filter:v "scale=${TARGET_W}:${TARGET_H}:force_original_aspect_ratio=increase:flags=lanczos,crop=${TARGET_W}:${TARGET_H}:(in_w-out_w)*${focus_x}:(in_h-out_h)*${focus_y},fps=24,setsar=1" \
    -map 0:v:0 -map 1:a:0 -shortest \
    -c:v libx264 -crf 18 -pix_fmt yuv420p -c:a aac -ar 48000 -b:a 192k \
    "$INSERT_DIR/gameplay-$number.mp4"
done

has_audio() {
  ffprobe -v error -select_streams a:0 -show_entries stream=index \
    -of csv=p=0 "$1" | grep -q .
}

# Convert the five user clips to one dependable delivery format before joining.
# Some render exports have no audio stream, so give those silent stereo audio.
for index in 01 02 03 04 05; do
  source="$INCOMING_DIR/$index.mp4"
  output="$WORK_DIR/main-$index.mp4"
  duration=$(ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "$source")
  edge_end=$(awk -v e="$EDGE_SECONDS" 'BEGIN { printf "%.3f", 2*e }')
  edge_tail_start=$(awk -v d="$duration" -v e="$EDGE_SECONDS" 'BEGIN { printf "%.3f", d-2*e }')
  # Keep 0..30 seconds in the conservative branch and 15..duration-15 in
  # the voice branch.  A 15-second double-exponential sigmoid crossfade
  # blends matching content from each branch at both boundaries.
  audio_graph="[0:a]asplit=3[intro][middle][outro];[intro]atrim=start=0:end=${edge_end},asetpts=PTS-STARTPTS,${EDGE_AUDIO_FILTER},aformat=sample_rates=48000:channel_layouts=stereo[a0];[middle]atrim=start=${EDGE_SECONDS}:end=${edge_tail_start},asetpts=PTS-STARTPTS,${VOICE_AUDIO_FILTER},aformat=sample_rates=48000:channel_layouts=stereo[a1];[outro]atrim=start=${edge_tail_start},asetpts=PTS-STARTPTS,${EDGE_AUDIO_FILTER},aformat=sample_rates=48000:channel_layouts=stereo[a2];[a0][a1]acrossfade=d=${EDGE_SECONDS}:c1=desi:c2=desi[x];[x][a2]acrossfade=d=${EDGE_SECONDS}:c1=desi:c2=desi[a]"
  if has_audio "$source"; then
    ffmpeg -y -hide_banner -i "$source" \
      -filter_complex "[0:v]${NORMALIZE_FILTER}[v];${audio_graph}" \
      -map '[v]' -map '[a]' -c:v libx264 -crf 18 -pix_fmt yuv420p -c:a aac -b:a 192k \
      "$output"
  else
    ffmpeg -y -hide_banner -i "$source" \
      -f lavfi -i 'anullsrc=channel_layout=stereo:sample_rate=48000' \
      -filter:v "$NORMALIZE_FILTER" \
      -map 0:v:0 -map 1:a:0 -shortest -c:v libx264 -crf 18 -pix_fmt yuv420p -c:a aac -b:a 192k \
      "$output"
  fi
done

LIST_FILE=$(mktemp)
trap 'rm -f "$LIST_FILE"' EXIT
printf "file '%s'\n" "$INSERT_DIR/gameplay-01.mp4" >> "$LIST_FILE"
for index in 01 02 03 04; do
  printf "file '%s'\n" "$WORK_DIR/main-$index.mp4" >> "$LIST_FILE"
  next=$(printf '%02d' "$((10#$index + 1))")
  printf "file '%s'\n" "$INSERT_DIR/gameplay-$next.mp4" >> "$LIST_FILE"
done
printf "file '%s'\n" "$WORK_DIR/main-05.mp4" >> "$LIST_FILE"
printf "file '%s'\n" "$INSERT_DIR/gameplay-06.mp4" >> "$LIST_FILE"

# Every source has now been normalized.  Main-clip audio remains intact; the
# gameplay inserts carry the Kickstarter soundtrack pairing.
ffmpeg -y -hide_banner -f concat -safe 0 -i "$LIST_FILE" \
  -c copy -movflags +faststart \
  "$EXPORT_DIR/organism-gameplay-final.mp4"

echo "Wrote $EXPORT_DIR/organism-gameplay-final.mp4"
