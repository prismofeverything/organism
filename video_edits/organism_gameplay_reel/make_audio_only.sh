#!/usr/bin/env bash
set -euo pipefail

# Rebuild only the audio and mux it into an existing assembled video.
PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$PROJECT_DIR/../.." && pwd)"
INCOMING_DIR="$PROJECT_DIR/incoming"
INSERT_DIR="$PROJECT_DIR/interstitials"
EXPORT_DIR="$PROJECT_DIR/exports"
WORK_DIR="$PROJECT_DIR/.audio-work"
VIDEO_SOURCE="${ORGANISM_VIDEO_SOURCE:-$EXPORT_DIR/organism-gameplay-final.mp4}"
GAMEPLAY_SOURCE="${ORGANISM_GAMEPLAY_SOURCE:-$HOME/Downloads/ORGANISM videos/zach-dan-ryan-play.mp4}"
MUSIC_SOURCE="${ORGANISM_MUSIC_SOURCE:-$HOME/prismofeverything/jhokolabaluptalinstograrain/jhokolabaluptalinstograrain/jhokolabaluptalinstograrain.mp3}"
GAMEPLAY_STARTS=(4 18 32 46 60 74)
EDGE_SECONDS=2.5
EDGE_AUDIO_FILTER='highpass=f=80,afftdn=nr=15:nf=-45:tn=0,acompressor=threshold=-18dB:ratio=2:attack=20:release=250:makeup=0dB,alimiter=limit=0.95'
VOICE_AUDIO_FILTER='highpass=f=80,afftdn=nr=10:nf=-45:tn=1,acompressor=threshold=-32dB:ratio=3:attack=20:release=250:makeup=8dB,loudnorm=I=-16:LRA=11:TP=-1.5'

mkdir -p "$WORK_DIR" "$INSERT_DIR" "$EXPORT_DIR"
for index in 01 02 03 04 05; do
  [[ -s "$INCOMING_DIR/$index.mp4" ]] || { echo "Missing $INCOMING_DIR/$index.mp4" >&2; exit 1; }
done
for source in "$VIDEO_SOURCE" "$GAMEPLAY_SOURCE" "$MUSIC_SOURCE"; do
  [[ -s "$source" ]] || { echo "Missing source: $source" >&2; exit 1; }
done

has_audio() { ffprobe -v error -select_streams a:0 -show_entries stream=index -of csv=p=0 "$1" | grep -q .; }

# Produce six matching three-second gameplay audio/video inserts if needed.
for i in 0 1 2 3 4 5; do
  number=$(printf '%02d' "$((i + 1))")
  if [[ ! -s "$INSERT_DIR/gameplay-$number.mp4" ]]; then
    start="${GAMEPLAY_STARTS[$i]}"
    ffmpeg -y -hide_banner -loglevel error -ss "$start" -t 3 -i "$GAMEPLAY_SOURCE" \
      -ss "$start" -t 3 -i "$MUSIC_SOURCE" \
      -filter:v 'scale=1920:1080:force_original_aspect_ratio=increase:flags=lanczos,crop=1920:1080:(in_w-out_w)*0.5:(in_h-out_h)*0.5,fps=24,setsar=1' \
      -map 0:v:0 -map 1:a:0 -shortest -c:v libx264 -crf 18 -pix_fmt yuv420p \
      -c:a aac -ar 48000 -b:a 192k "$INSERT_DIR/gameplay-$number.mp4"
  fi
  start="${GAMEPLAY_STARTS[$i]}"
  ffmpeg -y -hide_banner -loglevel error -ss "$start" -t 3 -i "$MUSIC_SOURCE" \
    -af aresample=48000 -c:a aac -ar 48000 -ac 2 -b:a 192k \
    "$WORK_DIR/gameplay-audio-$number.m4a"
done

for index in 01 02 03 04 05; do
  source="$INCOMING_DIR/$index.mp4"
  duration=$(ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 "$source")
  edge_end=$(awk -v e="$EDGE_SECONDS" 'BEGIN { printf "%.3f", 2*e }')
  edge_tail_start=$(awk -v d="$duration" -v e="$EDGE_SECONDS" 'BEGIN { printf "%.3f", d-2*e }')
  middle_end=$(awk -v d="$duration" -v e="$EDGE_SECONDS" 'BEGIN { printf "%.3f", d-e }')
  graph="[0:a]asplit=3[intro][middle][outro];[intro]atrim=start=0:end=${edge_end},asetpts=PTS-STARTPTS,${EDGE_AUDIO_FILTER},aformat=sample_rates=48000:channel_layouts=stereo[a0];[middle]atrim=start=${EDGE_SECONDS}:end=${middle_end},asetpts=PTS-STARTPTS,${VOICE_AUDIO_FILTER},aformat=sample_rates=48000:channel_layouts=stereo[a1];[outro]atrim=start=${edge_tail_start},asetpts=PTS-STARTPTS,${EDGE_AUDIO_FILTER},aformat=sample_rates=48000:channel_layouts=stereo[a2];[a0][a1]acrossfade=d=${EDGE_SECONDS}:c1=desi:c2=desi[x];[x][a2]acrossfade=d=${EDGE_SECONDS}:c1=desi:c2=desi[a]"
  output="$WORK_DIR/main-audio-$index.m4a"
  if has_audio "$source"; then
    ffmpeg -y -hide_banner -loglevel error -i "$source" -filter_complex "$graph" \
      -map '[a]' -c:a aac -ar 48000 -ac 2 -b:a 192k "$output"
  else
    ffmpeg -y -hide_banner -loglevel error -f lavfi -i 'anullsrc=channel_layout=stereo:sample_rate=48000' -t "$duration" \
      -c:a aac -ar 48000 -ac 2 -b:a 192k "$output"
  fi
done

LIST_FILE=$(mktemp)
trap 'rm -f "$LIST_FILE"' EXIT
printf "file '%s'\n" "$WORK_DIR/gameplay-audio-01.m4a" >> "$LIST_FILE"
for index in 01 02 03 04; do
  printf "file '%s'\n" "$WORK_DIR/main-audio-$index.m4a" >> "$LIST_FILE"
  next=$(printf '%02d' "$((10#$index + 1))")
  printf "file '%s'\n" "$WORK_DIR/gameplay-audio-$next.m4a" >> "$LIST_FILE"
done
printf "file '%s'\n" "$WORK_DIR/main-audio-05.m4a" >> "$LIST_FILE"
printf "file '%s'\n" "$WORK_DIR/gameplay-audio-06.m4a" >> "$LIST_FILE"
# Decode the small audio-only sequence once at the join.  Stream-copying AAC
# segments from different encoders can leave incompatible headers at joins.
ffmpeg -y -hide_banner -loglevel error -f concat -safe 0 -i "$LIST_FILE" \
  -vn -af aresample=48000 -c:a aac -ar 48000 -ac 2 -b:a 192k "$WORK_DIR/assembled-audio.m4a"

TEMP_OUTPUT="$EXPORT_DIR/.organism-gameplay-final-audio-update.mp4"
ffmpeg -y -hide_banner -loglevel error -i "$VIDEO_SOURCE" -i "$WORK_DIR/assembled-audio.m4a" \
  -map 0:v:0 -map 1:a:0 -c:v copy -c:a copy -shortest -movflags +faststart \
  "$TEMP_OUTPUT"
mv "$TEMP_OUTPUT" "$EXPORT_DIR/organism-gameplay-final.mp4"
echo "Wrote audio-only update to $EXPORT_DIR/organism-gameplay-final.mp4"
