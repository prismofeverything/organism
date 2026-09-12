# Organism gameplay reel

This is the working folder for a five-clip reel.  It is arranged as an edit
sequence, so more reels can reuse the same sources and rendering script.

## Intended sequence

gameplay insert → `01` → gameplay insert → `02` → gameplay insert → `03` →
gameplay insert → `04` → gameplay insert → `05` → gameplay insert

The inserts use six different moments from the newer Zach/Dan/Ryan game render
(June 19, 2026). They use
the Kickstarter's paired **jhokolabaluptalinstograrain** soundtrack during those three seconds; the music does not
run over the five main clips.

## Add the five clips

The Google Drive downloads currently visible in `~/Downloads` are empty ZIP
placeholders.  Once they contain data, extract them and put the desired files
here, naming them in the intended order:

```text
incoming/01.mp4
incoming/02.mp4
incoming/03.mp4
incoming/04.mp4
incoming/05.mp4
```

For this reel, the original phone filenames are preserved.  The numbered
entries are lightweight links in chronological order, so the render script
can use them without renaming or duplicating the camera files.

Then run `./make_rough_cut.sh`.  It creates:

- `interstitials/gameplay-01.mp4` through `gameplay-06.mp4` — reusable,
  three-second gameplay clips with music;
- `exports/organism-gameplay-final.mp4` — the final assembled video.

For audio timing or level changes after the picture is assembled, use
`./make_audio_only.sh`.  It processes the five audio tracks and swaps the new
track into the existing video with the video stream copied unchanged.

The first main clip sets the export's dimensions and aspect ratio.  Each
gameplay insert fills that rectangular frame by zooming and cropping the
square source, so it never adds square borders.  The source clips stay
untouched.

The first and last 5 seconds of each phone clip use a conservative denoise
and limiter treatment blended with the stronger middle voice chain through a
2.5-second double-exponential sigmoid.  The ramp is complete before speaking
begins, while the close-mic hiss at the edges is kept from jumping up.

## Revising the order

Rename the files in `incoming/`, or change the six `GAMEPLAY_STARTS` values
near the top of `make_rough_cut.sh`.  The `GAMEPLAY_FOCUS_X` and
`GAMEPLAY_FOCUS_Y` values set which part of the gameplay image fills each
rectangle: `0` is left/top, `0.5` is centre, and `1` is right/bottom.
`timeline.csv` is the editable shot list and keeps the intended order visible
without opening an editor.

For a final edit with trim points, titles, or different audio treatment, open
the source clips and generated interstitials together in Kdenlive.  The
generated rough cut gives that project a concrete starting sequence.
