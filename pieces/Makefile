# Organism piece pipeline: SVG silhouettes -> blank bodies -> connector-grafted
# pieces -> combined print plate STL.  See FOOD.md and GRAFTING.md.
#
# One-shot:  make print
# Per stage: make blanks | grafts | food | plate
# Override:  make print BLENDER=/path/to/blender  (also read by meshlib/build.py)

# auto-detect the newest installed blender-5.1.* (machines differ: 5.1.1 here, 5.1.2 elsewhere);
# override explicitly with `make <target> BLENDER=/path/to/blender`.
BLENDER_AUTO := $(shell ls -d $(HOME)/Downloads/blender-5.1.*/blender 2>/dev/null | sort -V | tail -1)
BLENDER ?= $(or $(BLENDER_AUTO),$(HOME)/Downloads/blender-5.1.2-linux-x64/blender)
PY      ?= ../.venv/bin/python
UV      ?= uv
BFLAGS  := --background --threads 4 --python

ROOT    := $(CURDIR)
OUT     := $(ROOT)/out
FOOD    := $(ROOT)/renders/food

BLANKS    := $(OUT)/EAT.obj $(OUT)/MOVE.obj $(OUT)/GROW.obj
GRAFTS    := $(OUT)/EAT_graft.obj $(OUT)/MOVE_graft.obj $(OUT)/GROW_graft.obj
FOOD_STLS := $(FOOD)/FOOD_nosnap.stl $(FOOD)/FOOD_snap.stl
PLATE_STL := $(FOOD)/print_plate.stl
PIECES_PLATE_STL := $(FOOD)/pieces_plate.stl

help:                       ## list targets (default)
	@awk 'BEGIN {FS = ":.*##"} /^[a-zA-Z_-]+:.*##/ {printf "  %-10s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

print: $(PLATE_STL)         ## end-to-end -> slicer-ready print_plate.stl
	@echo ""
	@echo "Slicer input ready: $(PLATE_STL)"

blanks: $(BLANKS)           ## 3 sculpt-ready blank bodies in out/
grafts: $(GRAFTS)           ## 3 connector-grafted pieces in out/ (needs blanks)
food:   $(FOOD_STLS)        ## slip + snap FOOD STLs in renders/food/
plate:  $(PLATE_STL)        ## combined print plate STL (needs grafts + food)
pieceplate: $(PIECES_PLATE_STL) ## pieces-only plate STL (EAT/MOVE/GROW, no food -> separate colour)
all:    blanks grafts food plate   ## rebuild everything (skips video)

$(BLANKS): meshlib/build.py meshlib/graft_lib.py
	BLENDER=$(BLENDER) $(UV) run python meshlib/build.py

$(GRAFTS): $(BLANKS) meshlib/build.py meshlib/graft_lib.py meshlib/connector_field.py
	BLENDER=$(BLENDER) $(UV) run python meshlib/build.py graft EAT GROW MOVE

$(FOOD)/FOOD_nosnap.stl: build_food.py sor.py
	FOOD_GAP=0.15  FOOD_OUT=$(FOOD)/FOOD_nosnap.obj $(BLENDER) $(BFLAGS) build_food.py
	$(PY) render_food.py $(FOOD)/FOOD_nosnap.obj

$(FOOD)/FOOD_snap.stl: build_food.py sor.py
	FOOD_GAP=-0.10 FOOD_OUT=$(FOOD)/FOOD_snap.obj   $(BLENDER) $(BFLAGS) build_food.py
	$(PY) render_food.py $(FOOD)/FOOD_snap.obj

$(PLATE_STL): $(GRAFTS) $(FOOD_STLS) build_print_plate.py strip_slivers.py
	$(BLENDER) $(BFLAGS) build_print_plate.py
	$(PY) strip_slivers.py $(PLATE_STL)
	$(PY) preview_plate.py

# pieces-only plate: EAT/MOVE/GROW in one row, no food (food is a different filament colour).
$(PIECES_PLATE_STL): $(GRAFTS) build_print_plate.py strip_slivers.py
	PIECES_ONLY=true $(BLENDER) $(BFLAGS) build_print_plate.py
	$(PY) strip_slivers.py $(PIECES_PLATE_STL)
	PIECES_ONLY=true $(PY) preview_plate.py

measure: $(FOOD_STLS)       ## FOOD nesting % measurement
	$(PY) stack_food.py $(FOOD)/FOOD_nosnap.profile.json

preview: $(FOOD_STLS)       ## CPU 3D + cross-section render of FOOD
	$(PY) render_food.py $(FOOD)/FOOD_nosnap.obj

# gameplay video: configurable per game. Defaults reproduce the canonical render.
#   make video                              # default: zach-dan-ryan.json
#   make video OGF=/path/to/other.json      # any OGF
#   make video FPT=12 SEED=42               # slower playback, different camera
OGF       ?= $(ROOT)/../ogf/zach-dan-ryan.json
OGF_NAME  := $(basename $(notdir $(OGF)))
# frames -> pieces/renders/play (gitignored); override FR=/path for a scratch disk.
# NB: no inline comment here — Make keeps trailing whitespace, which breaks $(FR)/f*.png.
FR        ?= $(ROOT)/renders/play
FPT       ?= 8                                         # frames per game turn (8 = 0.33s at 24fps)
SEED      ?= 1234                                      # camera RNG seed
VIDEO_OUT ?= $(ROOT)/scene/$(OGF_NAME)-play.mp4

video:                      ## gameplay animation -> scene/<game>-play.mp4 (slow; nice + 2 threads)
	mkdir -p $(FR) && rm -f $(FR)/f*.png
	OGF=$(OGF) FR=$(FR) FPT=$(FPT) SEED=$(SEED) TURN_LIMIT=0 \
		nice -n 19 $(BLENDER) --background --threads 2 --python build_play_real.py
	nice -n 19 ffmpeg -y -framerate 24 -i $(FR)/f%04d.png -c:v libx264 -pix_fmt yuv420p $(VIDEO_OUT)
	@echo "wrote $(VIDEO_OUT)"

clean:                      ## nuke out/ blanks+grafts and plate STLs (food STLs preserved)
	rm -f $(BLANKS) $(GRAFTS) $(PLATE_STL) $(PIECES_PLATE_STL)

.PHONY: help print blanks grafts food plate pieceplate all measure preview video clean
.DEFAULT_GOAL := help
