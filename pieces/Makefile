# Round-parts pipeline (FOOD + universal connector). See FOOD.md.
# Override paths if needed:  make food BLENDER=/path/to/blender
BLENDER ?= $(HOME)/Downloads/blender-5.1.2-linux-x64/blender
PY      ?= ../.venv/bin/python
BFLAGS  := --background --threads 4 --python
OUT     := $(CURDIR)/renders/food

food:        ## build both fits -> OBJ + STL (print) + .profile.json (metric)
	FOOD_GAP=0.15  FOOD_OUT=$(OUT)/FOOD_nosnap.obj $(BLENDER) $(BFLAGS) build_food.py
	FOOD_GAP=-0.10 FOOD_OUT=$(OUT)/FOOD_snap.obj   $(BLENDER) $(BFLAGS) build_food.py

stl: food    ## (alias: the STL falls out of `make food`)

measure:     ## nesting % (mesh-accurate)
	$(PY) stack_food.py $(OUT)/FOOD_nosnap.profile.json

preview:     ## CPU 3D + cross-section render of the built food
	$(PY) render_food.py $(OUT)/FOOD_nosnap.obj

plate:       ## lay out a print plate: 3 slip + 3 snap food + 1 each piece -> one watertight STL
	$(BLENDER) $(BFLAGS) build_print_plate.py
	$(PY) preview_plate.py

video:       ## full gameplay animation -> scene/*.mp4 (gentle: nice + 2 threads; slow but won't freeze)
	rm -f /tmp/play4/f*.png
	TURN_LIMIT=0 nice -n 19 $(BLENDER) --background --threads 2 --python build_play_real.py
	nice -n 19 ffmpeg -y -framerate 24 -i /tmp/play4/f%04d.png -c:v libx264 -pix_fmt yuv420p \
		scene/zach-dan-ryan-play-newfood.mp4

all: food measure preview   ## everything except the video

.PHONY: food stl measure preview plate video all
