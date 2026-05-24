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

video:       ## full gameplay animation with the new food -> scene/*.mp4
	rm -f /tmp/play4/f*.png
	TURN_LIMIT=0 $(BLENDER) $(BFLAGS) build_play_real.py
	ffmpeg -y -framerate 24 -i /tmp/play4/f%04d.png -c:v libx264 -pix_fmt yuv420p \
		scene/zach-dan-ryan-play-newfood.mp4

all: food measure preview   ## everything except the video

.PHONY: food stl measure preview video all
