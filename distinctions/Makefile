# DISTINCTIONS: six two-way distinctions, 64 cards.
#
# make key   -> out/distinctions-key.{svg,png}
# make cards -> out/mpc/faces/*.png (64), out/mpc/back.png, out/proof.png
#              ../.venv/bin/python make_cards.py pairs -> out/mpc/back-pairs.png
#              ../.venv/bin/python jigsaw_tiling.py [--vary 2] -> jigsaw-{3,2}.json, then
#              ../.venv/bin/python make_cards.py jigsaw -> out/mpc/back-jigsaw-{3,2}.png
#              add --turn SEED to pairs/jigsaw for back-*-turned-SEED (each ring turned at random)
#               make faces PRESET=tgc for The Game Crafter's size

PY     ?= ../.venv/bin/python
PRESET ?= mpc
ROOT   := $(CURDIR)

.PHONY: key cards faces back proof clean

cards: faces back proof
faces: make_cards.py make_key.py; $(PY) make_cards.py faces --preset $(PRESET)
back:  make_cards.py make_key.py; $(PY) make_cards.py back  --preset $(PRESET)
proof: make_cards.py make_key.py; $(PY) make_cards.py proof

key: make_key.py
	$(PY) make_key.py
	rsvg-convert -w 3200 $(ROOT)/out/distinctions-key.svg -o $(ROOT)/out/distinctions-key.png
	cp $(ROOT)/out/distinctions-key.svg $(ROOT)/../resources/public/distinctions/key.svg

clean:
	rm -rf $(ROOT)/out
