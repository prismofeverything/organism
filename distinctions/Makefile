# DISTINCTIONS: six two-way distinctions, 64 cards.
#
# make key   -> out/distinctions-key.{svg,png}

PY   ?= ../.venv/bin/python
ROOT := $(CURDIR)

.PHONY: key clean

key: make_key.py
	$(PY) make_key.py
	rsvg-convert -w 3200 $(ROOT)/out/distinctions-key.svg -o $(ROOT)/out/distinctions-key.png
	cp $(ROOT)/out/distinctions-key.svg $(ROOT)/../resources/public/distinctions/key.svg

clean:
	rm -rf $(ROOT)/out
