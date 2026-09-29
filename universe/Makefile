# UNIVERSE card pipeline: inked source PNGs -> alpha mattes -> 60 card faces,
# a back, and print-ready sheets.  See README.md.
#
# One-shot:  make all
# Per stage: make shapes | faces | back | sheets | proof
# Override:  make faces PRESET=tgc   |   make sheets PAPER=a4

PY     ?= ../.venv/bin/python
PRESET ?= mpc
PAPER  ?= letter

ROOT   := $(CURDIR)
SRC    := $(wildcard $(ROOT)/inputs/*.png)
MATTES := $(patsubst $(ROOT)/inputs/%.png,$(ROOT)/shapes/%.png,$(SRC))
RINGS  := $(ROOT)/shapes/rings.json

.PHONY: all shapes faces back sheets proof palette chart lattice space pyramid seven key web explore clean distclean

all: faces back sheets proof palette chart lattice space pyramid key web

shapes: $(MATTES)

$(MATTES) $(ROOT)/shapes/shapes.json &: extract_shapes.py $(SRC)
	$(PY) extract_shapes.py

# rosette packing depends on the mattes' outlines, so it is rebuilt with them
$(RINGS): deck.py $(ROOT)/shapes/shapes.json
	$(PY) -c "import deck; deck.ring_table(rebuild=True)"

faces: $(RINGS); $(PY) make_cards.py faces --preset $(PRESET)
back:  $(RINGS); $(PY) make_cards.py back  --preset $(PRESET)
sheets: $(RINGS); $(PY) make_cards.py sheets --paper $(PAPER)
proof: $(RINGS); $(PY) make_cards.py proof
palette: ; $(PY) make_cards.py palette

# exhaustive enumeration of all C(60,5) hands; ~15s, so it is cached
$(ROOT)/hands.json: hands.py deck.py
	$(PY) hands.py

chart: $(ROOT)/hands.json make_chart.py $(RINGS)
	$(PY) make_chart.py

lattice: $(ROOT)/hands.json make_lattice.py
	$(PY) make_lattice.py

space: $(ROOT)/hands.json make_space.py
	$(PY) make_space.py

# the shapes as SVG outlines, so the figure below can be all vector
$(ROOT)/shapes/paths.json: trace_shapes.py $(ROOT)/shapes/shapes.json
	$(PY) trace_shapes.py

pyramid: $(ROOT)/hands.json $(ROOT)/shapes/paths.json $(RINGS) make_pyramid.py
	$(PY) make_pyramid.py
	rsvg-convert -w 3700 $(ROOT)/out/universe-pyramid.svg -o $(ROOT)/out/universe-pyramid.png
	cp $(ROOT)/out/universe-pyramid.svg $(ROOT)/../resources/public/universe/hands-five.svg

# best five of seven: sampled (C(60,7) is too many to enumerate), ~2 min at 10M
$(ROOT)/hands7.json: hands7.py hands_n.py holdem_n.py
	$(PY) hands7.py --trials 10000000

seven: $(ROOT)/hands7.json $(ROOT)/shapes/paths.json $(RINGS) make_pyramid.py
	$(PY) make_pyramid.py --seven
	rsvg-convert -w 3700 $(ROOT)/out/universe-pyramid-seven.svg -o $(ROOT)/out/universe-pyramid-seven.png
	cp $(ROOT)/out/universe-pyramid-seven.svg $(ROOT)/../resources/public/universe/hands-seven.svg

# the deck's geometry as data for the browser, so the online game draws the
# same card the printer does -- see src/cljs/universe/card.cljs
WEB := $(ROOT)/../resources/public/universe/deck.json

web: $(WEB)
$(WEB): make_web.py deck.py $(ROOT)/shapes/paths.json $(RINGS)
	$(PY) make_web.py

key: $(ROOT)/shapes/paths.json $(RINGS) make_key.py make_pyramid.py
	$(PY) make_key.py
	rsvg-convert -w 3200 $(ROOT)/out/universe-key.svg -o $(ROOT)/out/universe-key.png

# What a deck with a different number of values would look like. V=5 is the
# real deck; past it a sequence and a hand of five different numbers stop being
# the same thing, which is where the junk hand comes from. See README.
VALUES ?= 7

explore: $(ROOT)/shapes/paths.json $(RINGS) make_pyramid.py make_key.py hands_n.py holdem_n.py
	$(PY) holdem_n.py --trials 120000
	$(PY) make_key.py --values $(VALUES)
	$(PY) make_pyramid.py --values $(VALUES)
	rsvg-convert -w 3200 $(ROOT)/out/universe-key-$(VALUES).svg \
	  -o $(ROOT)/out/universe-key-$(VALUES).png
	rsvg-convert -w 3700 $(ROOT)/out/universe-pyramid-$(VALUES).svg \
	  -o $(ROOT)/out/universe-pyramid-$(VALUES).png

clean:
	rm -rf $(ROOT)/out

distclean: clean
	rm -rf $(ROOT)/shapes $(ROOT)/proof $(ROOT)/hands.json
