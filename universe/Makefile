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

.PHONY: all shapes faces back sheets proof palette chart lattice space pyramid key clean distclean

all: faces back sheets proof palette chart lattice space pyramid key

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

key: $(ROOT)/shapes/paths.json $(RINGS) make_key.py make_pyramid.py
	$(PY) make_key.py
	rsvg-convert -w 3200 $(ROOT)/out/universe-key.svg -o $(ROOT)/out/universe-key.png

clean:
	rm -rf $(ROOT)/out

distclean: clean
	rm -rf $(ROOT)/shapes $(ROOT)/proof $(ROOT)/hands.json
