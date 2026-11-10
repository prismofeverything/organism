# The playtest table's component library: every game's printed and modelled
# parts, shrunk for the web.  See build_library.py.
#
#   make          -> ../resources/public/table/library.json + assets/
#   make clean    -> drop the generated assets
#
# Rebuild after the sources change: `make tts` in pieces/, `make faces back`
# in universe/ or distinctions/.

PY ?= ../.venv/bin/python

.PHONY: library clean
library:
	$(PY) build_library.py
clean:
	rm -rf ../resources/public/table/assets ../resources/public/table/library.json
