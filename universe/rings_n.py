#!/usr/bin/env python3
"""Ring packing for rosettes of more than five copies.

deck.ring_table() only packs one through five, because that is the deck. The
packer itself is general, so exploring a seven-value deck needs nothing more
than running it further -- into its own cache, so the real deck's rings.json
is left exactly as it is.
"""
import json
import pathlib
import sys

HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE))
import deck

CACHE = HERE / "shapes" / "rings-extended.json"


def ring_table(max_n=7, rebuild=False):
    """The real table, extended to `max_n` copies."""
    table = dict(deck.ring_table())
    if CACHE.exists() and not rebuild:
        table.update(json.loads(CACHE.read_text()))
    missing = [(s, n) for s in deck.SHAPES for n in range(1, max_n + 1)
               if f"{s}:{n}" not in table]
    if missing:
        extra = {} if not CACHE.exists() else json.loads(CACHE.read_text())
        for s, n in missing:
            print(f"    packing {n} x {s} ...", flush=True)
            extra[f"{s}:{n}"] = round(deck.pack_ring(s, n), 5)
        CACHE.write_text(json.dumps(extra, indent=2, sort_keys=True) + "\n")
        table.update(extra)
    return table


if __name__ == "__main__":
    t = ring_table(int(sys.argv[1]) if len(sys.argv) > 1 else 7)
    for s in deck.SHAPES:
        print(f"  {s:8s} " + "  ".join(f"{n}:{t[f'{s}:{n}']:.4f}"
                                       for n in range(1, 8) if f"{s}:{n}" in t))
