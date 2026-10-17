#!/usr/bin/env python3
"""Tile the 64 cards into rings for the jigsaw back.

  python jigsaw_tiling.py            three distinctions vary per ring: 8 rings of 8
  python jigsaw_tiling.py --vary 2   two vary: 16 rings of 4

Every ring is a group of cards that agree on all but `vary` distinctions and
run through every combination of those -- a subcube of the six-bit deck -- and
the groups together use each card exactly once.  On the back each card gives
the one slice of its own face that lies in its ring and sector, so the groups
have to read as different *there*: an eye is invisible past the eye's tip,
and out in the corners a card is almost all bare field.

The centre group is fixed: circle, no eye, no rays, with background,
foreground and inversion varying, so the middle disc shows all four colors
(plain cards show their foreground, inverted ones their background).  With
--vary 2 it is bar, black, red, no rays, varying eye and inversion, the one
group of four whose centres are all four colors.

The search fills the most constrained rings first and prefers, in order,
groups whose slices differ most at their ring and pair/triple choices not used
yet.  The result is written to jigsaw-<vary>.json, which make_cards.py reads.
"""
import argparse
import itertools
import json
import math
import pathlib
import random
import tempfile

import numpy as np
from PIL import Image

import make_cards as mc

HERE = pathlib.Path(__file__).parent
BIT_OF = {k: bit for k, bit, _ in mc.BITS}         # axis -> bit
CENTRE = {3: (["bg", "fg", "inv"], 0b000000),       # circle, no eye, no rays
          2: (["eye", "inv"], 0b001000)}           # black, red, bar, no rays


def subcubes(vary):
    """Every group of 2**vary cards agreeing on the other bits."""
    out = []
    for axes in itertools.combinations([k for k, _, _ in mc.BITS], vary):
        mask = sum(1 << BIT_OF[a] for a in axes)
        for base in range(64):
            if base & mask:
                continue
            cards = sorted(base | sum(1 << BIT_OF[a] for a, on in zip(axes, combo) if on)
                           for combo in itertools.product([0, 1], repeat=vary))
            out.append((tuple(axes), base, tuple(cards)))
    return out


def ring_masks(rings, w, h):
    yy, xx = np.mgrid[0:h, 0:w]
    rr = np.hypot(xx + .5 - w / 2, yy + .5 - h / 2)
    t = math.hypot(w / 2, h / 2) / rings
    return [(rr >= r * t) & (rr < (r + 1) * t) for r in range(rings)]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--vary", type=int, default=3, choices=[2, 3])
    ap.add_argument("--budget", type=int, default=40000)
    a = ap.parse_args()
    rings = 64 >> a.vary

    spec = mc.PRESETS["mpc"]()
    w, h = spec.w // 4, spec.h // 4
    with tempfile.TemporaryDirectory() as tmp:
        imgs = []
        for n in range(64):
            p = pathlib.Path(tmp) / f"{n}.png"
            mc.rasterize(mc.face_svg(n, spec), w, h, p)
            imgs.append(np.asarray(Image.open(p).convert("RGB")).astype(int))
    masks = ring_masks(rings, w, h)

    groups = subcubes(a.vary)

    def score(cards, r):
        """The least different pair of slices, as a fraction of the ring."""
        px = [imgs[c][masks[r]] for c in cards]
        return min((np.abs(px[i] - px[j]).sum(1) > 60).mean()
                   for i, j in itertools.combinations(range(len(cards)), 2))

    sc = {r: {g: score(g[2], r) for g in groups} for r in range(rings)}
    # a ring takes groups that read clearly apart there, or -- where nothing
    # can, out in the corners -- anything within half of the ring's best
    valid = {}
    for r in range(rings):
        top = max(sc[r].values())
        need = min(0.04, 0.5 * top)
        valid[r] = [g for g in groups if sc[r][g] >= need]

    axes, base = CENTRE[a.vary]
    centre = next(g for g in groups if list(g[0]) == axes and g[1] == base)
    order = sorted(range(1, rings), key=lambda r: len(valid[r]))
    best = [None]

    def search(k, used, assign, budget):
        if budget[0] <= 0:
            return
        budget[0] -= 1
        if k == len(order):
            key = (sum(sc[r][g] for r, g in assign.items()),
                   len({g[0] for g in assign.values()}))
            if best[0] is None or key > best[0][0]:
                best[0] = (key, dict(assign))
            return
        r = order[k]
        kinds = {g[0] for g in assign.values()}
        opts = [g for g in valid[r] if not set(g[2]) & used]
        random.shuffle(opts)
        opts.sort(key=lambda g: (-round(sc[r][g], 2), g[0] in kinds))
        for g in opts:
            assign[r] = g
            search(k + 1, used | set(g[2]), assign, budget)
            del assign[r]
            if budget[0] <= 0:
                return

    for seed in range(20):
        random.seed(seed)
        search(0, set(centre[2]), {0: centre}, [a.budget])
    if best[0] is None:
        raise SystemExit("no tiling found")
    (total, kinds), assign = best[0]
    tiling = [{"ring": r, "vary": list(assign[r][0]), "base": assign[r][1],
               "cards": list(assign[r][2]), "distinct": round(sc[r][assign[r]], 4)}
              for r in range(rings)]
    assert sorted(c for t in tiling for c in t["cards"]) == list(range(64))
    dst = HERE / f"jigsaw-{a.vary}.json"
    dst.write_text(json.dumps(tiling, indent=1))
    for t in tiling:
        print(f"  ring {t['ring']:2d}  vary {'/'.join(t['vary']):14s}  "
              f"fixed {mc.card_name(t['base'])[:40]:40s}  distinct {t['distinct']:.3f}")
    print(f"  {rings} rings, {kinds} different {'pairs' if a.vary == 2 else 'triples'}"
          f" -> {dst}")


if __name__ == "__main__":
    main()
