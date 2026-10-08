#!/usr/bin/env python3
"""shed_hands.py again, with menus where multi-card patterns must also share
a colour or a shape -- the extra dimension doing the work."""
import random
import sys
from collections import Counter, defaultdict
from itertools import combinations, product
from statistics import mean

from shed_patterns import CARDS
import shed_hands


def patterns(hand, set_max, runs, pairs, set_flavour=False, run_flavour=False):
    idx = {c: i for i, c in enumerate(hand)}
    out = set()
    groups = [lambda c: True]
    if set_flavour or run_flavour:
        flav = [lambda c, k=k: c[1] == k for k in range(3)] + [lambda c, k=k: c[2] == k for k in range(4)]
    def by_number(pred):
        by = defaultdict(list)
        for c in hand:
            if pred(c):
                by[c[0]].append(idx[c])
        return by
    for pred in (flav if set_flavour else groups):
        by = by_number(pred)
        for n, cs in by.items():
            for k in range(1, min(set_max, len(cs)) + 1):
                for sub in combinations(cs, k):
                    out.add(sum(1 << i for i in sub))
    for pred in (flav if run_flavour else groups):
        by = by_number(pred)
        for L in runs:
            for start in range(1, 7 - L):
                if all(by[start + i] for i in range(L)):
                    for pick in product(*(by[start + i] for i in range(L))):
                        out.add(sum(1 << i for i in pick))
    if pairs:
        by = by_number(lambda c: True)
        for L in range(2, 6):
            for start in range(1, 7 - L):
                if all(len(by[start + i]) >= 2 for i in range(L)):
                    for pick in product(*(combinations(by[start + i], 2) for i in range(L))):
                        out.add(sum(1 << i for p in pick for i in p))
    return out


MENUS = {
    "sets<=4, runs 3-5 share col/shape":          dict(set_max=4, runs=(3, 4, 5), pairs=False, run_flavour=True),
    "sets & runs 3-5 share col/shape":            dict(set_max=4, runs=(3, 4, 5), pairs=False, set_flavour=True, run_flavour=True),
    "sets & runs 3-5 share col/shape, pair-runs": dict(set_max=4, runs=(3, 4, 5), pairs=True, set_flavour=True, run_flavour=True),
}

if __name__ == "__main__":
    shed_hands.patterns = patterns          # min_plays looks it up by name
    deals = int(sys.argv[1]) if len(sys.argv) > 1 else 300
    rng = random.Random(2)                  # the same hands as shed_hands.py
    for n in (6, 5, 4):
        size = 60 // n
        hands = [rng.sample(CARDS, size) for _ in range(deals)]
        print(f"\n{n} players, {size} cards each ({deals} hands)")
        for name, menu in MENUS.items():
            plays = [shed_hands.min_plays(h, menu) for h in hands]
            dist = Counter(plays)
            spread = "  ".join(f"{k}:{dist[k]/deals:.0%}" for k in sorted(dist))
            print(f"  {name:43} mean {mean(plays):4.1f}   [{spread}]", flush=True)
