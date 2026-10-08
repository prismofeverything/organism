#!/usr/bin/env python3
"""How many plays does it take to empty a hand, under different menus of
patterns?  Exact (a minimum partition of the hand into allowed patterns),
for the table sizes whose hands are small enough to search: 4p (15 cards),
5p (12), 6p (10).

A good shedding game wants a spread: some hands that fall into a few big
patterns, some with awkward leftovers."""
import random
import sys
from collections import Counter, defaultdict
from functools import lru_cache
from itertools import combinations, product
from statistics import mean

from shed_patterns import CARDS

MENUS = {
    "sets<=4, runs of 5":            dict(set_max=4, runs=(5,), pairs=False),
    "sets<=4, runs 3-5":             dict(set_max=4, runs=(3, 4, 5), pairs=False),
    "sets<=4, runs 3-5, pair-runs":  dict(set_max=4, runs=(3, 4, 5), pairs=True),
    "sets any, runs 3-5, pair-runs": dict(set_max=12, runs=(3, 4, 5), pairs=True),
}


def patterns(hand, set_max, runs, pairs):
    idx = {c: i for i, c in enumerate(hand)}
    by = defaultdict(list)
    for c in hand:
        by[c[0]].append(idx[c])
    out = set()
    for n, cs in by.items():
        for k in range(1, min(set_max, len(cs)) + 1):
            for sub in combinations(cs, k):
                out.add(sum(1 << i for i in sub))
    for L in runs:
        for start in range(1, 7 - L):
            if all(by[start + i] for i in range(L)):
                for pick in product(*(by[start + i] for i in range(L))):
                    out.add(sum(1 << i for i in pick))
    if pairs:
        for L in range(2, 6):
            for start in range(1, 7 - L):
                if all(len(by[start + i]) >= 2 for i in range(L)):
                    for pick in product(*(combinations(by[start + i], 2) for i in range(L))):
                        out.add(sum(1 << i for p in pick for i in p))
    return out


def min_plays(hand, menu):
    pats = patterns(hand, **menu)
    @lru_cache(maxsize=None)
    def go(rest):
        if rest == 0:
            return 0
        low = (rest & -rest).bit_length() - 1
        # a pattern must contain the lowest card left, and nothing already gone
        best = 99
        for p in pats_containing[low]:
            if p & rest == p:
                best = min(best, 1 + go(rest & ~p))
        return best

    pats_containing = defaultdict(list)
    for p in pats:
        q = p
        while q:
            b = (q & -q).bit_length() - 1
            pats_containing[b].append(p)
            q &= q - 1
    return go((1 << len(hand)) - 1)


if __name__ == "__main__":
    deals = int(sys.argv[1]) if len(sys.argv) > 1 else 300
    rng = random.Random(2)
    for n in (6, 5, 4):
        size = 60 // n
        hands = [rng.sample(CARDS, size) for _ in range(deals)]
        print(f"\n{n} players, {size} cards each ({deals} hands)")
        for name, menu in MENUS.items():
            plays = [min_plays(h, menu) for h in hands]
            dist = Counter(plays)
            spread = "  ".join(f"{k}:{dist[k]/deals:.0%}" for k in sorted(dist))
            print(f"  {name:31} mean {mean(plays):4.1f}   one play {dist[1]/deals:.1%}   [{spread}]", flush=True)
