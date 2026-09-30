#!/usr/bin/env python3
"""For each split of the six distinctions (k on one side, 6-k on the other)
and each pair of team sizes (m1 agreeing on the k side, m2 on the other),
how often a random 8-card hand already holds it.  To pick sizes that make
every split about equally hard."""
import random
from itertools import combinations
from cover import agree, AX

def holds(hand, side, m1, m2):
    other = [a for a in AX if a not in side]
    for team in combinations(hand, m1):
        if agree(team, side):
            rest = [c for c in hand if c not in team]
            if any(agree(t, other) for t in combinations(rest, m2)):
                return True
    return False

rng = random.Random(4)
N = 6000
hands = [rng.sample(range(64), 8) for _ in range(N)]
print("split   teams   dealt")
for k in (3, 4, 5):
    for m1 in range(2, 6):
        for m2 in range(2, 9 - m1):
            if k == 3 and m2 < m1: continue
            p = sum(any(holds(h, s, m1, m2) for s in combinations(AX, k)) for h in hands) / N
            if 0.0 < p < 0.6:
                print(f"{k}/{6-k}    {m1}+{m2}    {p:.2%}", flush=True)
