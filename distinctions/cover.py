#!/usr/bin/env python3
"""The 'cover all six' rule: split the six distinctions into two sides, and
find a team of cards agreeing on each side, the two teams sharing no card.

  3/3 split -> two teams of 4     4/2 split -> two teams of 3
  5/1 split -> two teams of 2

How often does a random 8-card hand already hold each pattern?"""
import random
from itertools import combinations

AX = range(6)

def agree(cards, axes):
    return all(len({(c >> a) & 1 for c in cards}) == 1 for a in axes)

def holds(hand, side, m):
    other = [a for a in AX if a not in side]
    for team in combinations(hand, m):
        if agree(team, side):
            rest = [c for c in hand if c not in team]
            if any(agree(t, other) for t in combinations(rest, m)):
                return True
    return False

PATTERNS = {"3/3 (4+4)": (3, 4), "4/2 (3+3)": (4, 3), "5/1 (2+2)": (5, 2)}

def which(hand):
    out = {}
    for name, (k, m) in PATTERNS.items():
        out[name] = any(holds(hand, side, m) for side in combinations(AX, k))
    return out

def main():
    rng = random.Random(3)
    N = 20000
    tally = {n: 0 for n in PATTERNS}
    anyp = 0
    for _ in range(N):
        h = rng.sample(range(64), 8)
        w = which(h)
        for n in w: tally[n] += w[n]
        anyp += any(w.values())
    for n, t in tally.items():
        print(f"{n}:  {t/N:.2%} of dealt 8-card hands")
    print(f"any of them: {anyp/N:.2%}")


if __name__ == "__main__":
    main()
