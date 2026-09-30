#!/usr/bin/env python3
"""Play the 'cover all six' rule: 8 cards, draw-and-discard, win when the
hand holds a pattern from the table.  A table maps the size of one side of
the split (3, 4 or 5 distinctions) to the team sizes (m1 on that side, m2 on
the other).  Greedy bot: minimise cards-still-missing, tie-break by how many
ways it is that close."""
import random
import sys
from itertools import combinations
from statistics import median

AX = range(6)
SPLITS = {k: [(s, tuple(a for a in AX if a not in s)) for s in combinations(AX, k)]
          for k in (3, 4, 5)}


def key(c, axes):
    return tuple((c >> a) & 1 for a in axes)


def buckets(cards, axes):
    b = {}
    for c in cards:
        b.setdefault(key(c, axes), []).append(c)
    return b.values()


def agree(cards, axes):
    return len({key(c, axes) for c in cards}) == 1


def won(hand, table):
    for k, (m1, m2) in table.items():
        for s, t in SPLITS[k]:
            for team in combinations(hand, m1):
                if agree(team, s):
                    rest = [c for c in hand if c not in team]
                    if any(len(b) >= m2 for b in buckets(rest, t)):
                        return True
    return False


def closeness(hand, table):
    best, ways = 99, 0
    for k, (m1, m2) in table.items():
        for s, t in SPLITS[k]:
            for b1 in buckets(hand, s):
                team = b1[:m1]
                rest = [c for c in hand if c not in team]
                b2 = max((len(b) for b in buckets(rest, t)), default=0)
                miss = max(0, m1 - len(b1)) + max(0, m2 - b2)
                if miss < best:
                    best, ways = miss, 1
                elif miss == best:
                    ways += 1
    return -best * 1000 + ways


def best_discard(hand, keep, table):
    return max((c for c in hand if c != keep),
               key=lambda c: closeness([x for x in hand if x != c], table))


def play(n, table, rng, strategy="greedy", size=8, cap=1500):
    deck = list(range(64)); rng.shuffle(deck)
    hands = [deck[i*size:(i+1)*size] for i in range(n)]
    deck = deck[n*size:]; pile = [deck.pop()]
    seat, turns = 0, 0
    while turns < cap:
        turns += 1
        h, top = hands[seat], pile[-1]
        if strategy == "greedy":
            w = h + [top]
            take = closeness([x for x in w if x != best_discard(w, top, table)], table) \
                > closeness(h, table)
        else:
            take = rng.random() < 0.5
        taken = None
        if take:
            pile.pop(); h.append(top); taken = top
        else:
            if not deck:
                deck = pile[:-1]; rng.shuffle(deck); pile = pile[-1:]
            h.append(deck.pop())
        d = best_discard(h, taken, table) if strategy == "greedy" \
            else rng.choice([c for c in h if c != taken])
        h.remove(d); pile.append(d)
        if won(h, table):
            return turns, seat
        seat = (seat + 1) % n
    return turns, None


TABLES = {
    "as proposed  3/3:4+4  4/2:3+3  5/1:2+2": {3: (4, 4), 4: (3, 3), 5: (2, 2)},
    "cube-ish only 3/3:4+4":                  {3: (4, 4)},
    "balanced A   3/3:3+4  4/2:4+2  5/1:2+6": {3: (3, 4), 4: (4, 2), 5: (2, 6)},
    "balanced B   3/3:3+4  4/2:3+4  5/1:2+6": {3: (3, 4), 4: (3, 4), 5: (2, 6)},
}

if __name__ == "__main__":
    games = int(sys.argv[1]) if len(sys.argv) > 1 else 150
    rng = random.Random(5)
    for name, table in TABLES.items():
        for n in (2, 3, 4):
            for strat in ("greedy", "random"):
                rs = [play(n, table, rng, strat) for _ in range(games)]
                rounds = sorted((t + n - 1) // n for t, _ in rs)
                stuck = sum(s is None for _, s in rs) / games
                first = sum(s == 0 for _, s in rs) / games
                print(f"{name}  {n}p  {strat:>6}: median {median(rounds):>5}  p90 {rounds[int(.9*games)]:>4}"
                      f"  seat-1 {first:>4.0%}  unfinished {stuck:.0%}", flush=True)
