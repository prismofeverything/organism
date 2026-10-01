#!/usr/bin/env python3
"""The path goal: seven cards running from a card to its opposite, one
distinction changing at each step.  A set of cards lies on one such path
exactly when, measured from some anchor card a, their differences from a
are nested (each contains the last).  Greedy bot: longest chain it holds,
tie-break by how many anchors give it."""
import random
import sys
from statistics import median

POP = [bin(i).count("1") for i in range(64)]


def chain_len(masks):
    """Longest chain under inclusion, strictly growing."""
    ms = sorted(set(masks), key=lambda m: POP[m])
    best = {}
    for m in ms:
        best[m] = 1 + max((best[p] for p in best if p != m and (p & m) == p and POP[p] < POP[m]), default=0)
    return max(best.values(), default=0)


def closeness(hand):
    top, ways = 0, 0
    for a in range(32):           # a and its opposite give the same chains
        L = chain_len([c ^ a for c in hand])
        if L > top:
            top, ways = L, 1
        elif L == top:
            ways += 1
    return top * 1000 + ways


def won(hand):
    s = set(hand)
    for c in hand:
        if (c ^ 63) in s and chain_len([x ^ c for x in hand]) == 7:
            return True
    return False


def best_discard(hand, keep):
    return max((c for c in hand if c != keep), key=lambda c: closeness([x for x in hand if x != c]))


def play(n, rng, cap=3000, size=7):
    deck = list(range(64)); rng.shuffle(deck)
    hands = [deck[i*size:(i+1)*size] for i in range(n)]
    deck = deck[n*size:]; pile = [deck.pop()]
    seat, turns = 0, 0
    while turns < cap:
        turns += 1
        h, top = hands[seat], pile[-1]
        w = h + [top]
        taken = None
        if closeness([x for x in w if x != best_discard(w, top)]) > closeness(h):
            pile.pop(); h.append(top); taken = top
        else:
            if not deck:
                deck = pile[:-1]; rng.shuffle(deck); pile = pile[-1:]
            h.append(deck.pop())
        d = best_discard(h, taken); h.remove(d); pile.append(d)
        if won(h):
            return turns, seat
        if n == 1:                       # solo: the deck turns a fresh card
            if not deck:
                deck = pile[:-1]; rng.shuffle(deck); pile = pile[-1:]
            pile.append(deck.pop())
        seat = (seat + 1) % n
    return turns, None


if __name__ == "__main__":
    games = int(sys.argv[1]) if len(sys.argv) > 1 else 150
    rng = random.Random(8)
    for n in (1, 2, 3, 4):
        rs = [play(n, rng) for _ in range(games)]
        r = sorted((t + n - 1) // n for t, _ in rs)
        print(f"{n}p  rounds median {median(r):>5}  p90 {r[int(.9*games)]:>4}  "
              f"seat-1 {sum(s == 0 for _, s in rs)/games:.0%}  unfinished {sum(s is None for _, s in rs)/games:.0%}",
              flush=True)
