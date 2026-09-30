#!/usr/bin/env python3
"""Same as hand_size.py's greedy game, but the goal generalised: every card
must agree on `need` distinctions at once (need=1 is the current rule)."""
import random
import sys
from itertools import combinations
from statistics import median


def targets(need):
    return [(mask, val) for axes in combinations(range(6), need)
            for mask in [sum(1 << a for a in axes)]
            for val in range(64) if val & ~mask == 0]


def play(n, size, need, rng, cap=3000):
    T = targets(need)
    member = [[(c & m) == v for m, v in T] for c in range(64)]

    def counts(h):
        return [sum(member[c][i] for c in h) for i in range(len(T))]

    def score(h):
        return sum(x ** 4 for x in counts(h))

    def best_discard(h, keep):
        return max((c for c in h if c != keep), key=lambda c: score([x for x in h if x != c]))

    deck = list(range(64)); rng.shuffle(deck)
    hands = [deck[i*size:(i+1)*size] for i in range(n)]
    deck = deck[n*size:]; pile = [deck.pop()]
    seat, turns = 0, 0
    while turns < cap:
        turns += 1
        h, top = hands[seat], pile[-1]
        w = h + [top]
        taken = None
        if score([x for x in w if x != best_discard(w, top)]) > score(h):
            pile.pop(); h.append(top); taken = top
        else:
            if not deck:
                deck = pile[:-1]; rng.shuffle(deck); pile = pile[-1:]
            h.append(deck.pop())
        d = best_discard(h, taken); h.remove(d); pile.append(d)
        if max(counts(h)) == len(h):
            return turns, seat
        seat = (seat + 1) % n
    return turns, None


if __name__ == "__main__":
    need = int(sys.argv[1]) if len(sys.argv) > 1 else 2
    games = int(sys.argv[2]) if len(sys.argv) > 2 else 200
    rng = random.Random(2)
    print(f"need {need} shared distinctions, greedy play, {games} games per row")
    print("size  players  rounds median  p90  seat-1 wins  unfinished")
    for size in range(4, 11):
        for n in (2, 3, 4):
            if n * size + 2 > 64: continue
            rs = [play(n, size, need, rng) for _ in range(games)]
            rounds = sorted((t + n - 1) // n for t, _ in rs)
            print(f"{size:>4}  {n:>7}  {median(rounds):>13}  {rounds[int(.9*games)]:>4}"
                  f"  {sum(s == 0 for _, s in rs)/games:>11.0%}  {sum(s is None for _, s in rs)/games:>10.0%}",
                  flush=True)
