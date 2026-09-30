#!/usr/bin/env python3
"""How many cards makes DISTINCTIONS interesting?

Exact odds that a dealt hand is already made, then simulated games at each
hand size under two strategies:

  greedy  -- the SORTER bot (src/cljc/distinctions/bot.cljc): take the pile's
             card only when it improves the hand, discard the card whose loss
             hurts least.  A decent player, not a perfect one.
  random  -- take the pile half the time, discard any card at random.  The
             floor: how long the game lasts when nobody is trying.

A real table sits between the two.  Each game takes about a millisecond."""
import random
import sys
from math import comb
from statistics import median

TARGETS = [(b, v) for b in range(6) for v in (0, 1)]


def counts(hand):
    return [sum(((c >> b) & 1) == v for c in hand) for b, v in TARGETS]


def score(hand):
    return sum(n ** 4 for n in counts(hand))


def made(hand):
    return any(n == len(hand) for n in counts(hand))


def best_discard(hand, keep):
    return max((c for c in hand if c != keep),
               key=lambda c: score([x for x in hand if x != c]))


def play(n, size, rng, strategy, cap=3000):
    deck = list(range(64))
    rng.shuffle(deck)
    hands = [deck[i * size:(i + 1) * size] for i in range(n)]
    deck = deck[n * size:]
    pile = [deck.pop()]
    seat, turns = 0, 0
    while turns < cap:
        turns += 1
        h, top = hands[seat], pile[-1]
        if strategy == "greedy":
            w = h + [top]
            take = score([x for x in w if x != best_discard(w, top)]) > score(h)
        else:
            take = rng.random() < 0.5
        taken = None
        if take:
            pile.pop()
            h.append(top)
            taken = top
        else:
            if not deck:
                deck = pile[:-1]
                rng.shuffle(deck)
                pile = pile[-1:]
            h.append(deck.pop())
        if strategy == "greedy":
            d = best_discard(h, taken)
        else:
            d = rng.choice([c for c in h if c != taken])
        h.remove(d)
        pile.append(d)
        if made(h):
            return turns, seat
        seat = (seat + 1) % n
    return turns, None


def main(games):
    rng = random.Random(1)
    print(f"{games} games per row.  'rounds' = turns each player got.\n")
    print("size  dealt made   players | greedy: median  p90  seat-1 wins | random: median  p90  unfinished")
    for size in range(6, 17):
        p = 12 * comb(32, size) / comb(64, size)
        for n in (2, 3, 4):
            if n * size + 2 > 64:
                continue
            row = f"{size:>4}  {p:>10.2%}   {n:>7} |"
            for strategy in ("greedy", "random"):
                rs = [play(n, size, rng, strategy) for _ in range(games)]
                rounds = sorted((t + n - 1) // n for t, _ in rs)
                med, p90 = median(rounds), rounds[int(.9 * games)]
                if strategy == "greedy":
                    first = sum(s == 0 for _, s in rs) / games
                    row += f" {med:>14} {p90:>4} {first:>12.0%} |"
                else:
                    stuck = sum(s is None for _, s in rs) / games
                    row += f" {med:>14} {p90:>4} {stuck:>11.0%}"
            print(row, flush=True)


if __name__ == "__main__":
    main(int(sys.argv[1]) if len(sys.argv) > 1 else 400)
