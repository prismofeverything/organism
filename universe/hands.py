#!/usr/bin/env python3
"""Exact hand frequencies for the UNIVERSE deck.

A card is (colour, shape, number) over 3 x 4 x 5 = 60.  Splitting the suit in
two gives three kinds of flush instead of one, and because there are only five
numbers, any hand without a repeat is automatically the full run 1-2-3-4-5.

Every one of the C(60,5) = 5,461,512 hands is enumerated and classified, so
the counts here are exact rather than argued.
"""
import itertools
import json
import pathlib
import random
from collections import Counter

HERE = pathlib.Path(__file__).parent
import deck as _deck

COLOURS = _deck.COLOUR_ORDER          # index order, shared with the renderer
SHAPES = _deck.SHAPES
NUMBERS = _deck.NUMBERS

DECK = [(c, s, n) for c in range(3) for s in range(4) for n in range(5)]

# number pattern, by the sorted multiplicities of the five numbers
SHAPE_OF = {
    (5,): "five of a kind",
    (4, 1): "four of a kind",
    (3, 2): "full house",
    (3, 1, 1): "three of a kind",
    (2, 2, 1): "two pair",
    (2, 1, 1, 1): "one pair",
    (1, 1, 1, 1, 1): "straight",      # only five numbers exist, so this is 1-2-3-4-5
}


def classify(hand):
    """(number pattern, suit pattern) for one hand of five card indices."""
    cols = 0
    shps = 0
    tally = [0] * 5
    for i in hand:
        c, s, n = DECK[i]
        cols |= 1 << c
        shps |= 1 << s
        tally[n] += 1
    one_colour = cols in (1, 2, 4)
    one_shape = shps in (1, 2, 4, 8)
    if one_colour and one_shape:
        suit = "perfect"
    elif one_colour:
        suit = "colour"
    elif one_shape:
        suit = "shape"
    else:
        suit = "mixed"
    mult = tuple(sorted((t for t in tally if t), reverse=True))
    return SHAPE_OF[mult], suit


def main():
    counts = Counter()
    examples = {}
    rng = random.Random(20260921)
    seen = Counter()
    total = 0
    for hand in itertools.combinations(range(60), 5):
        key = classify(hand)
        counts[key] += 1
        total += 1
        # reservoir of one, so the example does not depend on enumeration order
        seen[key] += 1
        if rng.random() < 1.0 / seen[key]:
            examples[key] = hand

    assert total == 5461512, total
    assert sum(counts.values()) == total
    out = {
        "total": total,
        "rows": [
            {
                "numbers": k[0],
                "suit": k[1],
                "count": v,
                "example": [list(DECK[i]) for i in examples[k]],
            }
            for k, v in sorted(counts.items(), key=lambda kv: kv[1])
        ],
    }
    (HERE / "hands.json").write_text(json.dumps(out, indent=1) + "\n")

    print(f"{'numbers':18s} {'suit':8s} {'count':>10s} {'percent':>9s}  {'1 in':>10s}")
    for r in out["rows"]:
        print(f"{r['numbers']:18s} {r['suit']:8s} {r['count']:10,d} "
              f"{100*r['count']/total:8.4f}% {total/r['count']:10,.0f}")
    print(f"{'':27s} {total:10,d}")


if __name__ == "__main__":
    main()


# --------------------------------------------------------------- comparison

def standard_deck():
    """Enumerate a normal 52-card deck the same way, so the comparison with
    UNIVERSE is measured rather than recalled."""
    cards = [(suit, rank) for suit in range(4) for rank in range(13)]
    counts = Counter()
    for hand in itertools.combinations(range(52), 5):
        suits = {cards[i][0] for i in hand}
        ranks = sorted(cards[i][1] for i in hand)
        tally = sorted(Counter(ranks).values(), reverse=True)
        flush = len(suits) == 1
        lo = ranks[0]
        straight = (tally[0] == 1 and (ranks == list(range(lo, lo + 5))
                                       or ranks == [0, 1, 2, 3, 12]))
        if straight and flush:
            k = "straight flush"
        elif tally == [4, 1]:
            k = "four of a kind"
        elif tally == [3, 2]:
            k = "full house"
        elif flush:
            k = "flush"
        elif straight:
            k = "straight"
        elif tally == [3, 1, 1]:
            k = "three of a kind"
        elif tally == [2, 2, 1]:
            k = "two pair"
        elif tally == [2, 1, 1, 1]:
            k = "one pair"
        else:
            k = "high card"
        counts[k] += 1
    return counts
