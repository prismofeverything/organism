#!/usr/bin/env python3
"""The hand chart for a UNIVERSE deck with any number of values.

hands.py enumerates the real deck by brute force. That does not scale: 3 x 4 x 7
is 84 cards and C(84,5) = 30,872,016 hands. So this counts them instead, and
checks the count against the brute-force answer for the deck we already know.

The counting rests on one observation. A hand's suit grade is the smallest
axis-aligned sub-block it fits in, so the number of hands of a given shape
*within a colour* is just the same calculation run on a 1 x 4 x V deck. Strict
grades come out by inclusion-exclusion:

    perfect        = 12 x count(1 x 1 x V)
    color (strict) =  3 x count(1 x 4 x V)  -  perfect
    shape (strict) =  4 x count(3 x 1 x V)  -  perfect
    mixed          =      count(3 x 4 x V)  -  the three above

`straight` is the one pattern that cares which numbers were chosen, not just
how many: at V = 5 every no-repeat hand is the whole run, which is why the real
deck has no high-card hand at all. Past five they come apart.
"""
import argparse
import json
import math
import pathlib

HERE = pathlib.Path(__file__).parent
C = math.comb

# number patterns as sorted group sizes; `run` marks the consecutive one
PATTERNS = {
    "five of a kind":  (5,),
    "four of a kind":  (4, 1),
    "full house":      (3, 2),
    "three of a kind": (3, 1, 1),
    "two pair":        (2, 2, 1),
    "one pair":        (2, 1, 1, 1),
    "straight":        (1, 1, 1, 1, 1),   # consecutive
    "high card":       (1, 1, 1, 1, 1),   # all distinct, not consecutive
}

GROUP_NAME = {"one pair": "dyad", "two pair": "split-tetrad",
              "three of a kind": "triad", "full house": "split-pentad",
              "four of a kind": "tetrad", "five of a kind": "pentad",
              "straight": "sequence", "high card": "scatter"}


def number_choices(pattern, values, consecutive=None):
    """How many ways to pick which numbers carry the groups."""
    sizes = PATTERNS[pattern]
    k = len(sizes)
    if k > values:
        return 0
    if consecutive is True:                 # a run of five consecutive numbers
        return max(0, values - 4)
    # distinct numbers for the k groups, then assign sizes to them; groups of
    # equal size are interchangeable
    per_size = {}
    for g in sizes:
        per_size[g] = per_size.get(g, 0) + 1
    arrangements = math.factorial(k)
    for m in per_size.values():
        arrangements //= math.factorial(m)
    total = C(values, k) * arrangements
    if consecutive is False:                # distinct but NOT a run
        return total - max(0, values - 4)
    return total


def count(pattern, colors, shapes, values):
    """Five-card hands of this pattern in a colors x shapes x values deck."""
    cells = colors * shapes
    sizes = PATTERNS[pattern]
    cons = True if pattern == "straight" else (False if pattern == "high card" else None)
    ways = number_choices(pattern, values, cons)
    if ways == 0:
        return 0
    for g in sizes:
        ways *= C(cells, g)
    return ways


def chart(values, colors=3, shapes=4, merge_high_card=False):
    """Every kind of hand in the deck, with its exact count.

    `merge_high_card` treats any no-repeat hand as a sequence, whether or not
    its numbers run consecutively -- the reading that keeps the deck's promise
    that no hand is nothing. At V = 5 the two readings are the same chart.
    """
    total = C(colors * shapes * values, 5)
    rows = []
    patterns = list(PATTERNS)
    if merge_high_card:
        patterns = [p for p in patterns if p != "high card"]
    for pattern in patterns:
        def n(c, s):
            if merge_high_card and pattern == "straight":
                # any five distinct numbers, run or not
                sizes = PATTERNS[pattern]
                w = C(values, 5)
                for g in sizes:
                    w *= C(c * s, g)
                return w
            return count(pattern, c, s, values)

        perfect = 12 * n(1, 1)
        color = colors * n(1, shapes) - perfect
        shape = shapes * n(colors, 1) - perfect
        mixed = n(colors, shapes) - color - shape - perfect
        for suit, v in (("perfect", perfect), ("color", color),
                        ("shape", shape), ("mixed", mixed)):
            if v > 0:
                if suit == "perfect" and pattern == "straight":
                    name = "singularity"
                elif suit == "perfect" and pattern == "high card":
                    name = "perfect-scatter"
                elif suit == "mixed":
                    name = GROUP_NAME[pattern]
                else:
                    name = f"{suit}-{GROUP_NAME[pattern]}"
                rows.append({"numbers": pattern, "suit": suit,
                             "count": v, "name": name})
    counted = sum(r["count"] for r in rows)
    assert counted == total, f"counted {counted}, deck holds {total}"
    rows.sort(key=lambda r: r["count"])
    rank, prev = -1, None
    for r in rows:
        if r["count"] != prev:
            rank += 1
            prev = r["count"]
        r["rank"] = rank
    return {"values": values, "cards": colors * shapes * values,
            "total": total, "rows": rows}


def show(ch, title):
    print(f"\n{title}: {ch['cards']} cards, {ch['total']:,} five-card hands, "
          f"{len(ch['rows'])} kinds\n")
    print(f"  {'rk':>2} {'hand':30s} {'count':>12s} {'share':>9s} {'1 in':>10s}")
    for r in ch["rows"]:
        print(f"  {r['rank']:2d} {r['name'].replace('-', ' '):30s} {r['count']:12,d} "
              f"{100*r['count']/ch['total']:8.4f}% {ch['total']/r['count']:10,.0f}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("values", nargs="*", type=int, default=[5, 7])
    ap.add_argument("--merge-high-card", action="store_true")
    ap.add_argument("--write", type=str, default=None)
    a = ap.parse_args()

    # the known deck is the test: these counts must match hands.json exactly
    known = json.loads((HERE / "hands.json").read_text())
    mine = {(r["numbers"], r["suit"]): r["count"] for r in chart(5)["rows"]}
    for row in known["rows"]:
        k = (row["numbers"], row["suit"])
        assert mine.get(k) == row["count"], f"{k}: {mine.get(k)} vs {row['count']}"
    print(f"  counting agrees with hands.py on all {len(known['rows'])} rows of the real deck")

    for v in a.values:
        ch = chart(v, merge_high_card=a.merge_high_card)
        show(ch, f"V = {v}")
        if a.write:
            (HERE / f"{a.write}{v}.json").write_text(json.dumps(ch, indent=1) + "\n")


# ── One real example of each row ───────────────────────────────────────────

def classify_one(hand, values):
    """(pattern, suit) for a hand of (color, shape, number) triples."""
    cols = {c for c, _, _ in hand}
    shps = {s for _, s, _ in hand}
    suit = ("perfect" if len(cols) == 1 and len(shps) == 1
            else "color" if len(cols) == 1
            else "shape" if len(shps) == 1 else "mixed")
    tally = {}
    for _, _, n in hand:
        tally[n] = tally.get(n, 0) + 1
    sizes = tuple(sorted(tally.values(), reverse=True))
    if sizes == (1, 1, 1, 1, 1):
        ns = sorted(tally)
        run = ns == list(range(ns[0], ns[0] + 5))
        return ("straight" if run else "high card"), suit
    for name, want in PATTERNS.items():
        if want == sizes and name not in ("straight", "high card"):
            return name, suit
    raise AssertionError(f"unclassifiable: {hand}")


def example(pattern, suit, values):
    """Build one hand of this kind, or None when there is no such hand.

    Constructed rather than searched: a singularity is 36 hands out of
    30,872,016, which random sampling will never find.
    """
    sizes = list(PATTERNS[pattern])
    # which numbers carry the groups
    if pattern == "straight":
        nums = list(range(5))
    elif pattern == "high card":
        if values < 6:
            return None
        nums = list(range(4)) + [5]            # a gap, so it is not a run
    else:
        nums = list(range(len(sizes)))
    if max(nums) >= values:
        return None
    # which cells each group may use
    if suit == "perfect":
        cells = [(0, 0)]
    elif suit == "color":
        cells = [(0, s) for s in range(4)]
    elif suit == "shape":
        cells = [(c, 0) for c in range(3)]
    else:
        cells = [(c, s) for c in range(3) for s in range(4)]
    hand = []
    for size, n in zip(sizes, nums):
        if size > len(cells):
            return None
        # mixed wants the groups spread so the hand really is mixed; taking
        # cells from alternating ends does that without any searching
        pick = cells[:size] if suit != "mixed" else (cells[:size] if n % 2 == 0
                                                     else cells[-size:])
        hand += [(c, s, n) for c, s in pick]
    if classify_one(hand, values) != (pattern, suit):
        # the simple pick landed in a stricter grade; nudge one card's cell
        for alt in cells:
            for i in range(len(hand)):
                trial = list(hand)
                c, s, n = trial[i]
                if (alt[0], alt[1], n) in trial:
                    continue
                trial[i] = (alt[0], alt[1], n)
                if len(set(trial)) == 5 and classify_one(trial, values) == (pattern, suit):
                    return trial
        return None
    return hand


def with_examples(values, **kw):
    ch = chart(values, **kw)
    for r in ch["rows"]:
        ex = example(r["numbers"], r["suit"], values)
        assert ex is not None, f"no example built for {r['suit']} {r['numbers']}"
        r["example"] = [list(t) for t in ex]
    return ch
