#!/usr/bin/env python3
"""Which patterns should a UNIVERSE shedding game offer?

For every candidate pattern, at every table size (2-6, the deck split evenly):

  lead    how often a hand holds at least one, so its owner could lead it
  beat    after the leader leads their weakest one, how often somebody else
          can play a higher one of the same kind
  beat#   the same, ranking by number only (no colour tie-break)

A card ranks by number, then colour (purple > green > yellow), as hold'em
breaks ties; shape never ranks.  A pattern ranks by its top card.

  set k        k cards of one number
  set k/c      ... all one colour     (at most 4: the four shapes)
  set k/s      ... all one shape      (at most 3: the three colours)
  run L        L consecutive numbers, any colours and shapes
  run L/c      ... all one colour
  run L/s      ... all one shape
  pairs L      L consecutive numbers, two of each
"""
import random
from collections import defaultdict
from itertools import combinations

N_COL, N_SHP = 3, 4
CARDS = [(n, c, s) for n in range(1, 6) for c in range(N_COL) for s in range(N_SHP)]  # c: 0 yellow 1 green 2 purple


def rank(card, colour=True):
    n, c, _ = card
    return n * 3 + c if colour else n * 3


# ---- the best / weakest instance of each kind in a hand, as a rank ---------

def set_groups(hand, flavour):
    g = defaultdict(list)
    for card in hand:
        n, c, s = card
        key = (n,) if flavour == "" else (n, c) if flavour == "c" else (n, s)
        g[key].append(card)
    return g.values()


def run_tops(hand, L, flavour, colour):
    """(weakest, strongest) rank of any L-run in the hand, or None."""
    keys = [None] if flavour == "" else range(N_COL) if flavour == "c" else range(N_SHP)
    weakest, strongest = None, None
    for k in keys:
        cards = [x for x in hand if flavour == "" or (x[1] if flavour == "c" else x[2]) == k]
        nums = {x[0] for x in cards}
        for start in range(1, 7 - L):
            if all(start + i in nums for i in range(L)):
                top = [rank(x, colour) for x in cards if x[0] == start + L - 1]
                weakest = min(min(top), weakest if weakest is not None else 99)
                strongest = max(max(top), strongest if strongest is not None else -1)
    return None if weakest is None else (weakest, strongest)


def kinds():
    out = []
    for k in (1, 2, 3, 4):
        out.append((f"set {k}", "set", k, ""))
    for k in (2, 3, 4):
        out.append((f"set {k}/c", "set", k, "c"))
    for k in (2, 3):
        out.append((f"set {k}/s", "set", k, "s"))
    for L in (3, 4, 5):
        for f in ("", "c", "s"):
            out.append((f"run {L}" + (f"/{f}" if f else ""), "run", L, f))
    for L in (2, 3):
        out.append((f"pairs {L}", "pairs", L, ""))
    return out


def instance(hand, kind, colour):
    """(weakest, strongest) rank of this kind of pattern in the hand."""
    _, form, k, f = kind
    if form == "set":
        weak, strong = None, None
        for grp in set_groups(hand, f):
            if len(grp) >= k:
                rs = sorted(rank(x, colour) for x in grp)
                weak = min(rs[k - 1], weak if weak is not None else 99)
                strong = max(rs[-1], strong if strong is not None else -1)
        return None if weak is None else (weak, strong)
    if form == "run":
        return run_tops(hand, k, f, colour)
    # pairs: L consecutive numbers with two each
    by = defaultdict(list)
    for x in hand:
        by[x[0]].append(rank(x, colour))
    weak, strong = None, None
    for start in range(1, 7 - k):
        if all(len(by[start + i]) >= 2 for i in range(k)):
            top = sorted(by[start + k - 1])
            weak = min(top[1], weak if weak is not None else 99)
            strong = max(top[-1], strong if strong is not None else -1)
    return None if weak is None else (weak, strong)


def survey(deals=4000, seed=1):
    rng = random.Random(seed)
    ks = kinds()
    print(f"{deals} deals per table size.  lead = hand can lead it;  beat = someone can top the leader's"
          " weakest one (colour breaks ties);  beat# = ranking by number alone\n")
    header = f"{'pattern':10}" + "".join(f" | {n}p lead  beat beat#" for n in range(2, 7))
    print(header)
    stats = {(k[0], n): [0, 0, 0, 0] for k in ks for n in range(2, 7)}
    for n in range(2, 7):
        size = 60 // n
        for _ in range(deals):
            deck = CARDS[:]
            rng.shuffle(deck)
            hands = [deck[i * size:(i + 1) * size] for i in range(n)]
            for kind in ks:
                st = stats[(kind[0], n)]
                lead = instance(hands[0], kind, True)
                if lead is None:
                    continue
                st[0] += 1
                others = [instance(h, kind, True) for h in hands[1:]]
                st[1] += any(o and o[1] > lead[0] for o in others)
                lead_n = instance(hands[0], kind, False)
                others_n = [instance(h, kind, False) for h in hands[1:]]
                st[2] += any(o and o[1] > lead_n[0] for o in others_n)
    for kind in ks:
        row = f"{kind[0]:10}"
        for n in range(2, 7):
            led, beat, beatn, _ = stats[(kind[0], n)]
            row += f" | {led/deals:7.0%} {beat/max(led,1):5.0%} {beatn/max(led,1):5.0%}"
        print(row, flush=True)


if __name__ == "__main__":
    survey()
