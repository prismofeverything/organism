#!/usr/bin/env python3
"""The hand chart for best-five-of-seven: which hands a seven-card player ends
up holding, how often, and what order they rank in.

The five-card chart is exact (hands.py enumerates every hand) and ranks by
rarity. Seven cards cannot simply borrow it, and cannot rank by how often each
hand is *kept* either: which five a player keeps depends on the ranking, and
chasing that round in a circle buries hands. A full house is common in seven
cards, so it sinks below two pair -- and every full house holds a two pair, so
nobody ever keeps it, it is never seen, and it stays sunk.

So the order is by how rarely seven cards *contain* each hand, whatever else
they hold. That cannot bury anything: if holding A always means holding B, A is
contained no more often than B and ranks above it, so A is still somebody's
best hand. Then, under that order, the chart counts how often each hand is what
a player actually keeps -- the frequency you would see at the table.

Seven cards cannot be enumerated here (C(60,7) is 386 million deals), so the
counts are sampled; `--trials` sets how many.

Two rows still cannot occur: seven cards over five numbers always hold two
pairs or better, so `triad` and `dyad` are never anyone's best hand. They are
left off the chart and listed as impossible.

Writes hands7.json in hands.json's shape, rarest first, so make_pyramid.py can
draw it (`make_pyramid.py --seven`).
"""
import argparse
import itertools
import json
import pathlib
import sys
import time

import numpy as np

HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE))
import hands_n
import holdem_n

V, N = 5, 7


def five_card_rows():
    """The five-card chart's rows, rarest first, each with a distinct rank.
    Its one exact tie (two rows at 240 hands) is broken by chart order, which
    only decides where the iteration starts, not where it ends."""
    rows = hands_n.chart(V)["rows"]
    return [(r["numbers"], r["suit"]) for r in rows]


def deal(trials, seed, chunk=40000):
    """Seven-card deals in chunks, each with the chart row of every one of its
    21 five-card subsets: (deals, subset rows) where rows index `keys`."""
    keys = five_card_rows()
    rank = {key: i for i, key in enumerate(keys)}
    ncards, colbit, shpbit, numctr = holdem_n.deck_arrays(V)
    subsets = np.array(list(itertools.combinations(range(N), 5)))
    rng = np.random.default_rng(seed)
    done = 0
    while done < trials:
        m = min(chunk, trials - done)
        idx = np.argpartition(rng.random((m, ncards)), N, axis=1)[:, :N]
        cm = np.bitwise_or.reduce(colbit[idx][:, subsets], 2)
        sm = np.bitwise_or.reduce(shpbit[idx][:, subsets], 2)
        nc = numctr[idx][:, subsets].sum(2)
        yield idx, holdem_n.classify(cm, sm, nc, V, rank, False)
        done += m


def containment(trials, seed):
    """How many deals hold each row somewhere among their 21 subsets."""
    keys = five_card_rows()
    held = np.zeros(len(keys), np.int64)
    for _, rows in deal(trials, seed):
        present = np.zeros((rows.shape[0], len(keys)), bool)
        present[np.arange(rows.shape[0])[:, None], rows] = True
        held += present.sum(0)
    return dict(zip(keys, held))


def kept(order, trials, seed):
    """Under `order` (earlier is better), how often each row is the best five,
    with one real example of it."""
    keys = five_card_rows()
    place = np.array([order.index(k) for k in keys])      # row -> place in order
    cards = [(c, s, n) for c in range(3) for s in range(4) for n in range(V)]
    subsets = np.array(list(itertools.combinations(range(N), 5)))
    tally = np.zeros(len(order), np.int64)
    examples = {}
    for idx, rows in deal(trials, seed):
        places = place[rows]
        which = places.argmin(axis=1)
        best = places[np.arange(rows.shape[0]), which]
        tally += np.bincount(best, minlength=len(order))
        for r in np.unique(best):
            key = order[r]
            if key not in examples:
                i = int(np.flatnonzero(best == r)[0])
                hand = idx[i][subsets[which[i]]]
                examples[key] = [list(cards[k]) for k in sorted(hand)]
    return dict(zip(order, tally)), examples


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--trials", type=int, default=4_000_000)
    ap.add_argument("--seed", type=int, default=7)
    args = ap.parse_args()

    t0 = time.time()
    print(f"how often seven cards hold each hand ({args.trials:,} deals)", flush=True)
    held = containment(args.trials, args.seed)
    order = sorted(held, key=lambda k: held[k])            # rarest held is best
    print(f"how often each is the hand kept ({args.trials:,} deals)", flush=True)
    counts, examples = kept(order, args.trials, args.seed + 1)

    rows, impossible = [], []
    for key in order:
        entry = {"numbers": key[0], "suit": key[1], "held": int(held[key])}
        if counts[key] == 0:
            impossible.append(entry)
        else:
            rows.append(dict(entry, count=int(counts[key]), example=examples[key]))
    inversions = sum(1 for i, a in enumerate(rows) for b in rows[i + 1:]
                     if a["count"] > b["count"])
    out = {"cards": N, "total": args.trials, "sampled": True, "order": "held",
           "inversions": inversions, "rows": rows, "impossible": impossible}
    (HERE / "hands7.json").write_text(json.dumps(out, indent=1))

    print(f"\n  {'#':>2}  {'hand':<24} {'held':>12} {'kept':>12}")
    for i, r in enumerate(rows, 1):
        print(f"  {i:2d}  {r['suit'] + ' ' + r['numbers']:<24} "
              f"1 in {args.trials / r['held']:>7,.1f} 1 in {args.trials / r['count']:>7,.1f}")
    for r in impossible:
        print(f"   -  {r['suit'] + ' ' + r['numbers']:<24} "
              f"1 in {args.trials / r['held']:>7,.1f}       never")
    print(f"\n{inversions} inversions in what is kept; wrote hands7.json in {time.time() - t0:.0f}s")


if __name__ == "__main__":
    main()
