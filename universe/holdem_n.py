#!/usr/bin/env python3
"""Does hold'em work on a UNIVERSE deck with V values?

Best-five-of-seven broke the real deck because five numbers over sixty cards
saturate: seven cards always contain two pairs, so `triad` and `dyad` cannot be
made at all and the published order stops holding. Each number has twelve cards
whatever V is -- three colours by four shapes -- so what V changes is only how
many buckets there are to spread across. This measures what that buys.

An *inversion* is a pair where the better-ranked hand ends up the more common
one. Zero means the table is still playing the chart it prints.
"""
import argparse
import itertools
import math
import pathlib
import sys

import numpy as np

HERE = pathlib.Path(__file__).parent
sys.path.insert(0, str(HERE))
import hands_n


def deck_arrays(V):
    cards = [(c, s, n) for c in range(3) for s in range(4) for n in range(V)]
    colbit = np.array([1 << c for c, _, _ in cards], np.int64)
    shpbit = np.array([1 << s for _, s, _ in cards], np.int64)
    numctr = np.array([1 << (4 * n) for _, _, n in cards], np.int64)
    return len(cards), colbit, shpbit, numctr


def rank_table(V, merge):
    ch = hands_n.chart(V, merge_high_card=merge)
    rank = {(r["numbers"], r["suit"]): r["rank"] for r in ch["rows"]}
    label = {}
    for r in ch["rows"]:
        label.setdefault(r["rank"], []).append(r["name"].replace("-", " "))
    nranks = max(rank.values()) + 1
    exact = np.zeros(nranks)
    for r in ch["rows"]:
        exact[r["rank"]] += r["count"] / ch["total"]
    return rank, {k: " / ".join(v) for k, v in label.items()}, nranks, exact


SUITNAME = ["perfect", "color", "shape", "mixed"]


def classify(cm, sm, nc, V, rank, merge):
    """Rank index for hands given as colour/shape masks and a number counter."""
    oc = np.isin(cm, [1, 2, 4])
    os_ = np.isin(sm, [1, 2, 4, 8])
    suit = np.where(oc & os_, 0, np.where(oc, 1, np.where(os_, 2, 3)))
    cnt = np.stack([(nc >> (4 * k)) & 0xF for k in range(V)], axis=-1)
    srt = -np.sort(-cnt, axis=-1)
    pat = np.zeros(srt.shape[:-1], np.int64)
    for k in range(5):
        pat = pat * 10 + srt[..., k]
    # a run of five consecutive numbers, when that distinction is being kept
    if not merge:
        present = (cnt > 0).astype(np.int8)
        run = np.zeros(pat.shape, bool)
        for start in range(V - 4):
            run |= present[..., start:start + 5].all(-1) & (present.sum(-1) == 5)
    out = np.full(pat.shape, -1, np.int8)
    for name, sizes in hands_n.PATTERNS.items():
        if merge and name == "high card":
            continue
        pp = list(sizes) + [0] * (5 - len(sizes))
        key = 0
        for v in pp:
            key = key * 10 + v
        m = pat == key
        if name == "straight" and not merge:
            m = m & run
        if name == "high card":
            m = (pat == key) & ~run
        if not m.any():
            continue
        for si, sn in enumerate(SUITNAME):
            mm = m & (suit == si)
            if mm.any():
                if (name, sn) not in rank:
                    raise AssertionError(f"impossible hand: {sn} {name}")
                out[mm] = rank[(name, sn)]
    assert (out >= 0).all(), "a hand fell through the classifier"
    return out


def best_of(V, N, trials, rank, nranks, merge, seed=3, chunk=40000):
    ncards, colbit, shpbit, numctr = deck_arrays(V)
    subsets = np.array(list(itertools.combinations(range(N), 5)))
    rng = np.random.default_rng(seed)
    tally = np.zeros(nranks, np.int64)
    done = 0
    while done < trials:
        m = min(chunk, trials - done)
        idx = np.argpartition(rng.random((m, ncards)), N, axis=1)[:, :N]
        cm = np.bitwise_or.reduce(colbit[idx][:, subsets], 2)
        sm = np.bitwise_or.reduce(shpbit[idx][:, subsets], 2)
        nc = numctr[idx][:, subsets].sum(2)
        best = classify(cm, sm, nc, V, rank, merge).min(axis=1)
        tally += np.bincount(best, minlength=nranks)
        done += m
    return tally / done


def report(V, merge, trials):
    rank, label, nranks, exact = rank_table(V, merge)
    print(f"\n  V = {V}   ({12*V} cards, {nranks} distinct ranks, "
          f"{'any five distinct counts as a sequence' if merge else 'a sequence must run consecutively'})\n")
    print(f"  {'N':>2} {'live':>7} {'inversions':>11} {'entropy':>8}   floor (commonest hand that can exist)")
    dists = {}
    for N in (5, 6, 7, 8, 9):
        p = exact if N == 5 else best_of(V, N, trials, rank, nranks, merge)
        dists[N] = p
        live = int((p > 1e-9).sum())
        inv = sum(1 for a in range(nranks) for b in range(a + 1, nranks)
                  if p[a] > 1e-4 and p[b] > 1e-4 and p[a] > p[b])
        ent = -sum(x * math.log2(x) for x in p if x > 1e-12)
        floor = max((r for r in range(nranks) if p[r] > 1e-9))
        print(f"  {N:2d} {live:3d}/{nranks:<3d} {inv:11d} {ent:7.2f}b   "
              f"{label[floor]}  ({100*p[floor]:.1f}%)")
    return dists, label, nranks


# ── How the 2+3 structure fares at other V ─────────────────────────────────

def ordinal(idx, V, rank, merge, colbit, shpbit, numctr, colval):
    cm = np.bitwise_or.reduce(colbit[idx], -1)
    sm = np.bitwise_or.reduce(shpbit[idx], -1)
    nc = numctr[idx].sum(-1)
    rk = classify(cm, sm, nc, V, rank, merge)
    cnt = np.stack([(nc >> (4 * k)) & 0xF for k in range(V)], axis=-1)
    key = cnt * 16 + np.arange(V)
    order = np.argsort(-key, -1)[..., :5]
    scnt = np.take_along_axis(cnt, order, -1)
    snum = np.take_along_axis(np.broadcast_to(np.arange(V), cnt.shape), order, -1)
    tb = np.zeros(cnt.shape[:-1], np.int64)
    for k in range(5):
        tb = tb * 16 + np.where(scnt[..., k] > 0, snum[..., k] + 1, 0)
    cv = -np.sort(-colval[idx], -1)
    ex = np.zeros(tb.shape, np.int64)
    for k in range(5):
        ex = ex * 4 + cv[..., k]
    return rk.astype(np.int64) * (1 << 30) - ((tb << 11) | ex)


def chop(V, hole, board, players, trials, rank, merge, seed=11, chunk=20000):
    ncards, colbit, shpbit, numctr = deck_arrays(V)
    colval = np.array([2 - c for c in range(3) for _ in range(4 * V)], np.int64)
    rng = np.random.default_rng(seed)
    need = players * hole + board
    ch = done = 0
    while done < trials:
        m = min(chunk, trials - done)
        deal = np.argpartition(rng.random((m, ncards)), need, 1)[:, :need]
        bd = deal[:, players * hole:]
        o = np.stack([ordinal(np.concatenate([deal[:, p*hole:(p+1)*hole], bd], 1),
                              V, rank, merge, colbit, shpbit, numctr, colval)
                      for p in range(players)], 1)
        b = o.min(1)
        ch += int(((o == b[:, None]).sum(1) > 1).sum())
        done += m
    return ch / done


def chop_report(trials):
    """Only 2+3: every player holds exactly five cards, so `ordinal` is reading
    a whole hand rather than picking a best five out of more. 2+5 needs the
    selection step, and we already know what it does to the chart."""
    print(f"\n  split pots at 2 hole + 3 board, numbers then colour breaking ties\n")
    print(f"  {'deck':>20}  {'2 players':>10} {'3':>8} {'6':>8}")
    for V, lbl in ((5, "V=5  (60 cards)"), (7, "V=7  (84 cards)"), (9, "V=9 (108 cards)")):
        rank, _, _, _ = rank_table(V, False)
        vals = [f"{100*chop(V, 2, 3, p, trials, rank, False):7.2f}%" for p in (2, 3, 6)]
        print(f"  {lbl:>20}  " + " ".join(vals))


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--trials", type=int, default=120_000)
    a = ap.parse_args()
    print("  best five of N, dealt at random. N=5 is exact; the rest are sampled.")
    print("  A sequence is five numbers in a row. At V=5 that is the same thing as")
    print("  five different numbers, which is why the real deck has no junk hand.")
    for V in (5, 7, 9):
        report(V, False, a.trials)
    chop_report(max(8000, a.trials // 6))
