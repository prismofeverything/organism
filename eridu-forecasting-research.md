# Multi-Turn Forecasting in Game AI — Engineering Review

Review commissioned per Muhammad's directive ("get inspiration from actual models that intend to
forecast further out … review Arimaa learnings"). Synthesis of Arimaa, AlphaGo/AlphaZero, and
Deep Blue / classical chess, ranked for our high-branching, stochastic, delayed-reward setting.
Sourcing caveat at the end.

## 0. The core problem
The bot maximizes a per-turn heuristic *now*. A feat whose payoff lands several turns out is
invisible: the move that advances it scores worse than grabbing immediate value, so there's no
gradient toward the backloaded reward — and the GA, tuning weights on that same flat signal, can't
manufacture one. GA tuning changes which greedy hilltop you climb; it doesn't add lookahead.

## 1. Arimaa — built to break greedy/brute-force search (closest match to us)
- Branching ≈ 17,000 moves/turn (chess ~35, Go ~250) — depth is unbuyable; step-based play also
  weakens α-β cutoffs; randomized setup kills opening books; few captures kill endgame tablebases.
- David Wu's **Sharp** beat top humans 7–2 (2015) using **iterative-deepening α-β, not MCTS**.
- Levers that made bounded search work at 17,000 branching:
  1. **ML move-ranking used for pruning** (Bradley–Terry over expert games): expert move in top 1%
     of ranking >80% of the time, top 20% >99% — so you prune ~97% of moves and rarely lose the
     right one. **Biggest single lever.**
  2. **Tactical move generators**: emit ~3% of legal moves yet capture ~97% of the eval difference
     vs passing; ~80 Elo (Wu paper §5.3).
  3. **Goal/threat-based search with relevance pruning + static goal detection** (goal-in-≤4-steps
     recognized directly, not searched).
  4. **Severely depth-limited quiescence** (≤3 moves / 12 steps).
  5. **Large hand-tuned mostly-linear eval** with explicit **distance-to-objective gradients**
     ("freeness/domination" = 0–5 steps to a sub-goal; "threatens goal in 0–4 steps"). Only the
     move-ordering is learned; the eval is hand-tuned.
  6. Standard plumbing that survived: transposition table, killer moves, history heuristic, LMR.
- Headline: **win came from pruning the move set + encoding objectives as distance-gradients in the
  eval + static pattern recognition — not raw depth.**

## 2. AlphaGo / AlphaZero — value net as a learned long-horizon forecaster
- Value head outputs expected eventual value of *this position*, replacing thousands of rollouts —
  the literal statement of "value the resulting position, not the move now."
- Policy network prunes via **soft priors** (PUCT: `Q + c·P·√N/(1+N)`), same "rank then mostly look
  at the top" idea as Sharp, expressed probabilistically.
- MCTS visit counts = a policy-improvement operator the net distills (self-play).
- **Small-project takeaway:** no net needed — use the existing heuristic at the leaves + a cheap
  prior to prune; learn "potential" from self-play outcomes only if you want it later. Minimal
  references exist (alpha-zero-general, OpenSpiel, ~250-line single-player AlphaZero).

## 3. Deep Blue / classical chess — what bounded full-width buys, and where it stops
- α-β = `O(b^(d/2))` *iff* move ordering is good; iterative deepening (anytime + ordering);
  transposition table via Zobrist hashing; quiescence search defeats the **horizon effect**;
  eval = weighted material + positional patterns; singular extensions.
- Works because b≈35 is small. **Does NOT transfer:** full-width depth (our branching forbids it),
  minimax's deterministic zero-sum opponent assumption (we want **expectimax** / chance nodes).
- **Does transfer:** iterative deepening, **transposition tables (economic states transpose
  heavily)**, move ordering, quiescence-style selective extension of only the "loud" lines.

## 4. Ranked techniques for our codebase (payoff ÷ cost)
1. **Goal-distance / "potential" terms in the eval — a gradient toward each backloaded feat.**
   Highest payoff, lowest cost, attacks root cause. Backed by **Ng/Harada/Russell 1999**: shaping
   `F = γΦ(s') − Φ(s)` for bounded Φ leaves the optimal policy set unchanged (≡ initializing the
   value function with Φ). Standard Φ = negative distance-to-goal / progress-along-plan. **This is
   exactly what Sharp's eval does.** → *Implemented.*
2. **Shallow N-ply expectimax** (not minimax — stochastic) with the existing heuristic at the
   leaves; iterative deepening + Zobrist TT; STAR1/STAR2 if eval is bounded. Gated on pruning (#3).
3. **Move-set pruning** (N-best/beam, soft PUCT-style, CHANCEPROBCUT for stochastic chance nodes).
   The multiplier that makes #2 affordable. Start with a hand-written ranker, upgrade to logistic
   regression on game logs later (Sharp's path).
4. **Macro-actions / plan templates** (Sutton–Precup–Singh options) for known multi-turn feats;
   reduces planning depth. Pair with #3 (options raise branching). HTN precedent: Horizon Zero Dawn.
5. **Learned/self-play-tuned value** (AlphaGo value-net idea, scaled to a small linear/GBM model);
   cheapest version = GA-tune the #1 weights against multi-turn win-rate, not per-turn score.
   Do last, only if #1–#3 plateau.

## Recommended order
#1 potential terms → #3 ranker+top-N → #2 depth-2/3 expectimax (ID + Zobrist TT) → #4 macros →
#5 learned/self-play-tuned. Throughline: **depth is unbuyable at our branching — put objectives in
the eval as gradients, prune hard, reuse cheap classical infrastructure.**

## Sourcing caveat
WebFetch was unavailable; primary-PDF figures corroborated via multiple independent search-result
extractions. High-confidence (cross-checked): Arimaa ~17,000 vs chess ~35 vs Go ~250; Sharp 7–2
(2015); Bradley–Terry top-1%/top-20% pruning; tactical-generator ~3%/~97%; Ng/Harada/Russell
`F = γΦ(s') − Φ(s)` policy-invariance; PUCT; α-β `O(b^(d/2))`. The "~80 Elo from tactical
generators" is paper-sourced (Wu §5.3), not independently re-confirmed. Primary PDFs to open for
byte-exact quotes: Wu 2015 ICGA (icosahedral.net/downloads/djwu2015arimaa.pdf); Ng/Harada/Russell
1999 (cs.utexas.edu/~shivaram/readings/b2hd-NgHR1999.html).
