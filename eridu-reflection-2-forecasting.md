# Eridu — Reflection #2: the horizon gap (drop the priest framing)

**Supersedes the "priest gap" framing of Reflection #1.** You were right on both counts, so
this reflection starts by conceding them, then does the Arimaa/AlphaGo/Deep-Blue review you
asked for, ties it into the diagnosis, and documents the changes I implemented and measured.

---

## 0. On the honesty question

You asked: *did I play honestly, or did I pass through the bot code?*

Honest answer: **partly the latter, and you caught it.** What I actually did:

- I drove one seat through the **same `find-state-raw` choice set** a human/bot sees, read-only
  on the rules — that part was genuine (the playlogs are real, ~24 of my own decisions in the
  4-seat game, a focused solo segment).
- But I **did not play to game-over.** When I hit the question "does the priest line ever pay
  off?", I **read `apply-end-game-scoring` instead of playing the last ~40 turns.** That's the
  "pass through the bot code" shortcut. I disclosed it as a caveat, but disclosing a shortcut
  isn't the same as not taking it. The seat gave me a *felt* hypothesis; I then resolved it by
  reading code, and I over-trusted that hypothesis. Which leads directly to your second point.

## 1. There is no priest gap — and that's the whole lesson

You said: *"There is no priest gap, stop trying to focus on it. Focus on why the bots struggle
at feats — it's clearly the same thing."* That is correct, and it dissolves Reflection #1.

I had taken one **instance** (priest/temple felt like a trap) and reified it into a "priest
economy" problem. But the priest line isn't special. Look at the feat layer as a whole and the
*same* shape appears everywhere:

- **C1** four face-up temples, **G1** move a magistrate 4 cities, **H2** any role to 5,
  **F1** three point-side raiders, **D2** four river temples, **K1** a big gold sale…

Every one of these pays off **several turns after** the setup work begins, and **none of them
shows a per-turn gradient** while you're setting them up. The bot's evaluator scores the *type*
of an action and its *immediate* this-turn consequence. A backloaded objective is, by
construction, invisible to that evaluator: at the moment you'd start working toward it, the move
that advances it scores **worse** than grabbing immediate reputation. So the bot never starts.

That is a **horizon / forecasting gap across the entire feat system**, not an economy imbalance
in one role. The priest case was just where I happened to be sitting. Buffing temple/priest
rewards (my Reflection-#1 temptation) would have been exactly the wrong fix — patching one
symptom of a general blindness.

And — as you said — **it's the same reason the GA can't fix it by itself.** The GA only tunes
weights on the existing per-turn signal. Tuning changes *which* greedy hilltop the bot climbs;
it cannot manufacture a slope toward a peak the heuristic can't see. No gene for "value a payoff
8 turns out" exists, so evolution has nothing to select on.

## 2. The Arimaa / AlphaGo / Deep Blue review (what you asked for first)

You named three systems that forecast further than a greedy evaluator. Here's what each teaches,
and which constraint of *ours* it matches (full sourced report saved alongside; key citations
inline).

**Deep Blue / classical chess — bounded full-width α-β + quiescence.** Works because chess
branches at ~35: α-β's `O(b^(d/2))` reaches 8–12 ply, deep enough to see tactics; quiescence
search kills the *horizon effect* (a fixed-depth search shoves an unavoidable loss just past the
depth limit). **What transfers:** the horizon-effect concept itself, transposition tables
(Zobrist) since economic states transpose heavily, iterative deepening. **What does NOT:** raw
full-width depth — our branching factor forbids it, and our game is stochastic and not strictly
zero-sum, so we'd want *expectimax*, not minimax.

**Arimaa — the game built to break greedy/brute-force search.** This is the closest match to us:
~17,000 legal moves per turn (vs chess ~35, Go ~250), so depth is simply unbuyable. The program
that finally beat top humans (David Wu's *Sharp*, 2015, 7–2) won **not by searching deeper** but
by **(a) pruning the move set with a learned ranker** (it put the expert move in the top 1% of
its ranking >80% of the time, so it could discard ~97% of moves safely), **(b) encoding
objectives as *distance gradients* in the static eval** ("goal in 0–4 steps", "N steps to free a
piece"), and **(c) recognizing decisive multi-turn patterns statically** instead of searching
them. Headline for us: *in a high-branching game, put your objectives into the evaluation as
gradients and prune hard — don't chase depth.*

**AlphaGo / AlphaZero — the value network as a learned long-horizon forecaster.** Go's ~250
branching is unsearchable full-width; AlphaGo's value head outputs "expected eventual value of
*this position*", replacing thousands of rollouts. That is the cleanest statement of our goal:
**evaluate the expected eventual value of the resulting position, not the value right now.** Its
policy network prunes via *soft priors* (PUCT), the same "rank moves, mostly look at the top"
idea as Sharp. Scaled-down takeaway: you don't need a net — use the existing heuristic at the
leaves and a cheap prior to prune; and if you ever want "potential" learned rather than
hand-shaped, train a small value model on self-play outcomes.

**The throughline (all three):** depth is the thing you cannot buy in a high-branching, stochastic
game — so **put the objectives into the *evaluation* as gradients, prune the move set, and reuse
only the cheap classical infrastructure that survives any branching factor.**

## 3. The fix I implemented — potential-based reward shaping (the gradient)

The highest-leverage, lowest-cost technique from the review — and the one that attacks the root
cause rather than a symptom — is **goal-distance / potential terms in the evaluation**, which has
a clean theory backing it:

> **Ng, Harada & Russell (1999), "Policy invariance under reward transformations."** A shaping
> reward `F(s,s') = γ·Φ(s') − Φ(s)` for *any* bounded potential `Φ` over states leaves the set
> of optimal policies **unchanged** — it only redistributes credit so the agent feels per-step
> progress toward a delayed reward, without biasing what the optimum is. (Equivalently: it's the
> same as initializing the value function with `Φ`.)

So I defined a potential over the player's **planned feat chain** and made the bot score each
presented choice by the **realized change in potential**:

```
Φ(state) = Σ over still-unclaimed targeted feats f of:  value(f) · progress(f) · position-weight
ΔΦ = Φ(next-state) − Φ(state)            ; a 1-ply forecast of "how much did this advance the plan?"
```

- `value(f)` reuses the planner's own grounded ingredients (claim-order wild points + an ease
  factor from `feat-difficulty`), discounted by single-turn-burst `claimability` so phantom
  event-feats don't dominate — no invented numbers.
- `progress(f)` is the existing `feat-progress` gradient (e.g. C1 = face-up temples / 4).
- `ΔΦ` is read off the **same `next-state` a human would see** — it's a forecast of the *realized
  consequence*, which is exactly the Arimaa "distance-to-objective in the eval" and the AlphaGo
  "value the resulting position" idea, hand-shaped rather than learned.

**Where it's wired:** every *resolution* phase of `decide` — `:resolve-temple`, `:resolve-deploy`,
`:resolve-sell`, `:resolve-travel`, `:resolve-influence`, `:choose-role-increase`, and
`:choose-action`. Those are the points where the presented `next-state` reflects the true
consequence, so `ΔΦ` is meaningful there.

**Two bonus properties:**
1. **The representation bug from Reflection #1 is subsumed for free.** A dead / precondition-gated
   resolution (the "temple action that silently does nothing") produces `ΔΦ = 0`, so the forecast
   never rewards a no-op. I did *not* add a special-case temple filter — that would have been
   another local patch. The general fix handles it.
2. **It's GA-tunable now.** The signal that didn't exist before now exists, so the genome can
   learn how hard to weight it (next step, §5).

**Engineering discipline:** the term is gated behind a new gene `:feat-lookahead`, **neutral at
its 0.0 default** — every existing evolved bot and saved population behaves byte-for-byte
identically until the GA (or a bench) raises it. This matches the house pattern for new traits.

## 4. Does it work? Direct A/B (no re-evolution yet)

`eridu_lookahead_ab.clj`: 300 games, 4 copies of the **top evolved baseline** personality per
game, differing **only** in `:feat-lookahead` (2 treatment seats at 0.8, 2 control at 0.0),
treatment/control positions swapped each game to cancel seat bias. 600 seat-games per condition.

| metric (per seat-game)      | control (0.0) | treatment (0.8) | lift     |
|-----------------------------|---------------|-----------------|----------|
| **feats claimed**           | 0.805         | 0.858           | **+6.6%**|
| reputation (min amity,glory)| 11.332        | 11.592          | +2.3%    |
| game wins (by reputation)   | 144           | 174             | +21%     |

Modest but **consistent and positive on all three axes**, and — the important part — this is the
forecast term dropped onto a **fully-evolved bot with the GA never having tuned it.** It moves
feat achievement (the thing you pointed at) directly, and it doesn't cost reputation — it slightly
helps it. That's the signature of a real gradient, not a re-weighting.

## 5. Proceeding as described — now research-grounded

Reflection #1's "Phase A/B/C/D" was armchair. The review gives a better-ordered roadmap (ranked
by payoff ÷ cost for our high-branching, stochastic, delayed-reward setting). Done = ✅.

1. ✅ **Potential / goal-distance gradient** (this reflection). Root-cause fix, GA-tunable, shipped
   neutral, +6.6% feats measured.
2. **Re-evolve with `:feat-lookahead` live** (the GA can now select on the new signal — expected to
   exceed the static 0.8 result). This is a long detached bench; it's the natural next go-ahead.
3. **Heuristic move-ranker + top-N pruning** (Sharp's lever). Cheap; the enabler for any real
   lookahead and it improves greedy tie-breaking on its own.
4. **Depth-2/3 expectimax** over the *pruned* move set, shaped heuristic at the leaves, with a
   Zobrist transposition table. Only affordable *after* (3).
5. **Macro-actions / plan templates** for near-complete feats; and eventually a **self-play-tuned
   value** (GA-tune the §3 weights against multi-turn win-rate, not per-turn score) if the above
   plateau.

Each still gets its own reflection. But the framing has changed: we are no longer fixing a
"priest economy" — we are giving a greedy agent a **horizon**, one cheap layer at a time, exactly
as Arimaa and AlphaGo did.

---

### Files
- `src/cljc/eridu/decision.cljc` — `feat-value`, `feat-potential`, the `fl` forecast closure, wired
  into all resolution phases.
- `src/cljc/eridu/personality.cljc` — `:feat-lookahead` gene (default 0.0, range [0,1.5], in
  `random-personality` + `weight-bounds`).
- `test/clj/eridu/decision_test.clj` — neutral-at-default, live-when-nonzero, Φ-monotonicity guards
  (full suite green: 11 tests / 2145 assertions).
- `eridu_lookahead_ab.clj`, `eridu-lookahead-ab-results.txt` — the A/B harness and result.
