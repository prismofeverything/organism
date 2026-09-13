The three-player circulation/pass pattern has several identifiable causes. This
is a read-only audit; the training rules, rewards, search, and sampling settings
were not changed during it.

We froze 30 recent completed games for each active model and one checkpoint per
model, then replayed every recorded action through the Rust rules. Movement
availability below requires an executable source/destination, not merely an
available Move menu entry. Circulation availability was checked through its
source/destination choices too. These are decision-weighted observations from a
small recent window; long games contribute more decisions. They are not causal
estimates or an unbiased sample of all possible positions.

| Finding | 2p / 3 rings | 3p / 4 rings |
|---|---:|---:|
| Audited games | 30 | 30 |
| Rule victories | 21 | 6 |
| Exact-repetition cutoffs | 9 | 16 |
| Length cutoffs | 0 | 8 |
| Mode decisions | 2,426 | 12,804 |
| Executable movement available at mode selection | 2,296 (94.6%) | 11,223 (87.7%) |
| Move selected when executable | 384 (16.7%) | 3,151 (28.1%) |
| Grow mode selected | 1,161 | 6,827 |
| Grow mode selected while immediate growth unavailable | 538 (46.3%) | 3,287 (48.1%) |
| Passes | 1,344 | 8,544 |
| Passes with no executable alternative in that slot | 76 (5.7%) | 872 (10.2%) |
| Passes despite executable primary action | 764 | 4,239 |
| Passes with circulation as the only executable alternative | 504 | 3,433 |
| Circulation selected | 375 | 13,007 |
| Circulation selected despite executable primary action | 269 | 8,648 |
| Replay samples with cutoff/zero value targets | 1,226 / 5,000 | 5,000 / 5,000 |

For three-player Move-mode slots, an actual movement was available 4,415 times,
but only 550 movements were completed (572 menu selections minus
22 subsequent forced passes at destination selection). Thus it is declining
movement both when selecting the mode and after selecting Move mode.

**Grow mode and growth execution are different choices.** Mode selection grants
as many slots as there are pieces of that type in the organism. The slots can
be used for the primary action, circulation, or passing. Grow mode with empty
growers can therefore still be meaningful: circulate food into a grower, then
spend a later slot growing. If there is no usable food anywhere, circulation is
unavailable, but Eat mode can still acquire food from an empty adjacent space.
If a chosen mode leaves only passes, it can be a legal way to wait. Masking a
mode merely because its primary action is currently unavailable would remove
valid circulation/preparation strategies. Any future merging of redundant
forced-pass paths needs successor-equivalence checks and must preserve timing,
turn boundaries, integrity resolution, center scoring, and cutoff accounting.

Three-player Grow-mode slots account for 14,640 of the 23,237 action slots in the
sample. Only 328 were used for actual growth; 7,835 circulated and 6,477 passed.
Grow mode gave the largest available action budget, possibly tied with another
mode, at many decisions: across all modes the maximum budget was chosen in
9,084 / 12,804 cases (71%). This is consistent with the bot using Grow mode as a
large budget for circulation and waiting. It does not prove that the number of
slots itself caused the learned preference.

**There is a reproducible action-order bias in search.** A fresh root starts
with zero visits. The PUCT exploration term contains sqrt(parent.visits), so it
is zero for every action on the first simulation, regardless of neural prior.
The current max-by tie-break chooses the last child, which is Pass at an action
slot. A controlled one-simulation probe using a strongly Eat-favoring evaluator
put 100% of its visit target on Pass, on both boards. This probe isolates the
ordering mechanism, not its eventual effect at the production 64 simulations.
The same zero-visit situation can occur deeper in a search.

The final greedy action selector also resolves exact visit ties toward the
highest action index. In the frozen three-player replay, only 13 of 1,820 greedy
action-slot targets (0.7%) had a top tie involving Pass. Carried episode samples
had 259 / 31,546 (0.8%). Consequently final tie-breaking alone does not explain
most passing. A controlled comparison should remove positional preference in
ties and ensure the first simulation respects priors, without preferring Move,
Eat, or aggression. A positive root exploration count and reproducibly seeded
uniform tie-breaking are candidate implementations to test.

**Replay currently lets a few long games dominate training.** The buffer holds
5,000 positions per model and receives all positions of each completed game.
One 4,000-choice cutoff can occupy 80% of that buffer; two such arrivals can
replace the entire previous contents. All 5,000 positions in this three-player
snapshot had neutral cutoff targets, despite six victories among the 30 audited
games. Its action-slot policy targets averaged 50.9% Pass, 38.9% Circulate, 3.9%
Eat, 3.5% Grow, and 2.8% Move. The largest circulation-heavy games hit the choice
limit: one contained 908 circulations, 488 passes, and only 12 moves.

This supports a feedback-loop hypothesis: long passive games dominate replay,
neutral outcomes offer little evidence for productive alternatives, and policy
training reproduces passive search targets. It is not proof that every pass or
circulation is strategically wrong. Uniform sampling of positions and uniform
sampling of games optimize different distributions; neither is automatically
bias-free. A larger replay window spanning many complete games is the least
intrusive first experiment. Per-game sample caps or game-balanced replay should
be explicit ablations, not secret winner oversampling. Track distinct games,
outcomes, and age in replay before increasing the buffer indiscriminately.

**Three-player AlphaZero is feasible, but its evaluation needs more care.**
Petosa and Balch demonstrated a multiplayer adaptation on small three-player
Tic-Tac-Toe and Connect-4 variants. They replace scalar value/sign-flipping with
per-player values and corresponding backups. Our engine already follows that
structure. Their results do not establish convergence or competence in a much
longer game such as Organism. [Multiplayer AlphaZero, 2019](https://arxiv.org/abs/1910.13012).

The third player's interests are not simply the negative of ours. Our terminal
utility is +1 for the winner and -1/(players-1) for each loser. For three players
that is [1, -0.5, -0.5], with components rotated by seat. Search maximizes the
acting player's component. It does not assume the other two form a single
coalition. These are local implementation facts, not a claim that this utility
is the only sensible design.

An action that hurts one opponent may help another. If a player has no preferred
outcome between two losing lines, its tie-break can decide which other player
wins. Sturtevant documents the sensitivity of multiplayer max-n values to ties
and move ordering. That strengthens the case for auditing our ordering, but
changing tie-breaking is still a change to the realized strategy, not a promise
of better play. [Current Challenges in Multi-Player Game Search](https://www.cs.du.edu/~sturtevant/papers/Multi-PlayerChallenges.pdf).

Self-play against copies of one strategy is a limited test of robustness. Work
on policy-space response oracles studies overfitting to training partners and
training against mixtures of policies. Our practical next experiment should
retain a small opponent archive and evaluate candidate + old-A + old-B across
seat permutations, including distinct opponent versions. This is an application
of the research, not an implemented PSRO algorithm. [Lanctot et al., 2017](https://papers.nips.cc/paper_files/paper/2017/hash/3323fe11e9595c09af38fe67567a9394-Abstract.html).

A single rating or victory over one fixed pair can miss cyclic strengths and
weaknesses. Alpha-Rank provides population-based evaluation for multiplayer
interactions; initially, a small matchup table and uncertainty intervals are
more useful here than implementing its full machinery. [Alpha-Rank, 2019](https://arxiv.org/abs/1903.01373).

Cutoff treatment deserves a separate experiment. We give unresolved games zero
value, just as for an actual draw. With the current three-player utility, zero
also equals the expected utility of an even one-third chance to win a decisive
game. Treating a time-limited unknown outcome as zero can therefore sustain
passive behavior. That does not justify inventing a movement bonus or making
cutoffs losses for all players. Compare properly tracked truncations, value
bootstrapping or masked uncertain value targets, and longer search horizons;
all have tradeoffs. Exact repetition, food-changing stasis, and time limits
should remain distinguishable in reports.

Recommended order: fix and test action-order invariance first; widen and monitor
replay coverage second; add mixed-opponent evaluation third; then compare cutoff
and action-representation alternatives on frozen seeds/checkpoints. Continue
both player counts, using two-player as the simpler diagnostic control.

The reproducible audit is `native/src/bin/action-audit.rs`. Build with
`cargo build --manifest-path native/Cargo.toml --target-dir native/target --release --bin action-audit`.
Run it on a frozen directory containing complete OGF game JSON files and a
checkpoint `state.json`. Raw frozen inputs are under `/tmp/organism-action-audit`;
reported counts, example positions, game identities, and the first-simulation
probe are retained in `docs/action-selection-audit.json`. Count consistency
checks passed, and every recorded move replayed legally. No GPU was used by
the audit and production training was left running.
