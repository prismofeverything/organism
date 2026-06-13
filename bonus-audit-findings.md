# Bonus three-way audit findings (intent vs spec vs code vs oracle)

7 agents, 175 slots, 39 flagged. by_verdict: {'spec-wrong-clauses': 3, 'choice-type-wrong': 2, 'code-violates-intent': 19, 'oracle-wrong': 12, 'multiple': 2, 'spec-mislabels-state': 1}

Source: verify-bonus-specs workflow. Severity-sorted. Adjudication column added during triage.

## [5 3] — choice-type-wrong [high]
Card: 'Take a Deploy action then a Temple action' — no travel, no city choice. AUTO path is faithful (place raider on a route + place-temple-in caravan). But the slot is registered (bonus.cljc bonus-needs-choice?) as :pick-city filter :adjacent action :deploy, and the HUMAN path (apply-bonus-with-choice [5 3], grouped with [27 2]/[33 4]) does (travel-to chosen-city)+(auto-deploy-near) — it inserts a spurious TRAVEL and DROPS the temple clause. Wrong choice TYPE (should need no city pick) plus dual-path divergence. Spec [5 3] marks both :deploy and :place-temple :done with no :dual-path? flag, no interactive? marker, and no note — masking the human-path travel/temple defect. Oracle correctly expects delta-raiders 1 + delta-temples 1, which the human path violates.

## [8 0] — code-violates-intent [high]
Card: 'When you score a Raider, instead flip it to its active side' — flip INSTEAD of scoring (net 0 glory). Oracle [8 0] correctly notes 'per-trigger nets 0 glory'. But CODE (choice.cljc score-own-raider-on-route lines 648-667): when :keep-scored-raider is set it flips the raider to :raiding AND still falls through to add-glory 4. So the player keeps the raider active AND gains 4 glory — double benefit, violating 'instead'. Spec [8 0] marks :done (mislabels a broken effect).

## [10 3] — code-violates-intent [high]
Card: 'Place a Raider adjacent to a Magistrate. Score Amity based on your Leader level.' CODE (lines 1585-1596) places the raider but scores a FLAT +2 amity, not leader-level amity. ORACLE [10 3] correctly expects :delta-amity (leader-lvl). SPEC [10 3] mislabels the amity clause as :score-amity :amount 2 :state :done (no :approximation?), so spec both has the wrong amount and wrongly calls it faithful. Code should score leader-level amity.

## [12 3] — multiple [high]
Card: 'Increase your Merchant level (paying any costs). Then Sell to the city you are IN for Glory instead.' AUTO arm (game.cljc:1619) does increase-role-with-cost :merchant + flat +3 glory — a reasonable labeled approximation. But the HUMAN arm DIVERGES badly: bonus-needs-choice? returns {:type :pick-city, prompt 'Choose a magistrate city to sell in'} grouped with [9 4], and apply-bonus-with-choice routes [12 3] into the ([9 4][12 3]) branch = auto-sell-in choice-value. That (a) DROPS the merchant increase entirely, (b) sells in a MAGISTRATE city instead of 'the city you are in', and (c) yields AMITY, not the Glory the card specifies. Choice-type is also wrong: card needs no city pick at all (sell in current city). Dual-path divergence + wrong choice-type + wrong reward currency. Spec only documents the auto proxy and does not flag the human path.

## [15 3] — multiple [high]
Card: 'Increase your LOWEST role then take a Travel action (you pick if there is a TIE).' Three problems. (1) Travel is dropped in BOTH arms (spec marks :travel :stub — disclosed). (2) AUTO/HUMAN DIVERGE: auto arm (game.cljc:1677) does increase-role-FREE on the lowest role; human arm routes [15 3] through the ([15 3][35 4][35 3]) :pick-role group = increase-role-WITH-COST. Auto giving it free contradicts the card (no 'free' on this slot) — and the two arms apply different cost rules. (3) CHOICE-TYPE wrong: 'you pick' only breaks ties among the lowest roles, but bonus-needs-choice? offers {:type :pick-role 'Choose a role to increase'} letting the human pick ANY role, not just a tied-lowest one. Oracle correctly models lowest-role + cost (delta-roles {(lowest-role) 1} + role-up-resource-delta), so oracle matches the card better than the code does.

## [17 1] — choice-type-wrong [high]
Card: 'Place a Raider next to Eridu on its point side' — a deterministic placement needing NO player choice (or at most a route pick near Eridu). bonus-needs-choice? (bonus.cljc line 231) tags it :pick-resource, and the human arm (game.cljc line 1351) merely grants the chosen resource. The auto arm (line 1709) instead FLIPS an existing :raiding raider to :point and places nothing. Both paths are wrong and neither places a raider next to Eridu. Spec faithfully documents this as the Gap-3 divergence (clause :place-raider :target :eridu :stub) and oracle note flags the impl gap, so SPEC/ORACLE are correct; CODE + the choice-type are wrong.

## [22 4] — code-violates-intent [high]
Card 'Score 2 Amity for each of your Raiders. Then take a travel action.' AUTO arm correctly does +2*raiders amity (travel dropped). But the HUMAN arm routes [22 4] into the simple-travel case (apply-bonus-with-choice line ~1385 / bonus-needs-choice ~264) = (travel-to choice) ONLY — it drops the entire 2-amity-per-raider scoring, the card's primary effect. A human who plays this gets travel and zero amity. SPEC is also wrong here: it labels both clauses :done and calls it 'faithful — human travel', not flagging that the human path loses the amity.

## [29 1] — code-violates-intent [high]
Card: 'Decrease your Leader role to increase ALL of your OTHER roles.' Other roles (roles = [:merchant :priest :raider :leader]) are merchant, priest AND raider — three roles. Code (game.cljc:1902) only increases merchant and priest; the RAIDER increase is dropped. Oracle [29 1] (closure at bonus_oracle.clj:756) correctly expects leader -1, merchant +1, priest +1, raider +1, so oracle catches it. Spec [29 1] lists only decrease-leader + increase merchant + increase priest (all :done), mirroring the buggy code and missing the raider clause. Fix: add (increase-role-free player-key :raider) to the [29 1] arm and add the raider clause to the spec.

## [31 3] — code-violates-intent [high]
Card = 'Gain a resource of your CHOICE and place a Facedown temple in your city (even if you already have one)'. Human path routes through bonus-needs-choice? :pick-resource and apply-bonus-with-choice groups [31 3] with the plain pick-resource arm -> it ONLY adds the chosen resource and DROPS the temple entirely. Auto arm adds gems(1) (not player choice) and merely FLIPS an existing face-up temple to face-down rather than PLACING a new facedown temple, plus +2 amity. Spec lists place-temple :side :face-down :done with no truncation/dual-path note; it should mark the human path as dropping the temple clause and the auto path as flip-temple, not place-temple. Oracle (delta-temples 1, pick-resource) describes the intended effect, not what either arm does.

## [31 4] — code-violates-intent [high]
Card = 'Gain a resource of your choice and take a Deploy action'. Human path is grouped into the plain :pick-resource arm -> adds only the chosen resource, DROPS the deploy. Auto arm adds tools(1) (fixed, not chosen) then places one raider. Spec marks the deploy clause :done with no dual-path/truncation note; the human (player-experienced) path never deploys. Spec should flag dual-path + human truncation.

## [33 1] — code-violates-intent [high]
Card = 'Decrease your Merchant role to increase all of your OTHER roles (paying costs)'. The three other roles are leader, priest, raider. Code (when merchant>1) decrements merchant and calls increase-role-free for ONLY :raider and :priest -- LEADER is never increased. Spec lists exactly two increase-role-free clauses (raider, priest), so it matches the buggy code but omits the leader clause the card requires. Oracle [33 1] (alter-var-root) is CORRECT: it bumps leader, priest, AND raider by +1 when merchant>1. So code+spec are both missing the leader increase that intent and oracle demand.

## [34 1] — oracle-wrong [high]
Card = 'Pay Tools, Tools to place a Raider on each space surrounding Uruk' (Uruk has 4 adjacent routes -> 4 raiders for 2 tools). Oracle encodes delta-raiders 4, tools -2, which matches the card intent. But CODE caps placements at (min tools (count avail) 2) -> at most 2 raiders, and pays only n=that-count tools (not a fixed 2). So code under-delivers vs both card and oracle. Spec marks place-raider :around-uruk :cost :tools :done with no cap note. The mismatch surfaces here as oracle (4) vs code (<=2); code violates intent and spec mislabels :done. Tagged oracle-wrong because oracle's fixed delta-raiders 4 will never be met by the code, but the underlying fault is the code's 2-raider cap.

## [35 2] — code-violates-intent [high]
Card = 'You may pay any number of Pottery; for each Pottery paid, place a Temple in a city WHICH YOU HAVE A TEMPLE' (i.e. an additional temple in a city where you already have one). CODE inverts the target: n=(min pottery 2) and it places temples in cities you do NOT have a temple in (cities = (remove #(contains? (:temples pdata) %) (keys city-graph))). So it places in temple-LESS cities, the opposite of the card, and caps at 2 instead of 'any number'. Spec marks place-temple :cost :pottery :basis :per-pottery-paid :done with NO note about the inverted target or the 2-cap. Oracle deltas are placeholder {:temples 0,:pottery 0}.

## [3 3] — spec-wrong-clauses [medium]
Card: place raider adjacent to Eridu AND gain a good of choice. Dual-path divergence the spec misses: HUMAN path (apply-bonus-with-choice [3 3]) only does (add-player-resource choice) and NEVER places the raider; AUTO path places the raider but grants fixed :tools (no choice). Spec marks both clauses :done and only notes 'auto grants fixed tools', omitting that the human/UI path drops the :place-raider clause entirely. Spec should flag :dual-path? and mark the place-raider clause :partial on the human path. Oracle correctly expects delta-raiders 1 + pick-resource, which the human path fails to deliver.

## [6 4] — oracle-wrong [medium]
Card: 'Place a Raider adjacent to Lagash. Gain Tools, Tools.' Code (lines 1527-1528) ONLY adds 2 tools; the raider is never placed (no human arm either). Spec correctly marks the raider clause :stub. But ORACLE [6 4] asserts :delta-raiders 1, which the code never produces — oracle is wrong (should be :delta-resources {tools 2} only, or note raider unimplemented).

## [7 2] — code-violates-intent [medium]
Card: 'Place a Temple in a city with a Magistrate (even if you already have a temple there)'. AUTO arm (lines 1534-1537) filters out magistrate cities where you already have a temple — so if your only magistrate city already holds your temple, nothing is placed, directly contradicting 'even if you already have a temple there'. Human arm (apply-bonus-with-choice [7 2]) is correct (places in chosen city unconditionally). Dual-path divergence the spec does not flag; spec marks [7 2] plain :done.

## [7 3] — code-violates-intent [medium]
Card: 'Take a travel action. Score 3 Glory if you are in Eridu.' AUTO arm (lines 1538-1540) forces caravan to :eridu and always +3 glory. HUMAN arm (apply-bonus-with-choice [7 3], line 1385) only calls travel-to to a player-picked ADJACENT city and scores NOTHING — the 3 glory is silently dropped on the human path (bonus.cljc comment falsely claims 'score happens at destination automatically'). Spec marks both clauses :done with note about auto-travel-to-Eridu but does not flag that the human path awards zero glory. Dual-path divergence.

## [7 4] — code-violates-intent [medium]
Card: 'Take a travel action. Score 3 Amity if you are in Kish.' Same dual-path bug as [7 3]: AUTO arm (lines 1541-1543) forces caravan :kish +3 amity; HUMAN arm (line 1385) only travels to a chosen adjacent city and scores no amity. Spec marks :done without flagging the human-path drop.

## [11 2] — code-violates-intent [medium]
Card explicitly says 'Sell to Lagash for Double Glory (you don't have to be there)'. Code (game.cljc:1604) sets caravan to :lagash, i.e. it MOVES the player to Lagash — directly contradicting 'you don't have to be there', mutating board position as a side-effect. Glory proxy (+4) is a labeled approximation and acceptable, but the forced travel is a real intent violation. Spec note discloses 'caravan move' but does not flag that this contradicts the printed 'you don't have to be there'.

## [15 4] — oracle-wrong [medium]
Card: score 3 Amity for each Raider adjacent to a Magistrate. Code (game.cljc:1679) correctly computes adjacency (routes touching magistrate cities) and adds 3*adj-count amity; spec :score-amity :raiders-adjacent-magistrate :done (note correctly overrides hand-map :partial). But ORACLE encodes :delta-amity 0 with a note claiming adjacency 'not easily computable from pre-state' — it is fully pre-state computable (the code does exactly that), so the oracle silently expects 0 when the real delta is 3 per adjacent raider. Code/spec/intent agree; the oracle is wrong/under-specified.

## [18 2] — code-violates-intent [medium]
Card: 'Take a travel action then score 5 Glory IF you have a facedown temple in Samarra.' Code (line 1731) grants +5 when the Samarra facedown temple exists but ALSO grants +2 glory when the condition is UNMET — the card grants 0 in that case. Oracle correctly returns 0 when unmet. Spec correctly flags the score-glory clause :partial with note 'grants +2 glory even when condition unmet — not faithful'. SPEC/ORACLE right, CODE wrong.

## [20 1] — code-violates-intent [medium]
Card: 'Place a Raider on each route with an OPPOSING raider.' Code (line 1770) instead places (up to 2) raiders on routes adjacent to the player's own caravan city, with NO check for an opposing raider on the route — wrong target selection entirely. Spec correctly flags this: clause :place-raider :opposing-routes :partial with note 'code places on routes from caravan, ignoring the opposing check'. Oracle declares no static delta. SPEC/ORACLE right, CODE wrong.

## [20 3] — code-violates-intent [medium]
Card: 'Influence a Magistrate. Then score Amity based on your leader level.' Auto arm (line 1781) does +amity = leader level but SKIPS the influence/magistrate move. Human arm (line 1406, shared [20 3][25 1][30 2][32 4]) does do-influence then auto-sell-in — i.e. it SELLS instead of scoring leader-level amity (wrong tail). Spec captures both faults: influence clause :partial 'auto skips influence' and score-amity clause :partial 'human path sells instead of scoring leader-level amity — wrong tail'. Oracle = leader-lvl amity (correct rule). SPEC/ORACLE right; both CODE paths wrong.

## [21 0] — oracle-wrong [medium]
Card: 'may place an additional temple facedown in that city.' CODE (apply-passive-dispatch [21 :temple-placed]) returns state unchanged — explicit NO-OP (model keys temples by city, can't hold two). SPEC correctly marks :stub. But ORACLE [21 0] declares :delta-temples 1, claiming a temple is added per trigger. That contradicts the no-op code; oracle should have no temple delta.

## [22 2] — oracle-wrong [medium]
Card 'put a random demand token on each facedown temple.' CODE adds amity = count-face-down-temples (labeled approximation; SPEC marks :place-demand :partial :approximation?). But ORACLE [22 2] specifies NO delta (only :notes), so it understates the code, which does add +amity equal to the facedown-temple count. Oracle should carry a :delta-amity = facedown count.

## [23 4] — code-violates-intent [medium]
Card 'Place a Temple in a city with a Magistrate (EVEN IF you already have a temple there).' AUTO arm filters to magistrate cities where (not (contains? temples city)) — it actively EXCLUDES magistrate cities where you already have a temple, the exact opposite of the card's 'even if you already have one' allowance (and the model can't hold two temples in a city anyway, so the headline allowance is unimplementable). SPEC [23 4] only says :place-temple :magistrate-city :done and does not flag the 'even if already' violation. Choice-type :pick-city :magistrate is correct.

## [24 2] — oracle-wrong [medium]
Card 'put a random demand token on each Magistrate.' CODE adds flat +2 glory (SPEC :place-demand :partial :approximation? 'flat +2 glory; no demand tokens'). But ORACLE [24 2] specifies NO delta (only :notes), understating the code which adds +2 glory. Oracle should carry :delta-glory 2 to match impl.

## [24 4] — oracle-wrong [medium]
Card 'Take a good for each demand in cities with Magistrates.' CODE grants up to 2 goods matching the player's own :demand-tokens (not demands sitting at magistrate cities) — SPEC labels this :partial :approximation?. ORACLE [24 4] has :delta-resources {} (empty) with a note describing the LITERAL '+1 per demand at every magistrate city'; the empty delta matches neither the literal card nor the code (which grants up to 2 resources). Oracle understates the code's resource grant.

## [25 1] — code-violates-intent [medium]
Card 'Influence a Magistrate. Immediately score all of your raiders it moved through' (GLORY). AUTO arm: +glory (2 + point-raider count). HUMAN arm routes [25 1] into the influence+sell case (do-influence then auto-sell-in) — it produces AMITY from a sell instead of glory for raiders moved through; wrong tail. SPEC documents this with :score-glory :partial note 'human path sells instead — wrong tail'. Choice-type :pick-city :magistrate is correct. Oracle :delta-glory point-raiders matches only the auto arm.

## [25 3] — oracle-wrong [medium]
Card 'Place two facedown temples in your city (even if you already have one).' CODE does NOT add temples — it flips up to 2 EXISTING face-up temples to face-down and adds +1 amity each (SPEC :place-temple :partial :approximation? documents exactly this). ORACLE [25 3] declares :delta-temples 2, which is wrong: code's net temple count is unchanged (flip, not add) and oracle also ignores the amity the code grants. Oracle mismatches the impl.

## [26 0] — spec-mislabels-state [medium]
Card: 'When you score Magistrate BONUS points, score +2 Amity.' Code triggers on [26 :sold] and fires only when (:glory-scored context) > 0, i.e. glory from selling in a city that happens to have a magistrate present (leader-bonus glory at choice.cljc:417). That is NOT the same event as scoring magistrate bonus points; it is a sell-adjacent-to-magistrate proxy. Spec clause {:kind :score-amity :condition :magistrate-bonus :state :done} should be :partial / :approximation? — :done overstates fidelity. Oracle's trigger-event :score-magistrate-bonus is a semantic label that does not match the actual :sold dispatch key either.

## [26 3] — oracle-wrong [medium]
Auto arm (game.cljc:1858) does (update amity +2) AND place-temple-in caravan with allow-duplicate? true, which always places a temple when supply>0 (+1 temple). Oracle [26 3] is {:delta-temples 0} with NO delta-amity — it misses both the guaranteed +1 temple and the +2 amity the code produces. Spec correctly flags both (sell→flat+2 amity proxy; temple placed unconditionally), so spec/code agree; the oracle is the wrong source. Correct expected delta: delta-amity 2, delta-temples 1 (supply permitting).

## [26 4] — oracle-wrong [medium]
Card: place a Raider ADJACENT to your city; if you surround it, place a temple. Auto arm (game.cljc:1861) picks the FIRST globally-available active route (not one adjacent to the player's city/caravan) AND adds +2 amity as a proxy for the dropped surround-temple. Oracle [26 4] {:delta-raiders 1} omits the +2 amity the code actually grants. Spec target :adjacent-to-caravan is also inaccurate (code does not filter by caravan) and the spec omits the +2 amity proxy. Most actionable: oracle should include delta-amity 2; spec should drop the :adjacent-to-caravan target claim and note the amity proxy.

## [29 4] — oracle-wrong [medium]
Card: place a Temple in EACH city surrounded by your Raiders. Code (game.cljc:1916) just calls place-temple-in on the caravan with allow-duplicate? true — one guaranteed temple at the caravan, ignoring surround state and 'each city'. Spec marks place-temple :partial :approximation? (one temple at caravan vs each surrounded city) — accurate. Oracle [29 4] {:delta-temples 0} contradicts the code, which deterministically places +1 temple (supply permitting). Oracle should be delta-temples 1 to match the impl, or the impl reworked to honor surround.

## [32 2] — spec-wrong-clauses [medium]
Card = 'Take a Gem. Take two travel actions.' The two clauses never co-occur and the spec omits this. Auto arm grants gems(1) and ZERO travel; human path is grouped in 'simple travel' so it travels ONCE and grants NO gem. Spec lists gain-resource :gems :done + travel :count 2 :partial, but the gem clause is NOT :done on the human path (gem only on auto) and travel is absent on auto -> this is a dual-path slot the spec fails to mark :dual-path?, and 'count 2' is really at most 1. Oracle only encodes {:gems 1}.

## [33 2] — spec-wrong-clauses [medium]
Card = 'Place a facedown Temple in your city then take a travel action'. Auto arm FLIPS an existing face-up temple to face-down (+amity proxy) and does NOT travel; human path is grouped in 'simple travel' so it ONLY travels and does NOT place/flip any temple. Spec marks place-temple :face-down :done + travel :done and tags :dual-path?, but labeling both clauses :done overstates: each path does exactly one of the two, and the auto 'place' is really a flip-temple of an existing temple, not placing a new facedown one. The :dual-path? flag is present but the per-clause :done states are misleading.

## [6 3] — code-violates-intent [low]
Card: 'Sell to Babylon for double points (you don't need to be there)'. Code (lines 1524-1526) moves caravan to :babylon AND flat +4 amity. The caravan move contradicts 'you don't need to be there'. Spec already documents both the +4 proxy (:approximation?) and the caravan bug in its :note, so spec is faithful; flagging the residual code bug at low severity.

## [16 2] — oracle-wrong [low]
Amity-timing divergence. Card: 'Deploy then score Amity for each Raider you have.' Code deploys then counts post-deploy raiders (raiders+1)*2. Oracle uses pre-deploy raider-count, so oracle under-counts by 2 amity whenever the deploy succeeds. Spec marks both clauses :done and does not flag the off-by-one; oracle note admits the strict reading would add +1. Code's post-deploy reading matches card text; oracle is the wrong source here.

## [23 3] — code-violates-intent [low]
Card 'take a good of choice, then travel, then increase Merchant.' AUTO arm: +tools +merchant role-up. HUMAN arm routes [23 3] into the bare pick-resource case = (add-player-resource chosen) ONLY — it silently drops the Merchant role increase (and travel). SPEC documents this with note ':note auto only' on the increase-role clause and :stub on travel, so the divergence is labeled; flagged low because it is documented but is a real human-path drop of a card clause. Oracle includes both the merchant role-up and the chosen good.

---

# Adjudication (triage by fix mechanism)

**A. Clear bugs, fixed STRUCTURALLY by the clause interpreter (dual-path human-arm drops/swaps, 15 slots):**
[3 3] [5 3] [7 2] [7 3] [7 4] [12 3] [15 3] [20 3] [22 4] [23 3] [25 1] [31 3] [31 4] [32 2] [33 2].
The human path (apply-bonus-with-choice) is a separate hand-table with wrong groupings; one apply-clauses for both paths eliminates the class.

**B. Clear bugs, fixed by one faithful handler each (spec corrected 2026-06-12):**
[10 3] amity=leader-level not flat 2 · [29 1] +raider · [33 1] +leader · [34 1] 4 raiders around Uruk not capped 2 ·
[6 3]/[11 2] no spurious caravan move on "don't need to be there" · [18 2] glory 0 when condition unmet · [20 1] target=opposing routes.

**C. Genuine ORACLE errors to correct (so parity is valid): [15 4] (adjacency IS computable→3·adj), [16 2] (post-deploy count), [26 3] (+amity+temple), [29 4] (each surrounded).
   NOT oracle errors — oracle=intent, code=wrong, keep as faithful target: [6 4], [34 1].**

**D. DESIGN / RULES decisions — Mohammad's call, block faithful impl:**
- Temple data-model: temples keyed by city can't hold two; cards say "additional/two facedown temple(s) even if you have one" → [21 0] [25 3] [31 3] [33 2] [35 2] [23 4] [7 2].
- [8 0] "instead flip it to active": does flip negate the 4 glory (oracle) or keep raider AND glory (current code)?

**E. Separate: [26 0] proxy trigger (spec corrected to :partial).**

---

## Bug-report correlation — 2026-06-13 (6 in-game reports)

Source: `~/Documents/eridu-bug-reports.jsonl` (6 reports, play-keys trigger-test/handtest/splooooo).
Full delta + per-report multi-perspective verdicts: `bonus-coverage-delta.md`.

- **R1 [5 3]+[5 0]** "didn't get to select a raider to place the temple next to; passive didn't work" → existing choice-type/dual-path finding ([5 3]) + S6 passive stub ([5 0]). OPEN.
- **R2 E1** "Kish surrounded but couldn't score E1" → ✅ FIXED `56b0835` (contest topology via stored `:routes`).
- **R3 [35 0]** "no goods, no choice to select goods at turn start" → ✅ FIXED (defensive) `56b0835` BUG B. *Verify via snapshot replay.*
- **R4 [18 2]** "travel one space through a prompt window" → existing Bucket-B finding (same teleport mechanism as R5).
- **R5 SYSTEMIC** "bonus move to Babylon didn't pick up my point raider — systemic prompt-vs-action-resolution" → **root cause confirmed in code**: bonus-travel is `(assoc-in … :caravan choice)` teleport, never traverses path → drops point-raider pickup + river triggers. The pure dual-arm split; primary motivation for the step-D interpreter. **Highest-value single fix: make bonus-travel run the real travel action.**
- **R6 [18 1]** "moved magistrate across a road when card says river" → **NEW class**: `[18 1]`→`perform-influence`→`road-clockwise-path` walks road edges with no route-`:type` gate. Propose **S8 — typed-movement constraints** (river-only/road-only). Confirm card text before fixing.

Status of meta-fix (step D, clause-interpreter): authored in `effect_spec.cljc`, interpreter NOT built (deliberately deferred). R5/R1/R6 are all "clause dropped on one arm" — what one executable interpreter closes. Awaiting greenlight before the live-dispatch rewrite.
