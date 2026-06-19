#!/usr/bin/env python3
"""Generate bonus-walkthrough.md — a per-slot code-path walkthrough for every
bonus board (35 boards x 5 slots = 175 effects).

Read/document only. Data is transcribed from:
  - src/cljc/eridu/bonus.cljc   (effect-implementation-status, bonus-needs-choice?)
  - src/cljc/eridu/game.cljc     (apply-passive-dispatch, apply-bonus-dispatch,
                                  apply-bonus-with-choice, apply-bonus-effect)
"""

# ── Honesty status, one per [board slot] (slot 0 = persistent passive) ────────
# Transcribed verbatim from effect-implementation-status in bonus.cljc.
STATUS = {}
def _s(b, row):
    for s, v in enumerate(row):
        STATUS[(b, s)] = v
# row = [slot0, slot1, slot2, slot3, slot4]
_s(1,  ["persistent","implemented","implemented","implemented","implemented"])
_s(2,  ["persistent","implemented","implemented","implemented","implemented"])
_s(3,  ["persistent","implemented","implemented","implemented","implemented"])
_s(4,  ["persistent","implemented","implemented","implemented","implemented"])
_s(5,  ["persistent","implemented","implemented","implemented","implemented"])
_s(6,  ["persistent","implemented","implemented","implemented","partial"])
_s(7,  ["persistent","implemented","implemented","implemented","implemented"])
_s(8,  ["persistent","implemented","implemented","partial","implemented"])
_s(9,  ["persistent","implemented","implemented","implemented","implemented"])
_s(10, ["persistent","implemented","implemented","implemented","implemented"])
_s(11, ["persistent","implemented","implemented","implemented","implemented"])
_s(12, ["persistent","implemented","implemented","implemented","implemented"])
_s(13, ["persistent","implemented","implemented","implemented","implemented"])
_s(14, ["persistent","partial","partial","partial","implemented"])
_s(15, ["persistent","implemented","implemented","partial","partial"])
_s(16, ["persistent","implemented","implemented","implemented","implemented"])
_s(17, ["persistent","partial","partial","partial","partial"])
_s(18, ["persistent","partial","partial","partial","implemented"])
_s(19, ["persistent","implemented","implemented","implemented","implemented"])
_s(20, ["persistent","implemented","implemented","partial","implemented"])
_s(21, ["persistent","implemented","implemented","partial","implemented"])
_s(22, ["persistent","implemented","implemented","implemented","partial"])
_s(23, ["persistent","implemented","implemented","implemented","implemented"])
_s(24, ["persistent","implemented","implemented","implemented","implemented"])
_s(25, ["persistent","implemented","implemented","implemented","implemented"])
_s(26, ["persistent","implemented","implemented","implemented","implemented"])
_s(27, ["persistent","implemented","implemented","implemented","implemented"])
_s(28, ["persistent","implemented","implemented","implemented","implemented"])
_s(29, ["persistent","implemented","implemented","implemented","implemented"])
_s(30, ["persistent","partial","partial","partial","partial"])
_s(31, ["persistent","implemented","implemented","implemented","implemented"])
_s(32, ["persistent","partial","partial","implemented","implemented"])
_s(33, ["persistent","implemented","implemented","implemented","implemented"])
_s(34, ["persistent","implemented","implemented","implemented","implemented"])
_s(35, ["persistent","implemented","implemented","implemented","implemented"])

# ── Printed-card intent, transcribed from the in-source comments ──────────────
INTENT = {
 (1,0):"When you surround a city with Raiders, place a temple in it",
 (1,1):"Travel to Kish",(1,2):"Increase Raider and Leader",
 (1,3):"Place two raiders near Lagash",(1,4):"Glory per demand fulfilled",
 (2,0):"When you score a Raider, increase Priest",
 (2,1):"Increase Merchant and Raider",(2,2):"5 Amity if at a magistrate city",
 (2,3):"Temple in a magistrate city",(2,4):"Glory per facedown temple",
 (3,0):"River travel -> take a Gem (and gems worth amity)",
 (3,1):"Increase Leader (free)",(3,2):"Temple in Lagash",
 (3,3):"Raider near Eridu + a good",(3,4):"Travel then Sell",
 (4,0):"When you flip a temple, may sell in that city",
 (4,1):"Temple in Eridu",(4,2):"Gain Tools, Gems, Gold",
 (4,3):"Amity = Leader level x 2",(4,4):"2 Amity per raider",
 (5,0):"Influence magistrate in your city, travel with it",
 (5,1):"Increase Priest (free)",(5,2):"Place demand tokens in Uruk + gain resources",
 (5,3):"Deploy then Temple",(5,4):"2 Amity per raider",
 (6,0):"Action space 7 -> free Travel",
 (6,1):"Increase Merchant and Priest",(6,2):"Temple in each magistrate city",
 (6,3):"Sell to Babylon (double)",(6,4):"Raider near Lagash + Tools x2",
 (7,0):"Place raiders, extra one next to magistrate",
 (7,1):"Increase Merchant and Leader",(7,2):"Temple in a magistrate city",
 (7,3):"Travel + 3 Glory if at Eridu",(7,4):"Travel + 3 Amity if at Kish",
 (8,0):"Score raider -> flip to active instead of removing",
 (8,1):"Increase Raider and Priest",(8,2):"Place demand + sell",
 (8,3):"Gain Gold, Gems, Pottery (then sell)",(8,4):"Flip all raiders to point",
 (9,0):"Flip temple -> may increase a role",
 (9,1):"Gain Tools, Gold, Pottery + Amity = leader level",(9,2):"Increase Priest and Leader",
 (9,3):"Raider on each river",(9,4):"Sell to magistrate city + temple",
 (10,0):"Sell gold to empty demand cities",
 (10,1):"Increase Merchant (free)",(10,2):"Increase Merchant (free)",
 (10,3):"Raider near magistrate + amity",(10,4):"Temple in Nippur",
 (11,0):"Extra glory on contest claims",
 (11,1):"Place demand tokens in Lagash",(11,2):"Sell to Lagash (double glory)",
 (11,3):"Increase Raider (free)",(11,4):"Glory per facedown temple",
 (12,0):"River crossing -> place raider on that river",
 (12,1):"Increase all level-1 roles",(12,2):"Gain Gold x3 + Gems",
 (12,3):"Increase merchant + sell for glory",(12,4):"Glory per facedown temple",
 (13,0):"Temple placement -> raider adjacent",
 (13,1):"Gain Tools x3 + Glory = leader level",(13,2):"Gain Pottery x3 + Glory = leader level",
 (13,3):"Increase all level-3 roles",(13,4):"Temple adjacent to one of your raiders",
 (14,0):"Uruk travel bonus action",
 (14,1):"Glory per raider (place raider first)",(14,2):"Resources + move magistrate",
 (14,3):"Place demands in Eridu + travel to Eridu",(14,4):"Temple in Babylon",
 (15,0):"Free role increases (ignore threshold cost)",
 (15,1):"Good per demand fulfilled",(15,2):"Increase Priest + 4 Glory if Babylon facedown temple",
 (15,3):"Increase lowest role + travel",(15,4):"3 Amity per raider adjacent to a magistrate",
 (16,0):"2-astronomer space -> third action",
 (16,1):"Pottery per temple",(16,2):"Deploy + amity per raider",
 (16,3):"Increase Leader twice",(16,4):"Place demands + sell",
 (17,0):"Action space 7 -> take a good of choice",
 (17,1):"Flip one raider to point (after placement)",(17,2):"Facedown temple in magistrate city",
 (17,3):"4 Amity if you surround Uruk",(17,4):"Glory = merchant level (then sell)",
 (18,0):"Keep tools when spent + tools worth glory at end",
 (18,1):"Resources + move magistrate / sell",(18,2):"5 Glory if facedown temple in Samarra",
 (18,3):"3 Amity if you surround Kish",(18,4):"4 Amity per point raider, then remove",
 (19,0):"Take pottery -> extra pottery x2",
 (19,1):"Increase Priest twice",(19,2):"Sell to pottery cities",
 (19,3):"Discard good + move magistrate + sell",(19,4):"Flip all raiders to point",
 (20,0):"Flip temple -> discard pottery for 3 glory",
 (20,1):"Raider on each opposing route",(20,2):"Increase Merchant twice",
 (20,3):"Influence + Amity = leader level",(20,4):"Take goods from astronomer spaces",
 (21,0):"Temple placement -> extra facedown in same city",
 (21,1):"Travel to Eridu (anywhere from Eridu)",(21,2):"Increase Raider and Leader",
 (21,3):"Travel to Eridu + sell",(21,4):"Glory per demand fulfilled",
 (22,0):"Action space 7 -> same action twice",
 (22,1):"Increase Raider and Merchant",(22,2):"Demands on facedown temples",
 (22,3):"Good + travel",(22,4):"2 Amity per raider + travel",
 (23,0):"Sell -> glory instead of amity",
 (23,1):"Increase Priest and Merchant",(23,2):"Sell twice to Eridu",
 (23,3):"Good + travel + increase merchant",(23,4):"Temple in a magistrate city",
 (24,0):"Surround city -> sell there",
 (24,1):"Increase Raider and Leader",(24,2):"Demands on magistrates",
 (24,3):"Glory per demand fulfilled",(24,4):"Goods per demand at magistrates",
 (25,0):"Two raiders per path",
 (25,1):"Influence + score raiders",(25,2):"Increase Merchant and Leader",
 (25,3):"Two facedown temples",(25,4):"Good + travel",
 (26,0):"Extra 2 amity on magistrate bonus",
 (26,1):"Increase Priest and Leader",(26,2):"Increase Priest and Raider",
 (26,3):"Sell + temple",(26,4):"Raider + surround check",
 (27,0):"Role increase -> another role for double cost",
 (27,1):"Travel + sell",(27,2):"Travel + deploy",
 (27,3):"Travel + temple",(27,4):"Three goods of choice",
 (28,0):"4+ astronomers -> role increase at turn end",
 (28,1):"Travel + temple",(28,2):"Travel + temple",
 (28,3):"Sell gold to empty city",(28,4):"Raider point-side near Kish",
 (29,0):"Pay gold -> 2 amity",
 (29,1):"Decrease leader + increase others",(29,2):"Travel + sell",
 (29,3):"Raider on each river",(29,4):"Temple in surrounded cities",
 (30,0):"Take goods from other astronomer location",
 (30,1):"Influence + travel; Glory = leader level",(30,2):"Influence + sell; Amity = leader level",
 (30,3):"Deploy + influence; Glory = raider level",(30,4):"Influence + temple; Amity = priest level",
 (31,0):"Other astronomer on space 7 -> bonus travel",
 (31,1):"Increase all level-1 roles",(31,2):"Increase all level-3 roles",
 (31,3):"Resource + facedown temple",(31,4):"Resource + deploy",
 (32,0):"Sell: discard gem for priest-level scoring",
 (32,1):"Sell + glory per demand",(32,2):"Gem + travel",
 (32,3):"Raider between temple cities",(32,4):"Influence + sell",
 (33,0):"Deploy -> influence adjacent magistrate",
 (33,1):"Decrease merchant + increase others",(33,2):"Facedown temple + travel",
 (33,3):"Temple in Uruk",(33,4):"Deploy + travel",
 (34,0):"Score raiders -> amity instead of glory",
 (34,1):"Pay tools for raiders around Uruk",(34,2):"Raider on each existing route",
 (34,3):"Sell at each magistrate+temple city",(34,4):"Sell at each magistrate+temple city (same as #3)",
 (35,0):"Start of turn, no goods -> gain good of choice",
 (35,1):"Travel + sell",(35,2):"Pay pottery for temples",
 (35,3):"Increase role of choice",(35,4):"Influence + score raiders",
}

# ── Choice descriptors from bonus-needs-choice? (entry point = -with-choice) ──
# value: (choice-type, prompt, extra)
CHOICE = {
 (3,3):("pick-resource","Choose a resource to gain",""),
 (17,1):("pick-resource","Choose a resource to gain",""),
 (22,3):("pick-resource","Choose a resource to gain",""),
 (23,3):("pick-resource","Choose a resource to gain",""),
 (25,4):("pick-resource","Choose a resource to gain",""),
 (31,3):("pick-resource","Choose a resource to gain",""),
 (31,4):("pick-resource","Choose a resource to gain",""),
 (27,4):("pick-resource","Choose a resource to gain (1 of 3)","count 3"),
 (19,3):("pick-resource","Choose a resource to discard (move magistrate + sell)",""),
 (15,3):("pick-role","Choose a role to increase",""),
 (35,4):("pick-role","Choose a role to increase",""),
 (35,3):("pick-role","Choose a role to increase",""),
 (3,4):("pick-city","Travel to adjacent city and sell","filter :adjacent action :sell"),
 (27,1):("pick-city","Travel to adjacent city and sell","filter :adjacent action :sell"),
 (21,3):("pick-city","Travel to adjacent city and sell","filter :adjacent action :sell"),
 (29,2):("pick-city","Travel to adjacent city and sell","filter :adjacent action :sell"),
 (35,1):("pick-city","Travel to adjacent city and sell","filter :adjacent action :sell"),
 (34,3):("pick-city","Sell in a city with Magistrate + your Temple (no travel)","filter :magistrate-and-my-temple multi"),
 (34,4):("pick-city","Sell in a city with Magistrate + your Temple (no travel)","filter :magistrate-and-my-temple multi"),
 (5,3):("pick-city","Travel to adjacent city and deploy","filter :adjacent action :deploy"),
 (27,2):("pick-city","Travel to adjacent city and deploy","filter :adjacent action :deploy"),
 (33,4):("pick-city","Travel to adjacent city and deploy","filter :adjacent action :deploy"),
 (27,3):("pick-city","Travel to adjacent city and place a temple","filter :adjacent action :temple"),
 (28,1):("pick-city","Travel to adjacent city and place a temple","filter :adjacent action :temple"),
 (28,2):("pick-city","Travel to adjacent city and place a temple","filter :adjacent action :temple"),
 (7,3):("pick-city","Choose a city to travel to","filter :adjacent"),
 (7,4):("pick-city","Choose a city to travel to","filter :adjacent"),
 (18,2):("pick-city","Choose a city to travel to","filter :adjacent"),
 (22,4):("pick-city","Choose a city to travel to","filter :adjacent"),
 (30,1):("pick-city","Choose a city to travel to","filter :adjacent"),
 (32,2):("pick-city","Choose a city to travel to","filter :adjacent"),
 (33,2):("pick-city","Choose a city to travel to","filter :adjacent"),
 (21,1):("pick-city","Travel anywhere (from Eridu)","filter :any"),
 (13,4):("pick-city","Place a temple adjacent to one of your raiders","filter :adjacent-to-raider"),
 (2,3):("pick-city","Choose a magistrate city for your temple","filter :magistrate"),
 (7,2):("pick-city","Choose a magistrate city for your temple","filter :magistrate"),
 (23,4):("pick-city","Choose a magistrate city for your temple","filter :magistrate"),
 (9,4):("pick-city","Choose a magistrate city to sell in","filter :magistrate"),
 (12,3):("pick-city","Choose a magistrate city to sell in","filter :magistrate"),
 (20,3):("pick-city","Choose magistrate destination","filter :magistrate"),
 (25,1):("pick-city","Choose magistrate destination","filter :magistrate"),
 (30,2):("pick-city","Choose magistrate destination","filter :magistrate"),
 (30,4):("pick-city","Choose magistrate destination","filter :magistrate"),
 (32,4):("pick-city","Choose magistrate destination","filter :magistrate"),
 (30,3):("pick-city","Choose magistrate destination then deploy","filter :magistrate"),
 (18,1):("pick-city","Move magistrate across a river","filter :magistrate"),
}

# ── Passive (slot 0) behavior, per board: (trigger, where-checked, behavior) ──
PASSIVE = {
 1:(":deployed","after a deploy that surrounds a city","If context :surrounded-city is set and player lacks a temple there with supply>0: place a face-up temple, dec temples-supply. Else no-op."),
 2:(":raider-scored","when a raider is scored","Increase :priest one level if <5 and the threshold cost (role-threshold-costs) is affordable; pays the cost. Else no-op."),
 3:(":river-crossed","on river travel","Unconditionally +1 :gems (gems worth amity is a scoring-time rule, not applied here)."),
 4:(":temple-flipped","when a temple is flipped","If context city has a demand matching a held resource: sell it (dec resource, drop demand, +demand-token, + merchant-score amity)."),
 5:(":magistrate-moved","n/a","NO-OP / skip — comment: 'complex, would need movement tracking.' Passive does nothing."),
 6:(":action-space-7","landing on action space 7","Sets flag :pending-free-travel true; actual travel handled by choice.cljc."),
 7:(":deployed","after a deploy","If raider supply available and below raider-max-deployed: find a route adjacent to a magistrate-city with no raider, place a raiding raider, dec supply. Uses magistrate-cities, routes-from-city."),
 8:(":raider-scored","when a raider is scored","Sets flag :keep-scored-raider true so score-own-raider-on-route flips to :raiding instead of removing."),
 9:(":temple-flipped","when a temple is flipped","If any role is upgradeable AND affordable: stash :passive-choice-needed {:pick-role}. Resolved in apply-passive-choice (board 9) which pays cost + increases."),
 10:(":sold","on a sell","NO-OP / TODO: 'requires sell phase modification.' Passive does nothing yet."),
 11:(":feat-claimed","when a feat/contest is claimed","+glory equal to current :leader level."),
 12:(":river-crossed","when crossing a river","If context :route given, supply available, below max, no raider there: place a raiding raider on that route, dec supply."),
 13:(":temple-placed","after placing a temple","Find a route adjacent to context city with no raider; if supply + below max: place a raiding raider, dec supply. Uses routes-from-city."),
 14:(":turn-start","start of turn","NO-OP / skip — 'complex bonus action'. Passive does nothing."),
 15:(":role-increased","when increasing a role","Sets flag :free-role-increase true so choose-role-increase-choices ignores threshold cost."),
 16:(":landing","on landing","If context :astronomer-count == 2: set flag :bonus-extra-action true (third action)."),
 17:(":action-space-7","landing on action space 7","Stash :passive-choice-needed {:pick-resource}. Resolved in apply-passive-choice (board 17): +1 chosen resource."),
 18:(":end-game / :resource-spent","end of game; when a resource is spent","end-game: +glory equal to current :tools held. resource-spent: if :tools spent, refund +1 :tools (tools never consumed)."),
 19:(":goods-taken","when taking goods","If context :resources contains :pottery: +2 :pottery."),
 20:(":temple-flipped","when a temple is flipped","If pottery>0: stash :passive-choice-needed {:yes-no}. apply-passive-choice (board 20): yes -> dec pottery, +3 glory."),
 21:(":temple-placed","after placing a temple","NO-OP — data model keys temples by city, can't hold two in one city. Passive does nothing."),
 22:(":action-space-7","landing on action space 7","Sets flag :bonus-repeat-action true (same action twice)."),
 23:(":sold","on a sell","If context :amity-scored > 0: move that amount from :amity to :glory (glory instead of amity)."),
 24:(":deployed","after a deploy","NO-OP / skip — 'would need sell logic outside normal sell phase.' Passive does nothing."),
 25:(":deployed","on a deploy","Sets flag :allow-double-raiders true so resolve-deploy-choices allows placing on occupied routes."),
 26:(":sold","on a sell","If context :glory-scored > 0 (i.e. magistrate bonus fired): +2 :amity."),
 27:(":role-increased","when increasing a role","If a DIFFERENT role is upgradeable and affordable at DOUBLE cost: stash :passive-choice-needed {:pick-role, double-cost}. apply-passive-choice (board 27) pays 2x cost + increases."),
 28:(":landing","on landing","If context :astronomer-count >= 4: set flag :bonus-role-increase true (role increase at turn end)."),
 29:(":resource-spent","when a resource is spent","If :gold spent: +2 :amity."),
 30:(":goods-taken","when taking goods","NO-OP / skip — 'complex'. Passive does nothing."),
 31:(":landing","on landing","NO-OP / skip — 'complex positioning check'. Passive does nothing."),
 32:(":sold","on a sell","If gems>0 and priest-lv>merchant-lv: stash :passive-choice-needed {:yes-no}. apply-passive-choice (board 32): yes -> discard gem, re-score the sell at priest-level amity (merchant-score priest-lv) replacing merchant amity (QA lesson 7 fix)."),
 33:(":deployed","after a deploy","NO-OP / skip — 'would need to insert an influence action'. Passive does nothing."),
 34:(":raider-scored","when a raider is scored","Sets flag :raider-score-amity true so score-own-raider-on-route adds amity instead of glory."),
 35:(":turn-start","start of turn","If total of all resources == 0: stash :passive-choice-needed {:pick-resource}. apply-passive-choice (board 35): +1 chosen resource."),
}

# ── Instant-slot behavior summaries (slots 1-4), from apply-bonus-dispatch ────
# Concise note of what the dispatch arm actually does.
BEHAVIOR = {
 (1,1):"Set :caravan to :kish (travel to Kish).",
 (1,2):"increase-role-with-cost :raider, then :leader (pays threshold cost each).",
 (1,3):"Collect routes touching :lagash, drop occupied ones, place up to 2 raiding raiders (place-raider-on).",
 (1,4):"+glory = count of held :demand-tokens.",
 (2,1):"increase-role-with-cost :merchant, then :raider.",
 (2,2):"If magistrate-in-city? at player's :caravan: +5 amity. Else no-op.",
 (2,3):"Pick a magistrate-city without your temple (else first magistrate-city); place-temple-in face-up. AUTO; human path = with-choice [2 3] pick-city :magistrate.",
 (2,4):"+glory = count-face-down-temples.",
 (3,1):"increase-role-free :leader (no cost).",
 (3,2):"place-temple-in :lagash face-up.",
 (3,3):"Find a free route touching :eridu, place a raiding raider + 1 :tools; if none, just +1 :tools.",
 (3,4):"Set :caravan :eridu, +2 amity (approximates the sell).",
 (4,1):"place-temple-in :eridu face-up.",
 (4,2):"+1 each of :tools, :gems, :gold.",
 (4,3):"+amity = leader level x 2.",
 (4,4):"+amity = 2 x count-raiders-deployed.",
 (5,1):"increase-role-free :priest.",
 (5,2):"Draw up to 2 demand tokens from :demand-bag, push each onto :uruk demands AND grant matching resource.",
 (5,3):"Place a raiding raider on any free route, then place-temple-in at :caravan.",
 (5,4):"+amity = 2 x count-raiders-deployed.",
 (6,1):"increase-role-with-cost :merchant, then :priest.",
 (6,2):"For each magistrate-city without your temple: place-temple-in face-up.",
 (6,3):"Set :caravan :babylon, +4 amity (approximates the double sell).",
 (6,4):"+2 :tools only. PARTIAL: 'Raider near Lagash' part is NOT placed.",
 (7,1):"increase-role-with-cost :merchant, then :leader.",
 (7,2):"Pick magistrate-city without your temple; place-temple-in face-up. AUTO; human path = with-choice [7 2].",
 (7,3):"Set :caravan :eridu, +3 glory (assumes the 'if at Eridu' condition by travelling there first).",
 (7,4):"Set :caravan :kish, +3 amity (travels to Kish first).",
 (8,1):"increase-role-with-cost :raider, then :priest.",
 (8,2):"+3 amity (approximates place-demand-then-sell).",
 (8,3):"+1 each :gold, :gems, :pottery. PARTIAL: no sell step (status :partial).",
 (8,4):"Set every held raider to :point.",
 (9,1):"+1 each :tools, :gold, :pottery; +amity = leader level.",
 (9,2):"increase-role-with-cost :priest, then :leader.",
 (9,3):"Place raiding raiders on up to 3 free river routes (:type :river).",
 (9,4):"Pick magistrate-city without your temple; set :caravan there, +2 amity, place-temple-in. AUTO; human path = with-choice [9 4] sell-in-magistrate.",
 (10,1):"increase-role-free :merchant.",
 (10,2):"increase-role-free :merchant.",
 (10,3):"Place a raiding raider on a free route touching a magistrate-city, +2 amity; if none, just +2 amity.",
 (10,4):"place-temple-in :nippur face-up.",
 (11,1):"Set :caravan :lagash, +1 :gold +1 :pottery (approximates placing demand tokens).",
 (11,2):"Set :caravan :lagash, +4 glory (double-glory sell approximation).",
 (11,3):"increase-role-free :raider.",
 (11,4):"+glory = count-face-down-temples.",
 (12,1):"For each role currently at level 1: set to 2 (free).",
 (12,2):"+3 :gold, +1 :gems.",
 (12,3):"increase-role-with-cost :merchant, +3 glory (sell-for-glory approximation).",
 (12,4):"+glory = count-face-down-temples.",
 (13,1):"+3 :tools; +glory = leader level.",
 (13,2):"+3 :pottery; +glory = leader level.",
 (13,3):"For each role currently at level 3: increase-role-with-cost.",
 (13,4):"AUTO: place-temple-in the first city adjacent to any held raider. Human path = with-choice [13 4] pick-city :adjacent-to-raider. Fires :temple-placed passive.",
 (14,1):"Place a raiding raider on a free route touching :lagash, then +glory = count-raiders-deployed. PARTIAL.",
 (14,2):"+1 :tools, +1 :pottery. PARTIAL: no magistrate move.",
 (14,3):"Draw up to 2 demand tokens onto :eridu demands, set :caravan :eridu. PARTIAL (status).",
 (14,4):"place-temple-in :babylon face-up.",
 (15,1):"For each held demand-token: +1 of that resource.",
 (15,2):"increase-role-with-cost :priest; if :babylon temple is :face-down, +4 glory.",
 (15,3):"increase-role-free on the lowest-level role. PARTIAL: no travel.",
 (15,4):"+amity = 3 x (raiders on routes touching a magistrate-city). PARTIAL: status flags adjacency caveat.",
 (16,1):"+pottery = count-temples-placed.",
 (16,2):"Place a raiding raider on any free route, then +amity = 2 x count-raiders-deployed.",
 (16,3):"increase-role-with-cost :leader twice.",
 (16,4):"+1 :tools, +3 amity (place-demands-then-sell approximation).",
 (17,1):"Flip the first :raiding raider to :point. PARTIAL: no placement step. Human path = with-choice [17 1] pick-resource (note: choice arm grants a resource, see divergence).",
 (17,2):"For each magistrate-city with temple supply: place a :face-down temple, dec supply. PARTIAL.",
 (17,3):"If every route adjacent to :uruk holds your raider: +8 amity. Else no-op. PARTIAL.",
 (17,4):"+glory = merchant level. PARTIAL: no sell.",
 (18,1):"+2 :tools. PARTIAL: no magistrate move / sell. Human path = with-choice [18 1] move-magistrate.",
 (18,2):"If :samarra temple is :face-down: +5 glory; else +2 glory (partial fallback).",
 (18,3):"If every route adjacent to :kish holds your raider: +6 amity. Else no-op.",
 (18,4):"+amity = 4 x (count of :point raiders); then remove those raiders and refund supply (QA lesson 8 'score then remove').",
 (19,1):"increase-role-with-cost :priest twice.",
 (19,2):"+1 :pottery, +3 amity (sell-to-pottery-cities approximation).",
 (19,3):"Discard first held good, +3 glory. Human path = with-choice [19 3] pick-resource-to-discard then influence+sell at caravan.",
 (19,4):"Set every held raider to :point.",
 (20,1):"Place raiding raiders on up to 2 free routes from current :caravan city.",
 (20,2):"increase-role-with-cost :merchant twice.",
 (20,3):"+amity = leader level. PARTIAL: no influence step. Human path = with-choice [20 3] influence+sell.",
 (20,4):"+1 :tools, +1 :gold (take-from-astronomer-spaces approximation).",
 (21,1):"Set :caravan :eridu. Human path = with-choice [21 1] travel anywhere.",
 (21,2):"increase-role-with-cost :raider, then :leader.",
 (21,3):"Set :caravan :eridu. PARTIAL: no sell. Human path = with-choice [21 3] travel+sell.",
 (21,4):"+glory = count of held demand-tokens.",
 (22,1):"increase-role-with-cost :raider, then :merchant.",
 (22,2):"+amity = count-face-down-temples (demands-on-facedown approximation).",
 (22,3):"+1 :pottery. Human path = with-choice [22 3] pick-resource.",
 (22,4):"+amity = 2 x count-raiders-deployed. PARTIAL: no travel. Human path = with-choice [22 4] travel.",
 (23,1):"increase-role-with-cost :priest, then :merchant.",
 (23,2):"Set :caravan :eridu, +4 amity (sell twice approximation).",
 (23,3):"+1 :tools, increase-role-with-cost :merchant. Human path = with-choice [23 3] pick-resource.",
 (23,4):"Pick magistrate-city without your temple; place-temple-in. AUTO; human path = with-choice [23 4].",
 (24,1):"increase-role-with-cost :raider, then :leader.",
 (24,2):"+2 glory (demands-on-magistrates approximation).",
 (24,3):"+glory = count of held demand-tokens.",
 (24,4):"For up to 2 held demand-tokens: +1 of each matching resource.",
 (25,1):"+glory = 2 + count of :point raiders (influence+score approximation). Human path = with-choice [25 1] influence.",
 (25,2):"increase-role-with-cost :merchant, then :leader.",
 (25,3):"Flip up to 2 :face-up temples to :face-down, +1 amity each.",
 (25,4):"+1 :gems. Human path = with-choice [25 4] pick-resource.",
 (26,1):"increase-role-with-cost :priest, then :leader.",
 (26,2):"increase-role-with-cost :priest, then :raider.",
 (26,3):"+2 amity, place-temple-in at :caravan.",
 (26,4):"Place a raiding raider on any free route, +2 amity.",
 (27,1):"+3 amity (travel+sell approximation). Human path = with-choice [27 1] travel+sell.",
 (27,2):"Place a raiding raider on any free route. Human path = with-choice [27 2] travel+deploy.",
 (27,3):"place-temple-in at :caravan. Human path = with-choice [27 3] travel+temple.",
 (27,4):"+1 each :tools, :gold, :gems (three goods). Human path = with-choice [27 4] pick-resource x3.",
 (28,1):"place-temple-in at :caravan. Human path = with-choice [28 1] travel+temple.",
 (28,2):"place-temple-in at :caravan. Human path = with-choice [28 2] travel+temple.",
 (28,3):"If gold>0 dec gold; +4 amity (sell-gold-to-empty-city approximation).",
 (28,4):"Place a raiding raider on a free route touching :kish, then flip it to :point.",
 (29,1):"If leader>1: dec :leader, increase-role-free :merchant and :priest. Else no-op.",
 (29,2):"+3 amity (travel+sell approximation). Human path = with-choice [29 2] travel+sell.",
 (29,3):"Place raiding raiders on up to 3 free river routes.",
 (29,4):"place-temple-in at :caravan (surrounded-cities approximation).",
 (30,1):"+glory = leader level. PARTIAL: no influence+travel. Human path = with-choice [30 1] travel.",
 (30,2):"+amity = leader level. PARTIAL: no influence+sell. Human path = with-choice [30 2] influence+sell.",
 (30,3):"+glory = raider level. PARTIAL: no deploy+influence. Human path = with-choice [30 3] influence+deploy.",
 (30,4):"+amity = priest level. PARTIAL: no influence+temple. Human path = with-choice [30 4] influence+temple.",
 (31,1):"For each role at level 1: set to 2.",
 (31,2):"For each role at level 3: increase-role-with-cost.",
 (31,3):"+1 :gems; if a :face-up temple exists, flip it :face-down, +2 amity. Human path = with-choice [31 3] pick-resource.",
 (31,4):"+1 :tools; place a raiding raider on any free route. Human path = with-choice [31 4] pick-resource.",
 (32,1):"+glory = count of held demand-tokens (sell+glory-per-demand approximation).",
 (32,2):"+1 :gems. PARTIAL: no travel. Human path = with-choice [32 2] travel.",
 (32,3):"Place a raiding raider on a free route between two of your temple cities.",
 (32,4):"+2 amity, +2 glory (influence+sell approximation). Human path = with-choice [32 4] influence+sell.",
 (33,1):"If merchant>1: dec :merchant, increase-role-free :raider and :priest. Else no-op.",
 (33,2):"Flip first :face-up temple to :face-down, +3 amity; if none, +1 amity. Human path = with-choice [33 2] travel.",
 (33,3):"place-temple-in :uruk face-up.",
 (33,4):"Place a raiding raider on any free route. Human path = with-choice [33 4] travel+deploy.",
 (34,1):"Place raiders on up to min(tools,available,2) free routes touching :uruk, paying 1 tool each.",
 (34,2):"Place raiding raiders on up to 2 free routes (each existing route approximation).",
 (34,3):"AUTO: +amity = count of cities that are magistrate AND hold your temple. Human path = with-choice [34 3] multi-pick sell, no travel.",
 (34,4):"Same as [34 3] (duplicate of #3 per status comment '34-4 same as 34-3').",
 (35,1):"+3 amity (travel+sell approximation). Human path = with-choice [35 1] travel+sell.",
 (35,2):"Pay up to 2 :pottery, place that many temples in distinct free cities.",
 (35,3):"increase-role-with-cost on the lowest-level role. Human path = with-choice [35 3] pick-role.",
 (35,4):"+glory = 2 + count of :point raiders (influence+score approximation). Human path = with-choice [35 4] pick-role / score.",
}

BOARD_NAMES = {
 1:"Shield of Gilgamesh",2:"Seal of Enmerkar",3:"Voyage of Ziusudra",4:"Blessing of Inanna",
 5:"Wisdom of Adapa",6:"Trade of Dumuzid",7:"March of Lugalbanda",8:"Fury of Enkidu",
 9:"Rites of Ninhursag",10:"Wealth of Meskalamdug",11:"Ambition of Sargon",12:"Currents of Enki",
 13:"Pillars of Etana",14:"Roads of Shulgi",15:"Ascent of Ur-Nammu",16:"Dominion of Hammurabi",
 17:"Cunning of Kubaba",18:"Forge of Tubal-Cain",19:"Kilns of Ninkasi",20:"Vision of Rimush",
 21:"Legacy of Eannatum",22:"Strategy of Naram-Sin",23:"Market of Puabi",24:"Siege of Shulme",
 25:"Command of Mesannepada",26:"Court of Enshakushanna",27:"Path of Alulim",28:"Stars of Sin-Kashid",
 29:"Treasury of Ibbi-Sin",30:"Council of Amar-Sin",31:"Horizon of Sharkalisharri",32:"Jewel of Ku-Bau",
 33:"Vanguard of Enmebaragesi",34:"Honor of Agga",35:"Wanderer of Dumuzi",
}

# ── Emit ──────────────────────────────────────────────────────────────────────
STATUS_LABEL = {
 "persistent":"PERSISTENT (passive)","implemented":"IMPLEMENTED","partial":"PARTIAL",
}

def slot_name(s):
    return "Slot 0 (passive)" if s == 0 else f"Slot {s} (player-facing #{s})"

out = []
W = out.append

# Header / preamble
W("# Bonus-board code-path walkthrough suite")
W("")
W("**Scope:** all 35 bonus boards x 5 slots = **175 effects**. Slot 0 is the persistent")
W("passive; slots 1-4 are the instant bonus slots. Read/document only — no code changes.")
W("")
W("Sources: `src/cljc/eridu/bonus.cljc` (effect-implementation-status, bonus-needs-choice?)")
W("and `src/cljc/eridu/game.cljc` (apply-passive / apply-passive-dispatch,")
W("apply-bonus-effect / apply-bonus-dispatch, apply-bonus-with-choice).")
W("")
W("**Entry points**")
W("- Instant slots 1-4 enter through `apply-bonus-effect` -> `apply-bonus-dispatch`")
W("  (keyed by `[board-id slot-idx]`). If `bonus-needs-choice?` returns a descriptor,")
W("  the human/UI path instead routes through `apply-bonus-with-choice` once the player")
W("  picks; the dispatch arm is the bot/no-UI auto-resolve fallback.")
W("- Passive slot 0 enters through `apply-passive` -> `apply-passive-dispatch`")
W("  (keyed by `[board-id trigger-type]`), gated by `has-passive?`. Some passives stash")
W("  `:passive-choice-needed` resolved later by `apply-passive-choice`.")
W("")

# honesty tally
counts = {}
for v in STATUS.values():
    counts[v] = counts.get(v, 0) + 1
W("**Honesty tally (all 175 slots):** " +
  ", ".join(f"{k}={counts.get(k,0)}" for k in ["implemented","partial","persistent"]) +
  f"  (total={sum(counts.values())})")
W("")
W("> **Slot-5 bug cross-reference.** The in-game report used \"slot 5\" but the code")
W("> indexes slots 0-4. Player-facing slot N = code index N-1, so \"slot 5\" = code")
W("> index 4 (the 4th bonus slot). Each board's **Slot 4** entry below is the one the")
W("> slot-5 bug task maps to; see the per-slot dispatch arm `[board 4]`.")
W("")
W("---")
W("")

n = 0
for b in range(1, 36):
    W(f"# Board {b} — {BOARD_NAMES[b]}")
    W("")
    for s in range(0, 5):
        n += 1
        status = STATUS[(b, s)]
        intent = INTENT[(b, s)]
        W(f"## {n}. Board {b} / {slot_name(s)} — [{STATUS_LABEL[status]}]")
        W("")
        W(f"- **Printed intent:** {intent}")
        if s == 0:
            trig, where, beh = PASSIVE[b]
            W(f"- **Entry point:** `apply-passive` -> `apply-passive-dispatch` case `[{b} {trig}]` (gated by `has-passive?`).")
            W(f"- **Trigger:** `{trig}` — fires {where}.")
            W(f"- **Code path / outcome:** {beh}")
            # divergence
            if "NO-OP" in beh:
                W(f"- **Honesty / GAP:** classified `:persistent` but the passive arm is a **no-op** — printed effect is NOT applied. Flag for review.")
            elif ":passive-choice-needed" in beh:
                W(f"- **Honesty:** `:persistent`; uses a deferred choice resolved in `apply-passive-choice` (board {b}).")
            else:
                W(f"- **Honesty:** `:persistent` — tracked + applied on its trigger (not an instant slot).")
        else:
            ch = CHOICE.get((b, s))
            beh = BEHAVIOR[(b, s)]
            if ch:
                ctype, prompt, extra = ch
                ep = (f"`bonus-needs-choice?` returns `{{:type :{ctype}}}` -> human path "
                      f"`apply-bonus-with-choice [{b} {s}]`; bot fallback `apply-bonus-dispatch [{b} {s}]`.")
                W(f"- **Entry point:** {ep}")
                W(f"- **Choice:** :{ctype} — \"{prompt}\"" + (f" ({extra})" if extra else ""))
            else:
                W(f"- **Entry point:** auto-resolve `apply-bonus-dispatch [{b} {s}]` (no choice descriptor).")
            W(f"- **Code path / outcome:** {beh}")
            if status == "partial":
                W(f"- **Honesty / GAP:** `:partial` — code does something but diverges from printed text. {('See note in code path.' )}")
            else:
                W(f"- **Honesty:** `:implemented`.")
        if s == 4:
            W(f"- **Slot-5 bug link:** this slot (code index 4) is the \"slot 5\" referenced by the separate bug task.")
        W("")
    W("---")
    W("")

import os
path = os.path.expanduser("~/organism/bonus-walkthrough.md")
with open(path, "w") as f:
    f.write("\n".join(out))
print(f"Wrote {path}: {n} slot entries, {len(out)} lines")
print("Honesty tally:", counts)
