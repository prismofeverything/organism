# Journeymen E — How to Play

> Playtest rules, 2026-07-03 (rev 2 — green-inherited mechanics restored). Canonical design
> record: `journeymen-e.md`. Rules marked ⚑ are provisional numbers under active tuning.

## The pitch
You run a craft guild in a medieval market town. Your two carts circle the town's eight
districts **clockwise**, gathering workers, crafting goods, and selling them at your own
market stalls. Selling feeds the town's **sale tracks** — the shared clock — and master
artisans **display mastercrafts** that earn dividends every time their track advances.
When two tracks fill, the season ends.

## Components
- **8 locations** — Academy, Docks, Parks, Temple, Manor, Barracks, Castle, Tavern — on the
  6-wedge **two-track rondel** (outer + inner tracks, switch wedges join them, one wedge
  crosses). Wedge order is shuffled each game — the only setup randomness. ⚑ The
  action↔demand pairing per location awaits Mohammad's re-pairing. 2p/3p games seed
  **blocked shop slots** (12 / 4, ≤2 per space).
- **Workers**: 4 colors (black/blue/yellow/red) + white (apprentice) + grey (out-of-guild),
  4 each, seeded on districts. Plus a **skilled worker** reserve (★, 2 waves of one per color).
- **Goods**: Durability ⛏, Precision ⚙, Innovation 💡 (worth points) and Luxury 💎 (worth
  coins). Market pool: 10/10/10/12 — crafting TAKES from the pool, selling RETURNS (all but
  the tracked copy).
- **Per player**: guild board (skill grid with ◆ medallions, passive + 3 atelier abilities,
  **recipe slot board**: 4 seeded base recipes + 4 acquirable slots), 6 basic shops, 3
  ateliers, 2 carts, coins (**2 + seat number** — turn-order compensation, as green).
- **Recipe deck**: eras A → B → C (green's real cards); face-up market of 4.
- **Master pool**: 14 masterwork tokens with worker costs — a shared race.
- **4 sale tracks**: thresholds ⛏5 ⚙5 💡5 💎6, each with +2 overfill slots.

## Every district = an ACTION + a DEMAND
You can only **SELL** a district's demand where **you have a shop** (your market stall).
Demands: the 6 ordered pairs of ⛏/⚙/💡 (required good first, then the optional may ride
along), plus 2 Luxury districts (💎 required, up to 1 each of the others optional).
Actions: 3× **place shop** (each also grants a color-limited worker pickup —
red/yellow, black/blue, or white/grey), 2× **upgrade to atelier** (one points-leaning,
one coins-leaning), 2× **take recipe** (one pays 2 coins ⚑, one pays a worker of your
choice), 1× **claim a skilled worker**.

## Your turn
1. **MOVE** one of your carts **1 or 2 roads forward** along the directed two-track rondel
   (no backtracking; switch wedges let you change tracks).
2. **PICK UP** one worker pooled on your district (free), or on a neighbouring district for
   1 coin, or skip.
3. **MAIN** — ONE of: **sell** the district's demand (needs your shop here) · **display** a
   built mastercraft (needs your atelier here) · the district's **action** · skip.
4. **CRAFT** anywhere — pay a recipe's workers, take its goods from the market pool (spent
   workers drop on your district) — or **build a mastercraft**, or end your turn.

## Selling & the clock
- Required good first; payout per token = your **grid value** at the current rank
  (⛏⚙💡 → points, 💎 → coins); each good TYPE sold levels its skill once (max r6).
- **One sold token — your choice — goes on its sale track** and leaves circulation; the rest
  return to the pool. When the **2nd track reaches its threshold: finish the round, then play
  one more full round.** A full track still accepts +2 overfill, then sales stop tracking.

## Shops, ateliers, abilities
- A basic shop costs **coins equal to the pieces you already have on the board** (min 1);
  max 4 basics per space, minus any blocked slots. **Both** setup cart placements bring a
  free starting shop (room permitting), as green.
- An **atelier** (flat **4 coins**) swaps in for one of your board basics, still counts as a
  placed shop, pays the action district's bonus, and **unlocks ONE of your guild's three
  atelier abilities — your choice**. Every guild also has an always-on **passive**.
- Ateliers are where you **DISPLAY** mastercrafts.

## Mastercrafts — the dividend race
1. Climb your skill grid to a **◆ medallion** → gain a **master-recipe claim**.
2. **Build**: spend a claim + pay the worker cost of a **specific token from the shared
   master pool** (14 exist — claiming removes it for everyone).
3. **Display** it at your atelier (instead of selling) onto a chosen sale track: score its
   grid value now, **pay each mastercraft already on that track its +1★+1🪙 dividend
   immediately**, then earn **+1 point +1 coin every time ANY token lands there** (stacking).

## Skilled workers (★)
Claim from the reserve at the skilled district; a ★ worker pays as its color, and when
SPENT it drops still marked — and you immediately grab a matching regular worker there.
When the reserve empties, a second (final) wave opens.

## Recipes — the slot board
Your board holds **4 base recipes** (seeded) and **4 acquirable slots**; taking a market
recipe covers a **slot of your choice**. Craft by slot, paying a **chosen worker multiset**
(spending a ★ skilled worker is a deliberate timing choice — its spend-grab fires where it
drops). Flags: 🚫 one-time (empties its slot) · ⏫ bonus skill · ⤒ raise lowest skill ·
🏠 place a free shop · 🔗 **chain** — never crafted alone; when you craft the recipe to its
**LEFT**, the chain is offered (pay its own cost, fire or decline, cascades rightward).

## Winning
Most **points** when the season ends. Coins are the engine, not the score.

## Parks coin-free shop action (2026-07-04, corrected)
**The Parks ACTION** (cart parked at Parks) pays **2 grey workers** and places a basic shop
**ANYWHERE** with room — coin-free, flat regardless of shop count. The 2 grey drop on Parks and
recycle. It's simply the grey-paid version of the shop action (Docks/Barracks place anywhere for
the coin ramp) — a coin-free backstop so coin-starvation doesn't force a luxury stall. (Keeps
Academy/Parks — both innovation-required — deliberately asymmetric; a Temple-atelier coin-relief
is under consideration.)
