/* Chroma — shared-path invariant suite (node, no deps).
 *
 * The Eridu/Chroma unification contract: humans and bots traverse ONE engine.
 * The bot is a PURE weighted selector over the SAME presented choices the human
 * picks from; the SINGLE mutator (applyPlacement) removes the placed chit from
 * hand AND pushes it onto the stack — for everyone. The bug we fixed was a
 * second, parallel bot path that pushed to the board but never spliced the hand.
 *
 * These tests pin that contract so it cannot silently come back:
 *   A. core conservation  — bag + Σhands + Σboard + 2·swaps is invariant
 *                           (the law the old bot path violated: +1 every bot turn)
 *   B. hand never grows    — a hand can only shrink (dry/swap); never exceed START_HAND
 *   C. identical move set   — decide only ever returns a member of enumerateMoves
 *                            (the same set the human UI is shown)
 *   D. one apply path       — the SAME move applied for a "human" seat vs a "bot"
 *                            seat produces identical hand/board deltas
 *   E. context-responsive   — changing the genome (weights) changes decide's pick
 *                            (proves it's a weighted scorer, not a flat/fixed one)
 *   F. adversarial pokes    — a hoarder genome still can't inflate its hand;
 *                            a mud-rusher still conserves
 *   G. live MOCK            — drive chroma-mock.html headlessly (DOM stubbed) and
 *                            assert the same conservation + hand-bound invariants
 *                            on the real UI turn loop (human + bots).
 */
"use strict";
const fs = require("fs");
const C = require("./chroma-core.js");
C.setPalette("CMY"); C.setDepth(3);

let pass = 0, fail = 0;
const ok = (cond, msg) => { if (cond) { pass++; } else { fail++; console.error("  ✗ FAIL:", msg); } };
const section = (s) => console.log("\n== " + s + " ==");

const colors = () => C.PAL.order;
const boardCount = (G) => Object.keys(G.stacks).reduce((a, k) => a + G.stacks[k].length, 0);
const handCount = (G) => G.players.reduce((a, p) => a + p.hand.length, 0);
const discardCount = (G) => G.players.reduce((a, p) => a + (p.discarded || 0), 0);
// chits that LEFT PLAY via swap discards (1 for an upgraded/mudded swap, 2 for a
// normal swap) — so the ledger is invariant regardless of swap type.
const ledger = (G) => C.bagTotal(G.bag) + handCount(G) + boardCount(G) + discardCount(G);
// The conservation constant = (6×30 palette chits) + the immutable non-palette
// seeds on the board (the black "K" center). Measure it from a fresh game so the
// law is "ledger is invariant", not a hand-derived magic number.
const TOTAL = ledger(C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], 999999));
console.log("conservation constant (measured):", TOTAL, "=", C.PER_COLOR * colors().length, "palette chits + non-palette seeds");

function playToEnd(G, perTurn) {
  let guard = 0;
  while (!G.over && guard++ < 500) { C.step(G, false); if (perTurn) perTurn(G); }
  return G;
}

// ─────────────────────────────────────────────────────────────────────────────
section("A. conservation law (bag + hands + board + 2·swaps == 180), all-bot games");
for (let seed = 0; seed < 60; seed++) {
  const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], seed);
  ok(ledger(G) === TOTAL, `seed ${seed}: initial ledger ${ledger(G)} != ${TOTAL}`);
  let bad = null;
  playToEnd(G, g => { if (ledger(g) !== TOTAL && bad === null) bad = ledger(g); });
  ok(bad === null, `seed ${seed}: ledger drifted to ${bad} mid-game (was ${TOTAL})`);
}

section("B. a hand can only shrink — never exceeds START_HAND (the anti-bug check)");
for (let seed = 100; seed < 140; seed++) {
  const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], seed);
  let over = null;
  playToEnd(G, g => { g.players.forEach(p => { if (p.hand.length > C.START_HAND && over === null) over = p.hand.length; }); });
  ok(over === null, `seed ${seed}: a hand grew to ${over} (> START_HAND ${C.START_HAND}) — the bug signature`);
}

section("C. decide only ever returns a move from enumerateMoves (the presented set)");
{
  let checks = 0, leaks = 0;
  for (let seed = 200; seed < 215; seed++) {
    const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], seed);
    let guard = 0;
    while (!G.over && guard++ < 500) {
      for (let i = 0; i < G.N; i++) {
        const moves = C.enumerateMoves(G, i);
        const d = C.decide(G, i);
        if (d) {
          checks++;
          const inSet = moves.some(m => m.c[0] === d.c[0] && m.c[1] === d.c[1] && m.chit === d.chit);
          if (!inSet) leaks++;
        }
        if (d) C.applyPlacement(G, i, d);   // advance so later turns are real
      }
      C.resolveDraws(G, []);                 // already applied; just advances dry bookkeeping cleanly
      G.turn++;
      if (G.ending && G.turn > G.finalTurn) G.over = true;
      const md = Math.max(...G.players.map(p => p.dry));
      if (!G.ending && (md >= C.MUD_LIMIT || C.bagTotal(G.bag) === 0)) { G.ending = true; G.finalTurn = G.turn; }
    }
  }
  ok(checks > 0, "observed decide() picks");
  ok(leaks === 0, `${leaks}/${checks} decide() picks were NOT in enumerateMoves (path divergence)`);
}

section("D. one apply path — same move, 'human' seat vs 'bot' seat, identical deltas");
{
  // build two identical games; in one, apply a chosen move as seat 0 (human-style);
  // in the other, the same physical move as seat 0 too but via the bot's own pick —
  // here we assert the MUTATOR is seat-agnostic: applying move M for any pi splices
  // exactly that chit and pushes exactly once.
  const G = C.newGame([{ isBot: false }, { isBot: true }, { isBot: true }], 7);
  const pi = 0, hand0 = G.players[pi].hand.slice();
  const mv = C.enumerateMoves(G, pi)[0];
  const before = { hand: G.players[pi].hand.length, board: G.stacks[mv.k].length };
  const rec = C.applyPlacement(G, pi, mv);
  const after = { hand: G.players[pi].hand.length, board: G.stacks[mv.k].length };
  ok(after.hand === before.hand - 1, "applyPlacement removed exactly one chit from hand");
  ok(after.board === before.board + 1, "applyPlacement pushed exactly one chit to the stack");
  ok(rec.color === mv.chit, "the chit placed is the chit chosen");
  // the placed chit is the one removed (multiset check)
  const removed = hand0.slice(); removed.splice(removed.indexOf(mv.chit), 1);
  ok(JSON.stringify(removed.slice().sort()) === JSON.stringify(G.players[pi].hand.slice().sort()),
     "hand after == hand before minus exactly the placed chit");
}

section("E. context-responsiveness — different genomes pick differently on a fixed state");
{
  // A target-hungry genome vs a blank-canvas-hungry genome should, somewhere in a
  // game, diverge on choice — proving weights actually drive `decide`.
  let diverged = 0, compared = 0;
  for (let seed = 300; seed < 340; seed++) {
    const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], seed);
    let guard = 0;
    while (!G.over && guard++ < 60) {
      const gTarget = Object.assign(C.defaultGenome(), { targetDraw: 3, growRegion: 2, blankPriority: -1, edgeCenterPref: 0 });
      const gBlank = Object.assign(C.defaultGenome(), { targetDraw: 0, growRegion: 0, blankPriority: 2, edgeCenterPref: 0 });
      const saveRng = G.rng;
      // freeze rng so the only difference is the genome (no jitter)
      G.rng = () => 0;
      G.players[0].g = gTarget; const a = C.decide(G, 0);
      G.players[0].g = gBlank; const b = C.decide(G, 0);
      G.rng = saveRng;
      if (a && b) { compared++; if (!(a.c[0] === b.c[0] && a.c[1] === b.c[1] && a.chit === b.chit)) diverged++; }
      C.step(G, false);
    }
  }
  ok(compared > 0, "compared genome picks on real states");
  ok(diverged > 0, `genomes never diverged over ${compared} states — weights may be inert`);
}

section("F. adversarial pokes — pathological genomes still obey conservation + hand bound");
{
  const hoarder = Object.assign(C.defaultGenome(), { spendTargetPen: 3, anyDraw: 0, mudAversion: 3, dryAversion: 3 });
  const mudrush = Object.assign(C.defaultGenome(), { mudRush: 2, mudAversion: 0, anyDraw: 0, dryAversion: 0 });
  const dumper = Object.assign(C.defaultGenome(), { anyDraw: 0, dryAversion: 0, mudAversion: 0, spendTargetPen: 0 });
  for (const [name, trio] of [["hoarder", [hoarder, hoarder, hoarder]], ["mudrush", [mudrush, dumper, hoarder]]]) {
    for (let seed = 400; seed < 430; seed++) {
      const G = C.newGame(trio.map(g => ({ isBot: true, g })), seed);
      let bad = null, grew = null;
      playToEnd(G, g => {
        if (ledger(g) !== TOTAL && bad === null) bad = ledger(g);
        g.players.forEach(p => { if (p.hand.length > C.START_HAND && grew === null) grew = p.hand.length; });
      });
      ok(bad === null, `${name} seed ${seed}: ledger drifted to ${bad}`);
      ok(grew === null, `${name} seed ${seed}: hand grew to ${grew}`);
    }
  }
}

// ─────────────────────────────────────────────────────────────────────────────
section("G. LIVE MOCK — drive chroma-mock.html headlessly; same invariants on the UI loop");
{
  // Minimal DOM stub: every element is a Proxy whose methods are no-ops.
  const fakeEl = () => new Proxy({ textContent: "", innerHTML: "", value: "", style: {}, firstChild: null, scrollTop: 0, scrollHeight: 0 },
    { get(t, p) { if (p in t) return t[p]; if (p === "classList") return { toggle() {}, add() {}, remove() {} }; return () => {}; },
      set(t, p, v) { t[p] = v; return true; } });
  global.window = { Chroma: C };
  global.document = { getElementById: () => fakeEl(), createElement: () => fakeEl(), createElementNS: () => fakeEl(), createTextNode: () => fakeEl() };
  global.localStorage = { getItem: () => null, setItem() {} };
  global.fetch = () => Promise.reject(new Error("headless")); // saveGame falls into its .catch (just logs)
  global.Blob = function () {}; global.URL = { createObjectURL: () => "", revokeObjectURL() {} };

  const html = fs.readFileSync("chroma-mock.html", "utf8");
  const m = html.match(/<script>\n([\s\S]*?)<\/script>/);
  ok(!!m, "extracted the mock's inline <script>");

  // Static guard: the mock must NOT carry a second engine (the divergence smell).
  const body = m[1];
  ok(!/function\s+applyPlacement\s*\(/.test(body), "mock has NO local applyPlacement (uses C.applyPlacement)");
  ok(!/function\s+resolveDraws\s*\(/.test(body), "mock has NO local resolveDraws (uses C.resolveDraws)");
  ok(!/function\s+botDecide\s*\(/.test(body), "mock has NO local botDecide (uses C.decide)");
  ok(!/players\[0\]\.hand\.splice/.test(body), "mock human path has NO separate hand splice (the old bug)");

  // Driver appended into the SAME scope so it can reach the mock's functions.
  const driver = `
;(function(){
  globalThis.__checks = [];
  promptSwap = function(){ chooseSwap(null); };   // auto-skip the human swap prompt
  globalThis.__runMockGame = function(){
    newGame();
    let guard = 0;
    const TOTAL = ${TOTAL};
    while (!over && guard++ < 600) {
      if (awaitingSwap) { chooseSwap(null); continue; }
      const G = gameG();
      const mv = C.enumerateMoves(G, 0);
      if (!mv.length) { pass(); }
      else { sel = players[0].hand.indexOf(mv[0].chit); humanMove(mv[0].c); }
      const board = Object.keys(stacks).reduce((a,k)=>a+stacks[k].length,0);
      const hands = players.reduce((a,p)=>a+p.hand.length,0);
      const discarded = players.reduce((a,p)=>a+(p.discarded||0),0);
      const maxHand = Math.max.apply(null, players.map(p=>p.hand.length));
      __checks.push({ turn, ledger: bagTotal()+hands+board+discarded, maxHand });
    }
  };
})();
`;
  try {
    (0, eval)(body + driver);
    for (let g = 0; g < 8; g++) {
      globalThis.__checks = [];
      globalThis.__runMockGame();
      const chk = globalThis.__checks;
      ok(chk.length > 0, `mock game ${g}: produced turns`);
      const drift = chk.find(c => c.ledger !== TOTAL);
      ok(!drift, `mock game ${g}: ledger drifted to ${drift && drift.ledger} (turn ${drift && drift.turn}) — expected ${TOTAL}`);
      const grew = chk.find(c => c.maxHand > C.START_HAND);
      ok(!grew, `mock game ${g}: a hand grew to ${grew && grew.maxHand} (> ${C.START_HAND}) — the bug signature`);
    }
  } catch (e) {
    ok(false, "mock eval/run threw: " + e.message + "\n" + (e.stack || ""));
  }
}

// ─────────────────────────────────────────────────────────────────────────────
C.setRemoved([]);   // reset: the headless mock-drive (§G) set a global trim; later sections want the full board
section("H. op2b scoring — contested color goes to the MOST-chits holder only");
{
  // Paint a board with known structure: separate single-colour blobs (and TWO
  // blue blobs so a tie pays the 2nd-largest). Then reproduce the user's hands
  // (You=R,G · B1=R,R,C,B · B2=R,R,R,B) and assert ownership, not double-counting.
  const board = {}; C.cells.forEach(c => { board[C.key(c[0], c[1])] = []; });
  const used = new Set([C.key(0, 0)]);              // leave the center alone
  function grow(seed, size, color) {                // connected blob of `color`
    const sk = C.key(seed[0], seed[1]); if (used.has(sk) || !C.cellSet[sk]) return 0;
    const front = [seed], inF = new Set([sk]); let n = 0;
    while (n < size && front.length) {
      const cur = front.shift(), ck = C.key(cur[0], cur[1]);
      if (used.has(ck)) continue;
      used.add(ck); board[ck] = [color]; n++;
      for (const d of C.NEI) { const nb = [cur[0] + d[0], cur[1] + d[1]], nk = C.key(nb[0], nb[1]); if (C.cellSet[nk] && !used.has(nk) && !inF.has(nk)) { inF.add(nk); front.push(nb); } }
    }
    return n;
  }
  grow([5, 0], 8, "G");        // Green blob (You will solely own it)
  grow([-5, 5], 5, "R");       // Red blob (B2 will solely own it)
  grow([0, -5], 3, "C");       // Cyan blob (B1 will solely own it)
  grow([5, -5], 4, "B");       // Blue blob A
  grow([-5, 0], 2, "B");       // Blue blob B (far corner -> stays separate -> 2nd blue region)
  const reg = C.largestRegions(board), two = C.largestTwo(board);
  ok(reg.G > 0 && reg.R > 0 && reg.C > 0, `setup: G/R/C regions exist (G${reg.G} R${reg.R} C${reg.C})`);
  ok(two.second.B > 0, `setup: blue has a 2nd region for the tie payout (second.B=${two.second.B})`);

  const players = [
    { hand: ["R", "G"], dry: 0, swaps: 0, target: null },         // You
    { hand: ["R", "R", "C", "B"], dry: 0, swaps: 0, target: "R" }, // B1
    { hand: ["R", "R", "R", "B"], dry: 0, swaps: 0, target: "R" }, // B2
  ];
  const s = C.scoreGame(players, board);
  // Red: B2 has 3 > B1's 2 > You's 1 -> B2 sole owner.
  // Green: You sole holder. Cyan: B1 sole holder. Blue: B1 & B2 tie (1 each) -> each gets 2nd-largest blue region.
  ok(s[0].mult === reg.G, `You scores ONLY Green (${s[0].mult} should == reg.G ${reg.G}); must NOT include Red`);
  ok(s[1].mult === reg.C + two.second.B, `B1 scores Cyan + blue-tie-2nd (${s[1].mult} == ${reg.C}+${two.second.B})`);
  ok(s[2].mult === reg.R + two.second.B, `B2 scores Red + blue-tie-2nd (${s[2].mult} == ${reg.R}+${two.second.B})`);
  ok(s[0].mult !== reg.G + reg.R, "the bug is gone: You does NOT also score Red");
  ok(s[1].mult < reg.R + reg.C + two.first.B, "the bug is gone: B1 does NOT score Red, nor the largest blue");
}

section("I. op2b never over-counts vs the old base rule, and genuinely differs somewhere");
{
  let everLess = false, violations = 0;
  for (let seed = 500; seed < 560; seed++) {
    const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], seed);
    playToEnd(G);
    const reg = C.largestRegions(G.stacks);
    const op2b = C.scoreGame(G.players, G.stacks);
    G.players.forEach((p, i) => {
      const base = C.scoreHand(p.hand, reg).mult;     // old rule: every held colour, full region
      const region = op2b[i].regionScore;              // op2b region points (excl. the mud bonus)
      if (region > base) violations++;                 // op2b can only restrict who scores -> never higher
      if (region < base) everLess = true;
    });
  }
  ok(violations === 0, `op2b score exceeded base in ${violations} cases (op2b must be ≤ base — it only removes colours)`);
  ok(everLess, "op2b differs from base somewhere (the rule actually changed scoring, not a no-op)");
}

section("J. no-same-color-as-last-turn rule (with monochrome fallback)");
{
  let checks = 0, violations = 0, repeats = 0;
  for (let seed = 600; seed < 660; seed++) {
    const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], seed);
    const prev = [null, null, null];
    let guard = 0;
    while (!G.over && guard++ < 500) {
      const handsBefore = G.players.map(p => new Set(p.hand));   // distinct colors at turn start
      const plc = [];
      for (let i = 0; i < G.N; i++) {
        const d = C.decide(G, i);
        if (d) {
          checks++;
          // no fallback now: a placement may NEVER repeat the player's previous
          // placement color. (All-forbidden hands produce d===null -> a skip.)
          if (prev[i] !== null && d.chit === prev[i]) { repeats++; violations++; }
          handsBefore;  // (retained for clarity; no longer needed without the fallback)
          plc.push(C.applyPlacement(G, i, d));
          prev[i] = d.chit;
        }
        // a skip (d===null) does NOT update prev[i] — you stay barred from that
        // color until you actually place a different one (after a swap).
      }
      C.resolveDraws(G, plc); G.turn++;
      if (G.ending && G.turn > G.finalTurn) G.over = true;
      const md = Math.max(...G.players.map(p => p.dry));
      if (!G.ending && (md >= C.MUD_LIMIT || C.bagTotal(G.bag) === 0)) { G.ending = true; G.finalTurn = G.turn; }
    }
  }
  ok(checks > 0, "observed placements");
  ok(violations === 0, `${violations} placements repeated last turn's color (must be 0 — the rule has no fallback)`);
}

section("K. mud scoring v2 — fewest-hand player scores ALL mud; tie -> largest mud region");
{
  // Board with TWO contiguous mud cells ([5,0]+[4,0]) + two isolated mud cells
  // ([-5,5], [0,-5]) -> total 4 mud, largest contiguous region 2. Rest white.
  const board = {}; C.cells.forEach(c => { board[C.key(c[0], c[1])] = []; });
  [[5, 0], [4, 0], [-5, 5], [0, -5]].forEach(c => { board[C.key(c[0], c[1])] = ["C", "M", "Y"]; });
  const mr = C.mudRegions(board);
  ok(mr.total === 4, `setup: total mud 4 (got ${mr.total})`);
  ok(mr.largest === 2, `setup: largest contiguous mud region 2 (got ${mr.largest})`);
  // colors held have no regions on this board -> regionScore 0 -> mult == mudScore
  // SOLE fewest: You (2) < B1 (4) < B2 (5) -> You scores all 4 mud, others 0
  const sole = C.scoreGame([
    { hand: ["R", "G"], dry: 0, swaps: 0, target: null },
    { hand: ["R", "G", "B", "C"], dry: 0, swaps: 0, target: null },
    { hand: ["R", "G", "B", "C", "M"], dry: 0, swaps: 0, target: null },
  ], board);
  ok(sole[0].mudScore === 4, `sole fewest hand scores ALL mud =4 (got ${sole[0].mudScore})`);
  ok(sole[1].mudScore === 0 && sole[2].mudScore === 0, "no other player scores mud");
  ok(sole[0].mult === sole[0].regionScore + 4, "mult == regionScore + mudScore");
  // TIE for fewest: You (2) == B1 (2) < B2 (5) -> You & B1 each score largest region 2
  const tie = C.scoreGame([
    { hand: ["R", "G"], dry: 0, swaps: 0, target: null },
    { hand: ["B", "C"], dry: 0, swaps: 0, target: null },
    { hand: ["R", "G", "B", "C", "M"], dry: 0, swaps: 0, target: null },
  ], board);
  ok(tie[0].mudScore === 2 && tie[1].mudScore === 2, `tie for fewest -> each scores largest region 2 (got ${tie[0].mudScore}, ${tie[1].mudScore})`);
  ok(tie[2].mudScore === 0, "the non-fewest player scores no mud on a tie");
  // the seeded K center counts as mud in a real game
  const fresh = C.newGame([{ isBot: true }, { isBot: true }], 1);
  ok(C.scoreGame(fresh.players, fresh.stacks)[0].boardMud >= 1, "K center counts toward total mud in a real game");
  // magnitude sanity: mud is a tiebreaker, not a juggernaut (total mud is tiny)
  ok(mr.total <= 6, "total mud stays small relative to region scores");
}

section("L. mud-upgraded swaps — made mud => discard 1 (net-0), opposite OR either neighbor");
{
  const board = {}; C.cells.forEach(c => { board[C.key(c[0], c[1])] = []; });   // empty board, full bag
  const bag = {}; C.PAL.order.forEach(c => bag[c] = 30);
  const hand = ["C", "M", "Y", "R"];                                            // one of each — 4 chits
  const normal = C.availableSwaps(hand, bag, board, false);
  const upgraded = C.availableSwaps(hand, bag, board, true);
  ok(normal.every(o => o.discards.length === 2), "normal swaps discard 2");
  ok(upgraded.length > 0 && upgraded.every(o => o.discards.length === 1), "mudded swaps discard exactly 1 (net-0 with the gain)");
  // opposite: discard 1 X -> wheelOpposite(X)
  const opp = upgraded.find(o => o.type === "opposite" && o.discards[0] === "C");
  ok(opp && opp.get === C.wheelOpposite("C"), `mudded opposite: discard 1 C -> ${C.wheelOpposite("C")} (got ${opp && opp.get})`);
  // adjacent: discard 1 X -> a wheel-neighbor of X (both neighbors offered)
  const ord = C.PAL.order, i = ord.indexOf("C"), nbrs = [ord[(i + 5) % 6], ord[(i + 1) % 6]];
  const adj = upgraded.filter(o => o.type === "adjacent" && o.discards[0] === "C").map(o => o.get).sort();
  ok(JSON.stringify(adj) === JSON.stringify(nbrs.slice().sort()), `mudded adjacent: discard 1 C -> either neighbor ${JSON.stringify(nbrs)} (got ${JSON.stringify(adj)})`);

  // net-0 hand accounting: applying a mudded swap keeps hand size constant
  const G = C.newGame([{ isBot: true }], 5);
  G.players[0].hand = ["C", "M"]; const before = G.players[0].hand.length;
  const granted = C.resolveSwaps(G, [{ pi: 0, type: "opposite", discards: ["C"], get: C.wheelOpposite("C") }]);
  ok(granted.length === 1 && G.players[0].hand.length === before, "mudded swap is net-0 on hand size (−1 discard +1 gain)");
  ok(G.players[0].discarded === 1, "exactly 1 chit left play (vs 2 for a normal swap) — ledger stays exact");
}

section("M. tiebreak = most region types scored; cycleLock steers toward A/B/A/B");
{
  // scoredTypes on a board with Green/Red/Cyan blobs + two Blue blobs (2nd blue=2)
  const board = {}; C.cells.forEach(c => { board[C.key(c[0], c[1])] = []; });
  const used = new Set([C.key(0, 0)]);
  function grow(seed, size, color) {
    const sk = C.key(seed[0], seed[1]); if (used.has(sk)) return;
    const f = [seed], inF = new Set([sk]); let n = 0;
    while (n < size && f.length) { const cur = f.shift(), ck = C.key(cur[0], cur[1]); if (used.has(ck)) continue; used.add(ck); board[ck] = [color]; n++;
      for (const d of C.NEI) { const nb = [cur[0] + d[0], cur[1] + d[1]], nk = C.key(nb[0], nb[1]); if (C.cellSet[nk] && !used.has(nk) && !inF.has(nk)) { inF.add(nk); f.push(nb); } } }
  }
  grow([5, 0], 8, "G"); grow([-5, 5], 5, "R"); grow([0, -5], 3, "C"); grow([5, -5], 4, "B"); grow([-5, 0], 2, "B");
  const s = C.scoreGame([
    { hand: ["R", "G"], dry: 0, swaps: 0, target: null },           // Green only -> 1 type
    { hand: ["R", "R", "C", "B"], dry: 0, swaps: 0, target: null },  // Cyan + Blue(tie) -> 2 types
    { hand: ["R", "R", "R", "B"], dry: 0, swaps: 0, target: null },  // Red + Blue(tie) -> 2 types
  ], board);
  ok(s[0].scoredTypes === 1, `You scored 1 region type (got ${s[0].scoredTypes})`);
  ok(s[1].scoredTypes === 2 && s[2].scoredTypes === 2, `B1/B2 scored 2 region types each (${s[1].scoredTypes}, ${s[2].scoredTypes})`);

  // cycleLock: raising it makes decide prefer the 2-turns-ago color when that's a legal option
  let steered = 0, observed = 0;
  for (let seed = 700; seed < 740; seed++) {
    const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], seed);
    let guard = 0;
    while (!G.over && guard++ < 40) {
      const p = G.players[0];
      const legal = new Set(C.enumerateMoves(G, 0).map(m => m.chit));
      if (p.lastPlaced2 && legal.has(p.lastPlaced2) && legal.size >= 2) {
        const origG = p.g, saveRng = G.rng; G.rng = () => 0;
        p.g = Object.assign({}, origG, { cycleLock: 0 }); const a = C.decide(G, 0);
        p.g = Object.assign({}, origG, { cycleLock: 2 }); const b = C.decide(G, 0);
        G.rng = saveRng; p.g = origG;
        if (a && b) { observed++; if (a.chit !== p.lastPlaced2 && b.chit === p.lastPlaced2) steered++; }
      }
      C.step(G, false);
    }
  }
  ok(observed > 0, "observed decisions where the 2-back color was a legal option");
  ok(steered > 0, `cycleLock steered the pick to the 2-back color in ${steered}/${observed} cases (A/B/A/B lever is live)`);
}

// ─────────────────────────────────────────────────────────────────────────────
console.log(`\n${fail === 0 ? "✓ ALL PASS" : "✗ FAILURES"} — ${pass} passed, ${fail} failed.`);
process.exit(fail === 0 ? 0 : 1);
