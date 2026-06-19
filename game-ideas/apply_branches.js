/* Build the 4 isolated Chroma scoring branches from the pristine originals.
 * Gameplay is identical across branches (scoring is only read at game-end and
 * never feeds bot decisions); only scoreGame/scoreHand + RULE_NAME differ.
 * Run: node apply_branches.js   (from /home/m/organism/game-ideas) */
const fs = require("fs"), path = require("path");
const DIR = __dirname;
const coreSrc = fs.readFileSync(path.join(DIR, "chroma-core.js"), "utf8");
const simSrc = fs.readFileSync(path.join(DIR, "chroma-sim.js"), "utf8");

function must(s, find) { if (s.indexOf(find) < 0) throw new Error("anchor not found: " + find.slice(0, 60)); return s; }
function rep(s, find, with_) { must(s, find); return s.split(find).join(with_); }

// ── core: add largestTwo (top-2 region sizes per color) — used by op1 & op2b ──
const LARGEST_TWO = `  function largestTwo(stacks) {
    var col = boardColors(stacks), seen = {}, first = {}, second = {};
    PAL.order.forEach(function (c) { first[c] = 0; second[c] = 0; });
    cells.forEach(function (c) {
      var k0 = key(c[0], c[1]);
      if (seen[k0]) return;
      var color = col[k0];
      if (PAL.order.indexOf(color) < 0) { seen[k0] = true; return; }
      var st = [[c[0], c[1]]], size = 0; seen[k0] = true;
      while (st.length) {
        var cur = st.pop(); size++;
        NEI.forEach(function (d) {
          var nk = key(cur[0] + d[0], cur[1] + d[1]);
          if (cellSet[nk] && !seen[nk] && col[nk] === color) { seen[nk] = true; st.push([cur[0] + d[0], cur[1] + d[1]]); }
        });
      }
      if (size > first[color]) { second[color] = first[color]; first[color] = size; }
      else if (size > second[color]) { second[color] = size; }
    });
    return { first: first, second: second };
  }
  function tier(n) {`;

function coreWithLargestTwo(s) {
  s = rep(s, "  function tier(n) {", LARGEST_TWO);
  s = rep(s, "boardColors: boardColors, largestRegions: largestRegions, scoreHand: scoreHand,",
            "boardColors: boardColors, largestRegions: largestRegions, largestTwo: largestTwo, scoreHand: scoreHand,");
  return s;
}

// ── core: op1 scoreHand (additive second-largest region when holding >=2) ──
const SCOREHAND_OLD = `  function scoreHand(hand, regions) {
    var size = 0, add = 0, cnt = {};
    hand.forEach(function (c) { cnt[c] = (cnt[c] || 0) + 1; });
    PAL.order.forEach(function (c) { if ((cnt[c] || 0) > 0) { size += regions[c]; add += tier(regions[c]); } });
    return { mult: size, add: add, size: size };
  }`;
const SCOREHAND_OP1 = `  function scoreHand(hand, regions, second) {
    // op1: base (largest region for >=1 chit) PLUS the color's SECOND-largest
    // region when you hold >=2 chits of it. Additive on top of base.
    var size = 0, add = 0, cnt = {};
    hand.forEach(function (c) { cnt[c] = (cnt[c] || 0) + 1; });
    PAL.order.forEach(function (c) {
      var n = cnt[c] || 0;
      if (n > 0) {
        size += regions[c]; add += tier(regions[c]);
        if (n >= 2 && second) { size += second[c]; add += tier(second[c]); }
      }
    });
    return { mult: size, add: add, size: size };
  }`;

// ── sim: instrumented runVolume with end-game hand-variety report ──
const RUNVOL_OLD = `// volume run: fully-logged games (mixed archetype trios) -> JSONL corpus
function runVolume(N) {
  const SEED = 4242, rng = mulberry32(SEED);
  const stamp = new Date(1717880000000).toISOString().replace(/[:.]/g, "").slice(0, 15) + "Z"; // fixed-ish stamp
  const fn = path.join(LOGDIR, "volume-" + N + ".jsonl");
  const ws = fs.createWriteStream(fn);
  const orgs = ARCHETYPES.map(a => ({ name: a.name, genome: archetypeGenome(a) }));
  const agg = { games: 0, turnsSum: 0, dry: { mud: 0, empty: 0, conflict: 0 }, swaps: { opposite: 0, between: 0, denied: 0 }, byArch: {}, winnerTurns: [] };
  orgs.forEach(o => agg.byArch[o.name] = { win: 0, n: 0, swaps: 0 });
  for (let i = 0; i < N; i++) {
    const trio = orgs.slice().sort(() => rng() - 0.5).slice(0, 3);
    const seed = (rng() * 2 ** 31) >>> 0;
    const r = playGame(trio.map(o => o.genome), seed, true);
    const rec = {
      schema: "chroma-game/1", seed, palette: C.palette, nPlayers: 3, depth: 3, perColor: PER_COLOR, mudLimit: MUD_LIMIT,
      players: trio.map((o, j) => ({ name: o.name, region: regionTag(o.genome), target: r.scores[j].target, dry: r.scores[j].dry, swaps: r.scores[j].swaps, handLen: r.scores[j].handLen, mult: r.scores[j].mult, add: r.scores[j].add })),
      share: r.share, turns: r.turns, dryEvents: r.G.dryEvents, swapEvents: r.G.swapEvents, moves: r.G.moves,
    };
    ws.write(JSON.stringify(rec) + "\\n");
    agg.games++; agg.turnsSum += r.turns;
    ["mud", "empty", "conflict"].forEach(k => agg.dry[k] += r.G.dryEvents[k]);
    ["opposite", "between", "denied"].forEach(k => agg.swaps[k] += r.G.swapEvents[k]);
    trio.forEach((o, j) => { agg.byArch[o.name].n++; agg.byArch[o.name].win += r.share[j]; agg.byArch[o.name].swaps += r.scores[j].swaps; });
  }
  ws.end();
  agg.avgTurns = +(agg.turnsSum / agg.games).toFixed(1);
  agg.swapsPerGame = +((agg.swaps.opposite + agg.swaps.between) / agg.games).toFixed(2);
  Object.keys(agg.byArch).forEach(k => { agg.byArch[k].winRate = +(agg.byArch[k].win / Math.max(1, agg.byArch[k].n)).toFixed(3); agg.byArch[k].swapsPerGame = +(agg.byArch[k].swaps / Math.max(1, agg.byArch[k].n)).toFixed(2); });
  fs.writeFileSync(path.join(__dirname, "volume-summary.json"), JSON.stringify(agg, null, 1));
  console.log("Volume done:", N, "games ->", fn);
  console.log("avg turns", agg.avgTurns, "dry events", agg.dry, "swaps", agg.swaps, "(", agg.swapsPerGame, "/game )");
}`;

const RUNVOL_NEW = `// volume run: fully-logged games (mixed archetype trios) -> JSONL corpus
// Instrumented for the scoring experiment: emits an end-game hand-VARIETY report.
// NOTE: bots are fixed (not re-evolved per rule) and scoring is read only at
// game-end, so the boards/hands are identical across branches for the same seeds;
// the all-seats variety numbers are a population constant. The signal that moves
// between rules is the WIN-SHARE-WEIGHTED ("winner") profile — what the winning
// hand looks like under each scoring rule.
function runVolume(N) {
  const SEED = 4242, rng = mulberry32(SEED);
  const fn = path.join(LOGDIR, "volume-" + N + ".jsonl");
  const ws = fs.createWriteStream(fn);
  const orgs = ARCHETYPES.map(a => ({ name: a.name, genome: archetypeGenome(a) }));
  const agg = { rule: RULE_NAME, games: 0, turnsSum: 0, dry: { mud: 0, empty: 0, conflict: 0 }, swaps: { opposite: 0, between: 0, denied: 0 }, byArch: {}, winnerTurns: [] };
  orgs.forEach(o => agg.byArch[o.name] = { win: 0, n: 0, swaps: 0 });
  const cols = colors();
  const V = {
    seats: 0, games: 0, handLenSum: 0,
    distinctHist: [0, 0, 0, 0, 0, 0, 0], perColor: {},
    wShare: 0, wHandLenSum: 0, wDistinctHist: [0, 0, 0, 0, 0, 0, 0], wPerColor: {},
    mudCells: 0, boardCells: 0, byArchD: {},
  };
  cols.forEach(c => { V.perColor[c] = 0; V.wPerColor[c] = 0; });
  orgs.forEach(o => V.byArchD[o.name] = { wDistinct: 0, share: 0 });
  for (let i = 0; i < N; i++) {
    const trio = orgs.slice().sort(() => rng() - 0.5).slice(0, 3);
    const seed = (rng() * 2 ** 31) >>> 0;
    const r = playGame(trio.map(o => o.genome), seed, true);
    const rec = {
      schema: "chroma-game/1", rule: RULE_NAME, seed, palette: C.palette, nPlayers: 3, depth: 3, perColor: PER_COLOR, mudLimit: MUD_LIMIT,
      players: trio.map((o, j) => ({ name: o.name, region: regionTag(o.genome), target: r.scores[j].target, dry: r.scores[j].dry, swaps: r.scores[j].swaps, handLen: r.scores[j].handLen, distinct: r.scores[j].distinct, mult: r.scores[j].mult, add: r.scores[j].add })),
      share: r.share, turns: r.turns, dryEvents: r.G.dryEvents, swapEvents: r.G.swapEvents, moves: r.G.moves,
    };
    ws.write(JSON.stringify(rec) + "\\n");
    agg.games++; agg.turnsSum += r.turns;
    ["mud", "empty", "conflict"].forEach(k => agg.dry[k] += r.G.dryEvents[k]);
    ["opposite", "between", "denied"].forEach(k => agg.swaps[k] += r.G.swapEvents[k]);
    trio.forEach((o, j) => { agg.byArch[o.name].n++; agg.byArch[o.name].win += r.share[j]; agg.byArch[o.name].swaps += r.scores[j].swaps; });
    // variety: final-board mud frequency (exclude the fixed black center cell)
    const bcol = C.boardColors(r.G.stacks);
    for (const k in bcol) { if (k === C.key(0, 0)) continue; V.boardCells++; if (bcol[k] === "mud") V.mudCells++; }
    V.games++;
    trio.forEach((o, j) => {
      const hand = r.G.players[j].hand, distinct = new Set(hand).size, sh = r.share[j];
      V.seats++; V.handLenSum += hand.length; V.distinctHist[distinct]++;
      V.wShare += sh; V.wHandLenSum += sh * hand.length; V.wDistinctHist[distinct] += sh;
      const cnt = {}; hand.forEach(c => cnt[c] = (cnt[c] || 0) + 1);
      cols.forEach(c => { V.perColor[c] += (cnt[c] || 0); V.wPerColor[c] += sh * (cnt[c] || 0); });
      V.byArchD[o.name].wDistinct += sh * distinct; V.byArchD[o.name].share += sh;
    });
  }
  ws.end();
  agg.avgTurns = +(agg.turnsSum / agg.games).toFixed(1);
  agg.swapsPerGame = +((agg.swaps.opposite + agg.swaps.between) / agg.games).toFixed(2);
  Object.keys(agg.byArch).forEach(k => { agg.byArch[k].winRate = +(agg.byArch[k].win / Math.max(1, agg.byArch[k].n)).toFixed(3); agg.byArch[k].swapsPerGame = +(agg.byArch[k].swaps / Math.max(1, agg.byArch[k].n)).toFixed(2); });
  const norm = h => h.map(x => +(x / Math.max(1, V.seats)).toFixed(3));
  const wnorm = h => h.map(x => +(x / Math.max(1e-9, V.wShare)).toFixed(3));
  const obj = (f) => { const o = {}; cols.forEach(c => o[c] = f(c)); return o; };
  agg.variety = {
    rule: RULE_NAME, games: V.games, seats: V.seats,
    avgHandLen: +(V.handLenSum / V.seats).toFixed(2),
    winnerAvgHandLen: +(V.wHandLenSum / V.wShare).toFixed(2),
    avgDistinct: +(V.distinctHist.reduce((s, x, i) => s + i * x, 0) / V.seats).toFixed(2),
    winnerAvgDistinct: +(V.wDistinctHist.reduce((s, x, i) => s + i * x, 0) / V.wShare).toFixed(2),
    distinctDist: norm(V.distinctHist),
    winnerDistinctDist: wnorm(V.wDistinctHist),
    perColorPerGame: obj(c => +(V.perColor[c] / V.games).toFixed(2)),
    winnerPerColorShare: obj(c => +(V.wPerColor[c] / V.wShare).toFixed(2)),
    mudFreq: +(V.mudCells / V.boardCells).toFixed(4),
    byArchWinnerDistinct: (() => { const o = {}; Object.keys(V.byArchD).forEach(k => { const b = V.byArchD[k]; o[k] = b.share > 0 ? +(b.wDistinct / b.share).toFixed(2) : null; }); return o; })(),
  };
  fs.writeFileSync(path.join(__dirname, "volume-summary.json"), JSON.stringify(agg, null, 1));
  console.log("[" + RULE_NAME + "] Volume:", N, "games ->", fn, " avgTurns", agg.avgTurns, "mudFreq", agg.variety.mudFreq);
  console.log("  all-seats avgDistinct", agg.variety.avgDistinct, "distinctDist", JSON.stringify(agg.variety.distinctDist));
  console.log("  WINNERS   avgDistinct", agg.variety.winnerAvgDistinct, "distinctDist", JSON.stringify(agg.variety.winnerDistinctDist), "avgHandLen", agg.variety.winnerAvgHandLen);
}`;

// ── sim: scoreGame variants ──
const SCOREGAME_OLD = `function scoreGame(G) {
  const reg = C.largestRegions(G.stacks);
  return G.players.map(p => {
    const sc = C.scoreHand(p.hand, reg);
    const distinct = new Set(p.hand).size;
    return { mult: sc.mult, add: sc.add, distinct, dry: p.dry, swaps: p.swaps, handLen: p.hand.length, target: p.target };
  });
}`;
const SCOREGAME_OP1 = `function scoreGame(G) {
  const reg = C.largestRegions(G.stacks);
  const two = C.largestTwo(G.stacks);
  return G.players.map(p => {
    const sc = C.scoreHand(p.hand, reg, two.second);
    const distinct = new Set(p.hand).size;
    return { mult: sc.mult, add: sc.add, distinct, dry: p.dry, swaps: p.swaps, handLen: p.hand.length, target: p.target };
  });
}`;
function scoreGameOp2(tiemode) {
  return `function tier(n) { return (n >= 1 ? 1 : 0) + (n >= 4 ? 1 : 0) + (n >= 8 ? 1 : 0) + (n >= 12 ? 1 : 0); }
function scoreGame(G) {
  // op2 (${tiemode === "half" ? "op2a" : "op2b"}): a color only scores for the player
  // holding the MOST chits of it. Sole leader scores the FULL largest region.
  // On a tie for most, ${tiemode === "half" ? "each tied player scores HALF the largest region" : "each tied player scores the SECOND-largest region"}.
  const TIEMODE = "${tiemode}";
  const reg = C.largestRegions(G.stacks);
  const two = C.largestTwo(G.stacks);
  const ord = C.PAL.order, NP = G.players.length;
  const cnts = G.players.map(p => { const c = {}; ord.forEach(x => c[x] = 0); p.hand.forEach(x => { if (c[x] !== undefined) c[x]++; }); return c; });
  const mult = G.players.map(() => 0), add = G.players.map(() => 0);
  ord.forEach(color => {
    let mx = 0; for (let i = 0; i < NP; i++) if (cnts[i][color] > mx) mx = cnts[i][color];
    if (mx <= 0) return;                                  // nobody holds it -> region unclaimed
    const holders = []; for (let i = 0; i < NP; i++) if (cnts[i][color] === mx) holders.push(i);
    if (holders.length === 1) { const i = holders[0]; mult[i] += reg[color]; add[i] += tier(reg[color]); }
    else holders.forEach(i => { const v = TIEMODE === "half" ? reg[color] / 2 : two.second[color]; mult[i] += v; add[i] += tier(Math.round(v)); });
  });
  return G.players.map((p, i) => ({ mult: mult[i], add: add[i], distinct: new Set(p.hand).size, dry: p.dry, swaps: p.swaps, handLen: p.hand.length, target: p.target }));
}`;
}

function buildSim(ruleName, scoreGameNew) {
  let s = simSrc;
  s = rep(s, 'C.setDepth(3);', 'C.setDepth(3);\nconst RULE_NAME = "' + ruleName + '";');
  s = rep(s, RUNVOL_OLD, RUNVOL_NEW);
  if (scoreGameNew) s = rep(s, SCOREGAME_OLD, scoreGameNew);
  return s;
}

const branches = {
  base: { core: coreSrc, sim: buildSim("base", null) },
  op1:  { core: rep(coreWithLargestTwo(coreSrc), SCOREHAND_OLD, SCOREHAND_OP1), sim: buildSim("op1", SCOREGAME_OP1) },
  op2a: { core: coreWithLargestTwo(coreSrc), sim: buildSim("op2a", scoreGameOp2("half")) },
  op2b: { core: coreWithLargestTwo(coreSrc), sim: buildSim("op2b", scoreGameOp2("second")) },
};

Object.keys(branches).forEach(b => {
  const dir = path.join(DIR, "chroma-branch-" + b);
  fs.writeFileSync(path.join(dir, "chroma-core.js"), branches[b].core);
  fs.writeFileSync(path.join(dir, "chroma-sim.js"), branches[b].sim);
  console.log("wrote chroma-branch-" + b);
});
console.log("done");
