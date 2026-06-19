/* Scans the complete Chroma game logs + GA/arena outputs and answers the
 * design question Muhammad posed: is there an easy, obvious optimal that's
 * mindless to navigate? Writes analysis.json. */
"use strict";
const fs = require("fs");
const path = require("path");
const dir = __dirname;
const LOGDIR = path.join(dir, "chroma-logs");

function readJSON(f) { return JSON.parse(fs.readFileSync(path.join(dir, f), "utf8")); }

// ── 1. scan the full play-by-play corpus ────────────────────────────────────
const jsonl = fs.readdirSync(LOGDIR).filter(f => /^volume-.*\.jsonl$/.test(f)).sort().pop();
const lines = fs.readFileSync(path.join(LOGDIR, jsonl), "utf8").trim().split("\n");
const games = lines.map(l => JSON.parse(l));

let nMoves = 0, nDraws = 0, dry = { mud: 0, empty: 0, conflict: 0 }, mismatch = 0;
let blankPlays = 0, capSeals = 0;
const swaps = { opposite: 0, between: 0, denied: 0 };
const turnLens = [];
let winnerBiggestRegion = 0, winnerMostHoard = 0, winnerMostSwaps = 0, decided = 0;
const byArch = {};
for (const g of games) {
  turnLens.push(g.turns);
  ["mud", "empty", "conflict"].forEach(k => dry[k] += g.dryEvents[k]);
  if (g.swapEvents) ["opposite", "between", "denied"].forEach(k => swaps[k] += g.swapEvents[k]);
  for (const m of g.moves) for (const p of m.plays) {
    nMoves++;
    if (p.drew) nDraws++;
    if (p.dry) dry[p.dry] = dry[p.dry];           // already aggregated
    if (p.prev === "white") blankPlays++;
  }
  // who won, and why
  const share = g.share; const maxS = Math.max(...share);
  const winners = share.map((s, i) => i).filter(i => share[i] === maxS && maxS > 0);
  if (winners.length === 1) {
    decided++;
    const w = winners[0];
    // biggest hand-mult => already encodes region*hold; check if winner held most chits of a present color
    const wp = g.players[w];
    const others = g.players.filter((_, i) => i !== w);
    if (wp.mult >= Math.max(...others.map(o => o.mult))) winnerBiggestRegion++;
    if (wp.handLen >= Math.max(...others.map(o => o.handLen))) winnerMostHoard++;
    if ((wp.swaps || 0) >= Math.max(...others.map(o => o.swaps || 0)) && (wp.swaps || 0) > 0) winnerMostSwaps++;
  }
  g.players.forEach((p, i) => {
    const a = byArch[p.name] = byArch[p.name] || { n: 0, win: 0, mult: 0, dry: 0, swaps: 0 };
    a.n++; a.win += share[i]; a.mult += p.mult; a.dry += p.dry; a.swaps += (p.swaps || 0);
  });
}
const archTable = Object.entries(byArch).map(([name, a]) => ({
  name, winRate: +(a.win / a.n).toFixed(3), avgMult: +(a.mult / a.n).toFixed(1), avgDry: +(a.dry / a.n).toFixed(2), avgSwaps: +(a.swaps / a.n).toFixed(2), n: a.n,
})).sort((x, y) => y.winRate - x.winRate);

// ── 2. non-transitivity in the archetype head-to-head (anti-dominance signal) ─
const arena = readJSON("arena.json");
const h2h = arena.h2h, names = arena.table.map(t => t.name);
let cycles = 0, comparablePairs = 0;
function beats(a, b) { const v = h2h[a] && h2h[a][b]; return v !== undefined ? v > 0.5 : null; }
for (let i = 0; i < names.length; i++) for (let j = 0; j < names.length; j++) for (let k = 0; k < names.length; k++) {
  if (i === j || j === k || i === k) continue;
  const ab = beats(names[i], names[j]), bc = beats(names[j], names[k]), ca = beats(names[k], names[i]);
  if (ab && bc && ca) cycles++;                   // A>B>C>A intransitive triple
}
// rock-paper-scissors evidence: does each archetype have at least one it beats AND one it loses to?
const rps = names.map(n => {
  const row = h2h[n] || {};
  const wins = Object.values(row).filter(v => v > 0.5).length;
  const losses = Object.values(row).filter(v => v < 0.5).length;
  return { name: n, beats: wins, losesTo: losses };
});

// ── 3. dominance / skill-gradient verdict ───────────────────────────────────
const top = arena.table[0], bottom = arena.table[arena.table.length - 1];
const dumper = arena.table.find(t => t.name === "Dumper");       // naive "dump least-present" baseline
const mudrush = arena.table.find(t => t.name === "MudRusher");
const fieldAvg = 1 / 3;                                          // 3-player neutral expectation

// ── 4. GA convergence: did evolution collapse to ONE strategy or hold diversity?
const ga = readJSON("ga-result.json");
const topRegions = {};
ga.top.forEach(t => topRegions[t.region] = (topRegions[t.region] || 0) + 1);

const out = {
  corpus: { file: jsonl, games: games.length, totalPlacements: nMoves, drawSuccessRate: +(nDraws / nMoves).toFixed(3) },
  pacing: { avgTurns: +(turnLens.reduce((a, b) => a + b, 0) / turnLens.length).toFixed(1), minTurns: Math.min(...turnLens), maxTurns: Math.max(...turnLens) },
  dryComposition: dry,
  dryShare: { mud: +(dry.mud / (dry.mud + dry.empty + dry.conflict)).toFixed(3), empty: +(dry.empty / (dry.mud + dry.empty + dry.conflict)).toFixed(3), conflict: +(dry.conflict / (dry.mud + dry.empty + dry.conflict)).toFixed(3) },
  conflictRule: { conflictDriesPerGame: +(dry.conflict / games.length).toFixed(2), note: "draw-conflict denials (Muhammad's new rule) per game" },
  blankPlayShare: +(blankPlays / nMoves).toFixed(3),
  swaps: { opposite: swaps.opposite, between: swaps.between, denied: swaps.denied, perGame: +((swaps.opposite + swaps.between) / games.length).toFixed(2), note: "color-wheel hand-swaps (Mohammad's new mechanic). opposite=2-same→opposite, between=2-different→wheel-between." },
  decisiveness: { decidedGames: decided, winnerHadTopScore: +(winnerBiggestRegion / decided).toFixed(3), winnerHeldMostChits: +(winnerMostHoard / decided).toFixed(3), winnerSwappedMostOfThoseWhoSwapped: +(winnerMostSwaps / decided).toFixed(3) },
  archetypeWinRates: archTable,
  dominance: {
    topArchetype: top.name, topWinRate: top.winRate, fieldNeutral: +fieldAvg.toFixed(3),
    topToBottomSpread: +(top.winRate - bottom.winRate).toFixed(3),
    naiveDumperWinRate: dumper.winRate, naiveDumperRank: arena.table.findIndex(t => t.name === "Dumper") + 1,
    mudRusherWinRate: mudrush.winRate, mudRusherRank: arena.table.findIndex(t => t.name === "MudRusher") + 1,
    intransitiveTriples: cycles, rps,
  },
  gaConvergence: { generations: ga.generations, totalGames: ga.totalGames, topRegionMix: topRegions, bestElo: ga.top[0].elo },
  verdict: buildVerdict(top, dumper, cycles, rps),
};
fs.writeFileSync(path.join(dir, "analysis.json"), JSON.stringify(out, null, 1));
console.log(JSON.stringify(out, null, 1));

function buildVerdict(top, dumper, cycles, rps) {
  const v = [];
  if (top.winRate > 0.62) v.push("WARN: top archetype win-rate " + top.winRate + " is high — one lever may be too strong.");
  else v.push("OK: no archetype runs away (top win-rate " + top.winRate + " in a 3-player field where 0.333 is neutral).");
  if (dumper.winRate < fieldAvg) v.push("OK: the naive 'dump least-present chit' baseline (Dumper) is below neutral (" + dumper.winRate + ") — the obvious mindless line is NOT optimal.");
  else v.push("WARN: naive Dumper at/above neutral (" + dumper.winRate + ") — a mindless line competes.");
  v.push((cycles > 0 ? "OK" : "WARN") + ": " + cycles + " intransitive (rock-paper-scissors) triples among archetypes — " + (cycles > 0 ? "strategy choice depends on the table, not a single dominant line." : "no cycles found; ordering may be near-total."));
  const flexible = rps.filter(r => r.beats > 0 && r.losesTo > 0).length;
  v.push(flexible + "/" + rps.length + " archetypes both beat someone AND lose to someone (matchup texture).");
  return v;
}
