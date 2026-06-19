/* Chroma — headless simulator + genetic-algorithm bench.
 * Mirrors the live mock's engine (chroma-core.js, depth 3, 30/color bag, hidden
 * hands, dry clock) and adds Muhammad's simultaneous DRAW-CONFLICT rule, then
 * evolves weight-vector "personalities" the way the Eridu evolution bench does
 * (Elo + win-share tournament, niche fitness w/ diversity, crossover+mutation,
 * named archetypes seeding the population).
 *
 * Modes:   node chroma-sim.js smoke        quick engine sanity
 *          node chroma-sim.js ga           evolve a population, write ga-result.json
 *          node chroma-sim.js arena         fixed-archetype round-robin, write arena.json
 *          node chroma-sim.js volume [N]    play N fully-logged games -> chroma-logs/volume-*.jsonl
 */
"use strict";
const fs = require("fs");
const path = require("path");
const C = require("./chroma-core.js");
C.setPalette("CMY");
C.setDepth(3);

const LOGDIR = path.join(__dirname, "chroma-logs");
fs.mkdirSync(LOGDIR, { recursive: true });

// ── constants + engine: ALL shared from chroma-core.js ────────────────────────
// The engine (state, enumerate, decide, applyPlacement, resolveDraws, swaps,
// newGame, step) and the genome SCHEMA live in chroma-core.js so the bench and
// the live mock traverse the IDENTICAL code. This file is now only the GA
// harness (operators, archetypes, tournament/arena/volume) on top of that engine.
const { MUD_LIMIT, START_HAND, PER_COLOR, PRIMARIES, SECONDARIES, GENES, GENE_KEYS } = C;
const colors = () => C.PAL.order;
const key = C.key, NEI = C.NEI;
const mulberry32 = C.mulberry32, randomGenome = C.randomGenome, clampGene = C.clampGene;

// ── GA operators (genetics; the engine's `decide` consumes these weight vectors)
function mutate(genome, rate, rng) {
  const g = Object.assign({}, genome);
  GENE_KEYS.forEach(k => {
    if (rng() < rate) {
      const [lo, hi] = GENES[k], span = hi - lo;
      g[k] = clampGene(k, g[k] + (rng() - 0.5) * span * 0.5);  // gaussian-ish nudge
    }
  });
  return g;
}
function crossover(a, b, rng) {
  const g = {};
  GENE_KEYS.forEach(k => { g[k] = rng() < 0.5 ? a[k] : b[k]; });
  return g;
}

// region label of a genome (for diversity tracking) = its single loudest trait
function regionTag(g) {
  const cand = {
    cmy: g.cmyFocus, rgb: g.rgbFocus, lock: (g.colorLock - 0.5) * 4,
    mudrush: g.mudRush * 1.5, blank: g.blankPriority, cap: g.capPriority,
    edge: g.edgeCenterPref * 2, center: -g.edgeCenterPref * 2, bridge: g.bridgeWeight,
  };
  let best = "balanced", bv = 0.8;  // threshold; below = "balanced"
  for (const k in cand) if (cand[k] > bv) { bv = cand[k]; best = k; }
  return best;
}

// ── engine bridge: state/decide/apply/resolve/swaps/newGame/step come from
// chroma-core.js. The bench drives the SAME engine the live mock uses. ──────────
const bagTotal = C.bagTotal;
const labelRegions = C.labelRegions;
function newGame(genomes, seed) { return C.newGame(genomes.map(g => ({ isBot: true, g })), seed); }
const step = C.step;

// LIVE scoring is op2b, shared from core (game-wide: a color scores for the
// most-chits holder; tie -> each tied gets the 2nd-largest region).
function scoreGame(G) { return C.scoreGame(G.players, G.stacks); }
// rank + win-share (ties split) — multiplicative, tiebreak additive, then distinct colors
function rankAndShare(scores) {
  // tiebreak: total score, then MOST different region types scored (incl. mud), then distinct colors
  const cmp = (a, b) => (b.mult - a.mult) || (b.scoredTypes - a.scoredTypes) || (b.distinct - a.distinct);
  const idx = scores.map((s, i) => i).sort((a, b) => cmp(scores[a], scores[b]));
  const topMult = scores[idx[0]].mult;
  const winners = scores.map((s, i) => i).filter(i => scores[i].mult === topMult && topMult > 0);
  const share = scores.map(() => 0);
  if (winners.length) winners.forEach(i => share[i] = 1 / winners.length);
  return { share, ranking: idx };
}

function playGame(genomes, seed, record) {
  const G = newGame(genomes, seed);
  let guard = 0;
  while (!G.over && guard++ < 400) step(G, record);
  const scores = scoreGame(G);
  const { share } = rankAndShare(scores);
  return { G, scores, share, turns: G.turn };
}

// ── Elo (mirrors eridu/evolve.clj) ─────────────────────────────────────────────
const INIT_ELO = 1500, K = 32;
function expectedScore(a, b) { return 1 / (1 + Math.pow(10, (b - a) / 400)); }
function updateElo(elo, names, scores) {
  const n = names.length;
  const adj = names.map(() => 0);
  for (let i = 0; i < n; i++) for (let j = 0; j < n; j++) {
    if (i === j) continue;
    const exp = expectedScore(elo[names[i]], elo[names[j]]);
    const actual = scores[i].mult > scores[j].mult ? 1 : scores[i].mult === scores[j].mult ? 0.5 : 0;
    adj[i] += (K / (n - 1)) * (actual - exp);
  }
  names.forEach((nm, i) => elo[nm] += adj[i]);
}

// ── archetypes (hand-designed seeds, each leans on one listed trait) ───────────
const ARCHETYPES = [
  { name: "Primarist",  cmyFocus: 1.8, rgbFocus: -0.5, targetDraw: 2.2, growRegion: 1.2, spendTargetPen: 1.5, anyDraw: 1, mudAversion: 1.5 },
  { name: "Secondist",  rgbFocus: 1.8, cmyFocus: -0.5, targetDraw: 2.2, growRegion: 1.2, spendTargetPen: 1.5, anyDraw: 1, mudAversion: 1.5 },
  { name: "Monochrome", colorLock: 0.9, targetDraw: 2.6, growRegion: 1.6, spendTargetPen: 2.2, bridgeWeight: 1, anyDraw: 0.6, mudAversion: 1.5 },
  { name: "MudRusher",  mudRush: 1.6, mudAversion: 0.2, anyDraw: 0.4, blankPriority: 0.3, dryAversion: 0.2 },
  { name: "Canvasser",  blankPriority: 1.8, anyDraw: 1.4, growRegion: 1.0, targetDraw: 1.4, mudAversion: 1.8 },
  { name: "Capper",     capPriority: 1.8, growRegion: 1.2, targetDraw: 1.6, spendTargetPen: 1.2, mudAversion: 1.8 },
  { name: "Rimwalker",  edgeCenterPref: 0.9, blankPriority: 0.8, anyDraw: 1.2, growRegion: 0.8, mudAversion: 1.5 },
  { name: "Corehugger", edgeCenterPref: -0.9, growRegion: 1.4, bridgeWeight: 1.2, targetDraw: 1.6, mudAversion: 1.5 },
  { name: "Bridger",    bridgeWeight: 2.4, growRegion: 1.6, targetDraw: 1.8, spendTargetPen: 1, mudAversion: 1.8 },
  { name: "Hoarder",    spendTargetPen: 2.6, targetDraw: 2.0, growRegion: 1.0, anyDraw: 0.5, mudAversion: 2 },
  { name: "Survivor",   mudAversion: 2.8, dryAversion: 2.2, anyDraw: 1.6, blankPriority: 0.6 },  // pure mud/dry avoidance = extend the game
  { name: "Dumper",     anyDraw: 0.2, dryAversion: 0.1, mudAversion: 0.3, spendTargetPen: 0 },   // naive "dump least-present" baseline
  { name: "LateSwapper", lateSwap: 1.8, earlySwap: 0.2, growRegion: 1.4, targetDraw: 1.8, anyDraw: 1.2, mudAversion: 1.8, dryAversion: 1.4 },  // hoards swap power for the end
  { name: "EarlyShifter", earlySwap: 1.6, lateSwap: 0.8, anyDraw: 1.4, growRegion: 1.0, targetDraw: 1.6, mudAversion: 1.6, dryAversion: 1.2 },  // reshapes a poor opening hand fast
];
function archetypeGenome(a) {
  const base = {}; GENE_KEYS.forEach(k => base[k] = (k === "colorLock") ? 0 : (GENES[k][0] + GENES[k][1]) / 2 * 0); // neutral 0 baseline
  // neutral baseline: 0 for additive, 0 for prefs
  GENE_KEYS.forEach(k => base[k] = 0);
  base.anyDraw = 1; base.targetDraw = 1.5; base.mudAversion = 1.2; base.dryAversion = 1; base.growRegion = 0.8; base.spendTargetPen = 1;
  return Object.assign(base, a);
}

// ── base-12 PAINTER personalities (for future plays) ──────────────────────────
// Each is a competent build (a survival/draw backbone so it is never a punching
// bag) differentiated by ONE signature trait, named for a painter whose style
// rhymes with that trait. Validated by `node chroma-sim.js painters`.
function backbone() { return { anyDraw: 1.2, targetDraw: 1.8, mudAversion: 2.0, dryAversion: 1.6, growRegion: 1.0, spendTargetPen: 1.2 }; }
function painterGenome(sig) { const g = {}; GENE_KEYS.forEach(k => g[k] = 0); Object.assign(g, backbone(), sig); return g; }
const PAINTERS = [
  { name: "Mondrian",  painter: "Piet Mondrian", style: "De Stijl grids of pure primaries", trait: "C·M·Y primary focus", sig: { cmyFocus: 1.9, rgbFocus: -0.4, colorLock: 0.4, growRegion: 1.3, anyDraw: 1.5, dryAversion: 1.9 } },
  { name: "Matisse",   painter: "Henri Matisse", style: "Fauvist bold secondaries", trait: "R·G·B secondary focus", sig: { rgbFocus: 1.9, cmyFocus: -0.4, growRegion: 1.3 } },
  { name: "Klein",     painter: "Yves Klein", style: "whole canvases of one blue (IKB)", trait: "lock one color from the start", sig: { colorLock: 0.85, targetDraw: 2.6, spendTargetPen: 1.8, growRegion: 1.6, bridgeWeight: 1.0, anyDraw: 1.5, blankPriority: 0.6, dryAversion: 1.9 } },
  { name: "Monet",     painter: "Claude Monet", style: "builds light across open fields", trait: "blank-canvas farming", sig: { blankPriority: 1.9, anyDraw: 1.7, growRegion: 1.2 } },
  { name: "Seurat",    painter: "Georges Seurat", style: "pointillist, fills to completion", trait: "stack-capping / sealing", sig: { capPriority: 1.9, growRegion: 1.2, spendTargetPen: 1.4 } },
  { name: "Klimt",     painter: "Gustav Klimt", style: "decorative gilded borders", trait: "edge / rim preference", sig: { edgeCenterPref: 0.85, blankPriority: 0.6, growRegion: 1.0 } },
  { name: "Kandinsky", painter: "Wassily Kandinsky", style: "concentric forms around a center", trait: "center preference", sig: { edgeCenterPref: -0.85, bridgeWeight: 1.2, growRegion: 1.3 } },
  { name: "Miro",      painter: "Joan Miró", style: "linked biomorphic bodies", trait: "island-bridging", sig: { bridgeWeight: 2.4, growRegion: 1.7, targetDraw: 1.8, blankPriority: 0.7, anyDraw: 1.5, mudAversion: 2.2, dryAversion: 1.9 } },
  { name: "Vermeer",   painter: "Johannes Vermeer", style: "patient, withholds, transforms late", trait: "patient late-game hand-swaps", sig: { lateSwap: 1.9, earlySwap: 0.2, targetDraw: 2.0, growRegion: 1.2, mudAversion: 2.2, dryAversion: 1.6 } },
  { name: "Goya",      painter: "Francisco Goya", style: "the darkening Black Paintings", trait: "mud / clock pressure (tempered)", sig: { mudRush: 0.7, mudAversion: 1.4, dryAversion: 1.0, anyDraw: 1.3, blankPriority: 0.4, targetDraw: 1.7, growRegion: 1.0 } },
  { name: "Cezanne",   painter: "Paul Cézanne", style: "methodical structural building", trait: "pure survival / endurance", sig: { mudAversion: 2.9, dryAversion: 2.3, anyDraw: 1.7, blankPriority: 0.5 } },
  { name: "Picasso",   painter: "Pablo Picasso", style: "versatile across every style", trait: "balanced all-rounder (GA-favoured)", sig: { cmyFocus: 0.6, rgbFocus: 0.6, bridgeWeight: 1.2, growRegion: 1.4, blankPriority: 0.7, capPriority: 0.6, anyDraw: 1.3, earlySwap: 0.5, lateSwap: 0.9 } },
];
function painterOrg(p) { const g = painterGenome(p.sig); g.name = p.name; return g; }

function runPainters() {
  const SEED = 2718, rng = mulberry32(SEED);
  const orgs = PAINTERS.map(p => ({ name: p.name, genome: painterOrg(p), sum: 0, n: 0, byOpp: {} }));
  const idx = orgs.map((_, i) => i), combos = [];
  for (let i = 0; i < idx.length; i++) for (let j = i + 1; j < idx.length; j++) for (let k = j + 1; k < idx.length; k++) combos.push([i, j, k]);
  const elo = {}; orgs.forEach(o => elo[o.name] = INIT_ELO);
  const GAMES_PER = 24; let games = 0;
  for (const combo of combos) for (let r = 0; r < GAMES_PER; r++) {
    const order = combo.slice().sort(() => rng() - 0.5);
    const trio = order.map(i => orgs[i]);
    const res = playGame(trio.map(o => o.genome), (rng() * 2 ** 31) >>> 0, false);
    updateElo(elo, trio.map(o => o.name), res.scores);
    trio.forEach((o, i) => { o.sum += res.share[i]; o.n++; order.forEach((oppI, jj) => { if (jj !== i) { const on = orgs[oppI].name; o.byOpp[on] = o.byOpp[on] || { w: 0, n: 0 }; o.byOpp[on].w += res.share[i]; o.byOpp[on].n++; } }); });
    games++;
  }
  const table = orgs.map(o => ({ name: o.name, winRate: +(o.sum / o.n).toFixed(3), elo: Math.round(elo[o.name]) })).sort((a, b) => b.winRate - a.winRate);
  const h2h = {}; orgs.forEach(o => { h2h[o.name] = {}; Object.keys(o.byOpp).forEach(on => h2h[o.name][on] = +(o.byOpp[on].w / o.byOpp[on].n).toFixed(2)); });
  // export the personality library + validation
  const lib = {
    schema: "chroma-personalities/1", note: "Base-12 painter personalities for future Chroma plays. genome = decision weights (see chroma-sim.js GENES).",
    validation: { seed: SEED, gamesPerCombo: GAMES_PER, totalGames: games, table, h2h },
    painters: PAINTERS.map(p => ({ name: p.name, painter: p.painter, style: p.style, signatureTrait: p.trait, region: regionTag(painterOrg(p)), genome: roundGenome(painterOrg(p)) })),
  };
  fs.writeFileSync(path.join(__dirname, "chroma-personalities.json"), JSON.stringify(lib, null, 1));
  console.log("Painters round-robin:", games, "games");
  table.forEach(t => console.log("  " + t.name.padEnd(10), "wr " + t.winRate, "elo " + t.elo));
  console.log("wrote chroma-personalities.json");
}

// ── modes ───────────────────────────────────────────────────────────────────────
function organism(genome, meta = {}) {
  return Object.assign({ genome, name: genome.name || ("rand" + Math.floor(Math.random() * 1e6)), elo: INIT_ELO, sum: 0, n: 0, region: regionTag(genome), parentRich: 0, delta: 0, bestDelta: 0, age: 0 }, meta);
}

function tournament(pop, opts) {
  const { gamesPerOrg = 8, rng } = opts;
  const elo = {}; pop.forEach(o => elo[o.name] = o.elo);
  const totGames = Math.max(1, Math.floor(pop.length * gamesPerOrg / 3));
  for (let gi = 0; gi < totGames; gi++) {
    const shuffled = pop.slice().sort(() => rng() - 0.5);
    const trio = shuffled.slice(0, 3);
    const r = playGame(trio.map(o => o.genome), (rng() * 2 ** 31) >>> 0, false);
    updateElo(elo, trio.map(o => o.name), r.scores);
    trio.forEach((o, i) => { o.sum += r.share[i]; o.n++; });
  }
  pop.forEach(o => { const ne = elo[o.name]; o.delta = ne - o.parentRich; o.bestDelta = Math.max(o.bestDelta, o.delta); o.elo = ne; });
  return totGames;
}
function nicheFitness(o, pop) {
  const same = pop.filter(x => x.region === o.region).length;
  const richness = o.elo;
  const gradient = o.delta > 0 ? o.delta * 3 : (o.bestDelta > 0 ? o.bestDelta * 0.5 : 0);
  const diversity = 80 / Math.max(same, 1);
  return richness + gradient + diversity;
}
function winRate(o) { return o.n ? o.sum / o.n : 0; }

function runGA() {
  const SEED = 12345, rng = mulberry32(SEED);
  const POP = 48, GENS = 22, MUT = 0.3;
  let pop = [];
  // 60% archetype-seeded (with variation), 40% random — mirrors evolve.clj
  const archCount = Math.floor(POP * 0.5);
  for (let i = 0; i < archCount; i++) {
    const a = ARCHETYPES[i % ARCHETYPES.length];
    const g = archetypeGenome(a);
    pop.push(organism(i < ARCHETYPES.length ? g : mutate(g, 0.3, rng)));
  }
  while (pop.length < POP) { const g = randomGenome(rng); g.name = "rand" + pop.length; pop.push(organism(g)); }

  const history = [];
  let totalGames = 0;
  for (let gen = 0; gen < GENS; gen++) {
    pop.forEach(o => { o.sum = 0; o.n = 0; });
    totalGames += tournament(pop, { gamesPerOrg: 9, rng });
    pop.sort((a, b) => b.elo - a.elo);
    const best = pop[0];
    history.push({ gen, bestName: best.name, bestElo: Math.round(best.elo), bestWR: +winRate(best).toFixed(3), popAvgElo: Math.round(pop.reduce((s, o) => s + o.elo, 0) / pop.length) });
    // evolve
    const sorted = pop.slice().sort((a, b) => winRate(b) - winRate(a));
    const nCut = Math.max(1, Math.floor(POP * 0.15));
    const survivors = sorted.slice(0, POP - nCut);
    const parents = survivors.slice(0, Math.max(2, Math.floor(survivors.length * 0.5)));
    const childrenNeeded = POP - survivors.length;
    const nFresh = Math.max(1, Math.floor(childrenNeeded * 0.3));
    const children = [];
    for (let i = 0; i < childrenNeeded - nFresh; i++) {
      const pa = parents[i % parents.length], pb = parents[(i + 1) % parents.length];
      const gentle = winRate(pa) > 0.45;
      const child = mutate(crossover(pa.genome, pb.genome, rng), gentle ? MUT * 0.5 : MUT, rng);
      child.name = "g" + gen + "_" + i;
      children.push(organism(child, { parentRich: pa.elo }));
    }
    for (let i = 0; i < nFresh; i++) { const g = randomGenome(rng); g.name = "g" + gen + "_f" + i; children.push(organism(g)); }
    survivors.forEach(o => o.age++);
    pop = survivors.concat(children);
  }
  pop.sort((a, b) => b.elo - a.elo);
  const top = pop.slice(0, 16).map(o => ({ name: o.name, elo: Math.round(o.elo), winRate: +winRate(o).toFixed(3), region: o.region, age: o.age, genome: roundGenome(o.genome) }));
  const out = { seed: SEED, pop: POP, generations: GENS, totalGames, history, top };
  fs.writeFileSync(path.join(__dirname, "ga-result.json"), JSON.stringify(out, null, 1));
  console.log("GA done:", totalGames, "games over", GENS, "gens. Top:", top.slice(0, 5).map(t => t.name + "(" + t.elo + ")").join(", "));
  console.log("wrote ga-result.json");
}
function roundGenome(g) { const o = {}; GENE_KEYS.forEach(k => o[k] = +(+g[k]).toFixed(2)); if (g.name) o.name = g.name; return o; }

// fixed-archetype round robin: does any single archetype dominate regardless of opponents?
function runArena() {
  const SEED = 999, rng = mulberry32(SEED);
  const orgs = ARCHETYPES.map(a => { const g = archetypeGenome(a); return { name: a.name, genome: g, sum: 0, n: 0, byOpp: {}, elo: INIT_ELO }; });
  const idx = orgs.map((_, i) => i);
  const combos = [];
  for (let i = 0; i < idx.length; i++) for (let j = i + 1; j < idx.length; j++) for (let k = j + 1; k < idx.length; k++) combos.push([i, j, k]);
  const GAMES_PER = 30;
  const elo = {}; orgs.forEach(o => elo[o.name] = INIT_ELO);
  let games = 0;
  for (const combo of combos) {
    for (let r = 0; r < GAMES_PER; r++) {
      const order = combo.slice().sort(() => rng() - 0.5);                 // randomize seat
      const trio = order.map(i => orgs[i]);
      const res = playGame(trio.map(o => o.genome), (rng() * 2 ** 31) >>> 0, false);
      updateElo(elo, trio.map(o => o.name), res.scores);
      trio.forEach((o, i) => {
        o.sum += res.share[i]; o.n++;
        order.forEach((oppI, jj) => { if (jj !== i) { const on = orgs[oppI].name; o.byOpp[on] = o.byOpp[on] || { w: 0, n: 0 }; o.byOpp[on].w += res.share[i]; o.byOpp[on].n++; } });
      });
      games++;
    }
  }
  orgs.forEach(o => o.elo = elo[o.name]);
  const table = orgs.map(o => ({ name: o.name, winRate: +(o.sum / o.n).toFixed(3), elo: Math.round(o.elo), games: o.n }))
    .sort((a, b) => b.winRate - a.winRate);
  // head-to-head matrix (avg win-share vs each opponent)
  const h2h = {};
  orgs.forEach(o => { h2h[o.name] = {}; Object.keys(o.byOpp).forEach(on => h2h[o.name][on] = +(o.byOpp[on].w / o.byOpp[on].n).toFixed(2)); });
  const out = { seed: SEED, gamesPerCombo: GAMES_PER, totalGames: games, table, h2h };
  fs.writeFileSync(path.join(__dirname, "arena.json"), JSON.stringify(out, null, 1));
  console.log("Arena done:", games, "games.");
  table.forEach(t => console.log("  " + t.name.padEnd(11), "wr " + t.winRate, "elo " + t.elo));
  console.log("wrote arena.json");
}

// volume run: fully-logged games (mixed archetype trios) -> JSONL corpus
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
    ws.write(JSON.stringify(rec) + "\n");
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
}

// ── controlled swap A/B (Mohammad) — isolate the MARGINAL value of the swap on
// top of the identified baseline (Cézanne survival). Seat 2 gets the variant; the
// other two seats are plain baseline. Paired on identical seeds so the only
// difference is the swap genes. Answers "is late-game swap powerful?" directly.
function runSwapTest(N) {
  const SEED = 31415, rng = mulberry32(SEED);
  const cez = PAINTERS.find(p => p.name === "Cezanne");
  const baseG = painterOrg(cez);                 // the baseline AI we identified
  const variants = {
    "baseline(no swap)": Object.assign({}, baseG),
    "earlySwap":         Object.assign({}, baseG, { earlySwap: 1.8 }),
    "lateSwap":          Object.assign({}, baseG, { lateSwap: 1.8 }),
    "both swaps":        Object.assign({}, baseG, { earlySwap: 1.2, lateSwap: 1.8 }),
  };
  const seeds = []; for (let i = 0; i < N; i++) seeds.push((rng() * 2 ** 31) >>> 0);
  const out = {};
  Object.keys(variants).forEach(name => {
    let win = 0, swaps = 0, n = 0;
    for (const sd of seeds) {
      const trio = [Object.assign({}, baseG), variants[name], Object.assign({}, baseG)]; // seat 1 = variant
      const r = playGame(trio, sd, false);
      win += r.share[1]; swaps += r.scores[1].swaps; n++;
    }
    out[name] = { seatWinShare: +(win / n).toFixed(3), swapsPerGame: +(swaps / n).toFixed(2), games: n };
  });
  const base = out["baseline(no swap)"].seatWinShare;
  console.log("Swap A/B vs Cézanne baseline (seat win-share; neutral = 0.333, paired seeds):");
  Object.keys(out).forEach(k => console.log("  " + k.padEnd(18), "win " + out[k].seatWinShare, "Δ " + (+(out[k].seatWinShare - base).toFixed(3)), "swaps/game " + out[k].swapsPerGame));
  fs.writeFileSync(path.join(__dirname, "swaptest.json"), JSON.stringify({ seed: SEED, games: N, baseline: base, variants: out }, null, 1));
  console.log("wrote swaptest.json");
}

function smoke() {
  const trio = [ARCHETYPES[2], ARCHETYPES[8], ARCHETYPES[11]].map(a => archetypeGenome(a));
  const r = playGame(trio, 7, true);
  console.log("turns", r.turns, "scores", JSON.stringify(r.scores), "share", r.share, "dry", r.G.dryEvents);
  console.log("sample move[10]", JSON.stringify(r.G.moves[10]));
  // verifier: any mismatches across a few games?
  let mism = 0, games = 0;
  for (let s = 0; s < 50; s++) { const g = newGame(trio, s); let guard = 0; while (!g.over && guard++ < 400) { const plc = []; for (let i = 0; i < g.N; i++) { const d = C.decide(g, i); if (d) { const ap = C.applyPlacement(g, i, d); if (ap.mismatch) mism++; plc.push(ap); } } C.resolveDraws(g, plc); g.turn++; if (g.ending && g.turn > g.finalTurn) g.over = true; const md = Math.max(...g.players.map(p => p.dry)); if (!g.ending && (md >= MUD_LIMIT || bagTotal(g.bag) === 0)) { g.ending = true; g.finalTurn = g.turn; } } games++; }
  console.log("verifier mismatches over", games, "games:", mism);
}

const mode = process.argv[2] || "smoke";
if (mode === "smoke") smoke();
else if (mode === "ga") runGA();
else if (mode === "arena") runArena();
else if (mode === "volume") runVolume(parseInt(process.argv[3] || "1000", 10));
else if (mode === "painters") runPainters();
else if (mode === "swaptest") runSwapTest(parseInt(process.argv[3] || "3000", 10));
else console.log("unknown mode", mode);
