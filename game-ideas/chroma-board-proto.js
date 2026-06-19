/* Prototype: trim 9 BLANK cells at 3 alternating corners (sectors 0/2/4) and
 * measure the friction delta vs the full board. Uses the board's 120° rotational
 * symmetry so the removal is perfectly symmetric for 3 players.
 *   node chroma-board-proto.js [games]
 */
"use strict";
const C = require("./chroma-core.js");
C.setPalette("CMY"); C.setDepth(3);
const DEPTH = 3, GAMES = parseInt(process.argv[2] || "2000", 10);
const key = C.key, cells = C.cells;

const isCenter = c => c[0] === 0 && c[1] === 0;
const seeded = c => !!C.seed[key(c[0], c[1])];
const rot120 = c => [-c[0] - c[1], c[0]];                 // 120° about center (cube y,z,x)
const ringOf = c => Math.max(Math.abs(c[0]), Math.abs(c[1]), Math.abs(-c[0] - c[1]));
const hexDist = (a, b) => (Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) + Math.abs(a[0] + a[1] - b[0] - b[1])) / 2;

// 3 blanks nearest the sector-0 corner tip [5,0], then rotate to sectors 2 & 4.
const tip = [5, 0];
const sec0blanks = cells.filter(c => !isCenter(c) && C.sector(c[0], c[1]) === 0 && !seeded(c))
  .sort((a, b) => hexDist(a, tip) - hexDist(b, tip) || ringOf(b) - ringOf(a));
const base3 = sec0blanks.slice(0, 3);
const nine = [];
base3.forEach(c => { let x = c; for (let r = 0; r < 3; r++) { nine.push(x); x = rot120(x); } });
const removedKeys = nine.map(c => key(c[0], c[1]));

console.log("Removed cells (9 = 3 blanks × 3 alternating corners, 120°-symmetric):");
[0, 1, 2].forEach(s => {
  const grp = nine.filter(c => C.sector(c[0], c[1]) === [0, 2, 4][s]);
  console.log(`  sector ${[0,2,4][s]}: ` + grp.map(c => `[${c}] ring${ringOf(c)} ${seeded(c) ? "SEEDED!" : "blank"}`).join("  "));
});
const allBlank = nine.every(c => !seeded(c));
const symOK = removedKeys.length === 9 && new Set(removedKeys).size === 9;
console.log(`  all blank: ${allBlank} · 9 distinct & symmetric: ${symOK}\n`);

function run(removed) {
  C.setRemoved(removed);
  const playable = cells.filter(c => !isCenter(c) && !C.isRemoved(key(c[0], c[1])));
  const blanks = playable.filter(c => !seeded(c)).length;
  let turns = 0, placements = 0, blocked = 0, forcedPass = 0, pt = 0, tight = 0;
  let legalSum = 0, fullEnd = 0, fillSum = 0;
  const CAP = playable.length * DEPTH;
  for (let s = 0; s < GAMES; s++) {
    const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], s);
    let g = 0;
    while (!G.over && g++ < 500) {
      const plc = [];
      for (let i = 0; i < G.N; i++) {
        pt++;
        const p = G.players[i], w = (p.base + G.turn) % 6, lc = C.legalCells(G.stacks, w);
        legalSum += lc.length;
        if (p.hand.length > 0 && lc.length === 0) blocked++;
        else if (lc.length <= 2) tight++;
        const d = C.decide(G, i);
        if (!d && p.hand.length > 0 && lc.length > 0) forcedPass++;
        if (d) { plc.push(C.applyPlacement(G, i, d)); placements++; }
      }
      C.resolveDraws(G, plc);
      const reg = C.largestRegions(G.stacks); const sp = [];
      for (let i = 0; i < G.N; i++) { const sw = C.decideSwap(G, i, reg); if (sw) sp.push(sw); }
      if (sp.length) C.resolveSwaps(G, sp);
      G.turn++;
      if (G.over) break;
      if (G.ending && G.turn > G.finalTurn) { G.over = true; break; }
      const md = Math.max(...G.players.map(p => p.dry)), bt = C.bagTotal(G.bag);
      if (!G.ending && (md >= C.MUD_LIMIT || bt === 0)) { G.ending = true; G.finalTurn = G.turn; }
    }
    turns += G.turn;
    let placed = 0, full = 0;
    playable.forEach(c => { const h = G.stacks[key(c[0], c[1])].length; placed += h; if (h >= DEPTH) full++; });
    fullEnd += full; fillSum += placed / CAP;
  }
  C.setRemoved([]);
  const P = x => (x / GAMES).toFixed(2), pct = x => (100 * x).toFixed(1) + "%";
  return {
    cellsN: playable.length, blanks, cap: CAP,
    turns: P(turns), placements: P(placements), fill: pct(fillSum / GAMES),
    avgLegal: (legalSum / pt).toFixed(2), tight: pct(tight / pt), blocked: pct(blocked / pt),
    forcedPass: pct(forcedPass / pt), full: P(fullEnd),
  };
}

const base = run([]);
const trim = run(removedKeys);
const rows = [
  ["playable cells", base.cellsN, trim.cellsN],
  ["blank cells (open canvas)", base.blanks, trim.blanks],
  ["capacity", base.cap, trim.cap],
  ["avg turns / game", base.turns, trim.turns],
  ["avg placements / game", base.placements, trim.placements],
  ["end fill", base.fill, trim.fill],
  ["avg legal cells / decision", base.avgLegal, trim.avgLegal],
  ["tight turns (≤2 legal cells)", base.tight, trim.tight],
  ["blocked (0 legal cells, holds chits)", base.blocked, trim.blocked],
  ["forced pass (no legal chit)", base.forcedPass, trim.forcedPass],
  ["full (depth-3) stacks / game", base.full, trim.full],
];
console.log(`Friction comparison — ${GAMES} all-bot 3p games (op2b · mud bonus · no-same-color · swap-rescue)\n`);
console.log("  metric".padEnd(40) + "FULL".padStart(10) + "TRIMMED".padStart(12));
rows.forEach(r => console.log("  " + String(r[0]).padEnd(38) + String(r[1]).padStart(10) + String(r[2]).padStart(12)));
