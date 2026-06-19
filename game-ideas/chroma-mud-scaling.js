/* How does the mud bonus scale with player count? 100 all-bot games per count
 * (2..6) under the full current ruleset (op2b · mud bonus · no-same-color ·
 * swap-rescue), full board. Reports mud's share of scoring and how often games
 * are won on mud rather than regions.
 *   node chroma-mud-scaling.js [gamesPerCount]
 */
"use strict";
const C = require("./chroma-core.js");
C.setPalette("CMY"); C.setDepth(3);
const GAMES = parseInt(process.argv[2] || "100", 10);
const COUNTS = [2, 3, 4, 5, 6];
const key = C.key, isCtr = c => c[0] === 0 && c[1] === 0;
const playable = C.cells.filter(c => !isCtr(c));
const CAP = playable.length * 3;

function winnerIdx(scores) {
  let w = 0;
  for (let i = 1; i < scores.length; i++) {
    const a = scores[i], b = scores[w];
    if (a.mult > b.mult || (a.mult === b.mult && (a.add > b.add || (a.add === b.add && a.distinct > b.distinct)))) w = i;
  }
  return w;
}

const rows = [];
for (const N of COUNTS) {
  let turns = 0, fill = 0, mudSum = 0;
  let pRegion = 0, pMud = 0, pTotal = 0, np = 0;
  let wRegion = 0, wMud = 0, wTotal = 0, winOnMud = 0, winEmpty = 0, winnerHand = 0;
  for (let s = 0; s < GAMES; s++) {
    const G = C.newGame(Array.from({ length: N }, () => ({ isBot: true })), s + N * 100000);
    let g = 0; while (!G.over && g++ < 800) C.step(G, false);
    const scores = C.scoreGame(G.players, G.stacks);
    turns += G.turn;
    let placed = 0; playable.forEach(c => placed += G.stacks[key(c[0], c[1])].length);
    fill += placed / CAP;
    const bc = C.boardColors(G.stacks); let mud = 0; Object.keys(bc).forEach(k => { if (bc[k] === "mud") mud++; });
    mudSum += mud;
    scores.forEach(sc => { np++; pRegion += sc.regionScore; pMud += sc.mudScore; pTotal += sc.mult; });
    const wi = winnerIdx(scores), w = scores[wi];
    wRegion += w.regionScore; wMud += w.mudScore; wTotal += w.mult; winnerHand += w.handLen;
    if (w.mudScore > w.regionScore) winOnMud++;
    if (w.handLen === 0) winEmpty++;
  }
  const pavg = x => (x / np), wavg = x => (x / GAMES);
  rows.push({
    N,
    turns: (turns / GAMES).toFixed(1),
    fill: (100 * fill / GAMES).toFixed(0) + "%",
    mud: (mudSum / GAMES).toFixed(1),
    pReg: pavg(pRegion).toFixed(1),
    pMud: pavg(pMud).toFixed(1),
    pMudShare: (100 * pMud / pTotal).toFixed(0) + "%",
    wTotal: wavg(wTotal).toFixed(1),
    wReg: wavg(wRegion).toFixed(1),
    wMud: wavg(wMud).toFixed(1),
    wMudShare: (100 * wMud / wTotal).toFixed(0) + "%",
    winOnMud: (100 * winOnMud / GAMES).toFixed(0) + "%",
    winEmpty: (100 * winEmpty / GAMES).toFixed(0) + "%",
    wHand: (winnerHand / GAMES).toFixed(1),
  });
}

console.log(`\nMud-bonus scaling — ${GAMES} all-bot games per count, full board, current ruleset\n`);
const C1 = (s, w) => String(s).padStart(w);
console.log(C1("players", 8) + C1("turns", 7) + C1("fill", 6) + C1("mud", 6) + C1("reg/plyr", 10) + C1("mud/plyr", 10) + C1("mud%", 7));
rows.forEach(r => console.log(C1(r.N, 8) + C1(r.turns, 7) + C1(r.fill, 6) + C1(r.mud, 6) + C1(r.pReg, 10) + C1(r.pMud, 10) + C1(r.pMudShare, 7)));
console.log("\nWinner profile:");
console.log(C1("players", 8) + C1("score", 7) + C1("region", 8) + C1("mud", 7) + C1("mud%", 7) + C1("won-on-mud", 12) + C1("empty-hand", 12) + C1("avg hand", 10));
rows.forEach(r => console.log(C1(r.N, 8) + C1(r.wTotal, 7) + C1(r.wReg, 8) + C1(r.wMud, 7) + C1(r.wMudShare, 7) + C1(r.winOnMud, 12) + C1(r.winEmpty, 12) + C1(r.wHand, 10)));
console.log("");
