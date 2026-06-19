/* Chroma — 3-chit stack frequency + blocking analysis (under the CURRENT rules:
 * op2b scoring, mud bonus, no-same-color-as-last-turn). Answers Mohammad's flag:
 * how often do stacks reach full depth (3), and how often does that BLOCK a player
 * (their rotated wedge has no placeable cell while they still hold chits)? Informs
 * whether removing board spaces would make space-pressure matter.
 *
 *   node chroma-stack-analysis.js [games]
 */
"use strict";
const C = require("./chroma-core.js");
C.setPalette("CMY"); C.setDepth(3);
const DEPTH = 3;
const GAMES = parseInt(process.argv[2] || "1000", 10);

const playable = C.cells.filter(c => !(c[0] === 0 && c[1] === 0));   // 90 cells (exclude center)
const CAP = playable.length * DEPTH;

let totTurns = 0, totPlacements = 0, totPasses = 0, totBlocked = 0, totPlayerTurns = 0;
let gamesWithFull = 0;
const heightHist = [0, 0, 0, 0];          // count of cells ending at height 0/1/2/3 (summed over games)
let fullAtEnd = 0;                         // total full(=3) cells at game end, summed
let fillSum = 0;                           // board fill fraction at end, summed
const blockedWedgeFullness = [0, 0, 0, 0, 0, 0]; // when blocked, how many of the wedge's cells were full? (bucketed)
let endTrigger = { dry: 0, bagEmpty: 0 };

function wedgeOf(p, turn) { return (p.base + turn) % 6; }

for (let seed = 0; seed < GAMES; seed++) {
  const G = C.newGame([{ isBot: true }, { isBot: true }, { isBot: true }], seed);
  let guard = 0;
  while (!G.over && guard++ < 500) {
    const plc = [];
    for (let i = 0; i < G.N; i++) {
      totPlayerTurns++;
      const p = G.players[i], w = wedgeOf(p, G.turn);
      const legal = C.legalCells(G.stacks, w);
      if (p.hand.length > 0 && legal.length === 0) {
        // blocked: holds chits but the rotated wedge has no placeable cell
        totBlocked++;
        const wedgeCells = playable.filter(c => C.sector(c[0], c[1]) === w);
        const full = wedgeCells.filter(c => G.stacks[C.key(c[0], c[1])].length >= DEPTH).length;
        blockedWedgeFullness[Math.min(5, full)]++;
      }
      const d = C.decide(G, i);
      if (d) { plc.push(C.applyPlacement(G, i, d)); totPlacements++; }
      else totPasses++;
    }
    C.resolveDraws(G, plc);
    G.turn++;
    if (G.over) break;
    if (G.ending && G.turn > G.finalTurn) { G.over = true; break; }
    const md = Math.max(...G.players.map(p => p.dry)), bt = C.bagTotal(G.bag);
    if (!G.ending && (md >= C.MUD_LIMIT || bt === 0)) { G.ending = true; G.finalTurn = G.turn; if (bt === 0) endTrigger.bagEmpty++; else endTrigger.dry++; }
  }
  totTurns += G.turn;
  // end-state stack heights
  let full = 0, placed = 0;
  playable.forEach(c => { const h = G.stacks[C.key(c[0], c[1])].length; heightHist[Math.min(3, h)]++; placed += h; if (h >= DEPTH) full++; });
  fullAtEnd += full; fillSum += placed / CAP;
  if (full > 0) gamesWithFull++;
}

const pct = x => (100 * x).toFixed(1) + "%";
const per = x => (x / GAMES).toFixed(2);
const cellsTot = GAMES * playable.length;
console.log(`\nChroma stack/blocking analysis — ${GAMES} all-bot 3p games (op2b · mud bonus · no-same-color)`);
console.log(`Board: ${playable.length} playable cells × depth ${DEPTH} = ${CAP} capacity.`);
console.log(`\nGame length:   avg ${per(totTurns)} turns · ${per(totPlacements)} placements/game · end-of-bag fill ${pct(fillSum / GAMES)}`);
console.log(`End trigger:   dry-clock ${pct(endTrigger.dry / GAMES)} · bag-empty ${pct(endTrigger.bagEmpty / GAMES)}`);
console.log(`\nEnd-state cell heights (share of all playable cells):`);
console.log(`  h=0 (empty): ${pct(heightHist[0] / cellsTot)}   h=1: ${pct(heightHist[1] / cellsTot)}   h=2: ${pct(heightHist[2] / cellsTot)}   h=3 (FULL): ${pct(heightHist[3] / cellsTot)}`);
console.log(`Full (depth-3) stacks: ${per(fullAtEnd)}/game · ${pct(gamesWithFull / GAMES)} of games have ≥1 full stack.`);
console.log(`\nBlocking (player holds chits but rotated wedge has NO placeable cell):`);
console.log(`  ${totBlocked} blocked player-turns over ${totPlayerTurns} (${pct(totBlocked / totPlayerTurns)}) · ${per(totBlocked)}/game`);
console.log(`  when blocked, # of the wedge's cells that were FULL: ` +
  blockedWedgeFullness.map((n, i) => `${i}${i === 5 ? "+" : ""}:${n}`).join(" "));
console.log(`Passes (no legal move at all): ${per(totPasses)}/game.\n`);
