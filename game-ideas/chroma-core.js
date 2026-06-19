/* Chroma — pure game logic (no DOM). Shared by the HTML mock and node tests.
 * Two selectable palettes; PROVISIONAL mixing (per-channel multiply of pale base
 * transmissions) — to be replaced by the calibrated table after the swatchbook shoot.
 * Geometry/seeding mirror render_board.py (edge 6 = 91 cells). */
(function (root) {
  "use strict";
  var SIZE = 1.0, SQRT3 = Math.sqrt(3), N = 6, R = N - 1;

  function axToPx(q, r) { return [SIZE * (SQRT3 * q + SQRT3 / 2 * r), SIZE * 1.5 * r]; }
  function key(q, r) { return q + "," + r; }
  function deg(rad) { return rad * 180 / Math.PI; }

  function pxToCell(x, y) {
    var r = y / (1.5 * SIZE);
    var q = (x / SIZE - SQRT3 / 2 * r) / SQRT3;
    var cx = q, cy = -q - r, cz = r;
    var rx = Math.round(cx), ry = Math.round(cy), rz = Math.round(cz);
    var dx = Math.abs(rx - cx), dy = Math.abs(ry - cy), dz = Math.abs(rz - cz);
    if (dx > dy && dx > dz) rx = -ry - rz;
    else if (dy > dz) ry = -rx - rz;
    else rz = -rx - ry;
    return [rx, rz];
  }

  var cells = [];
  for (var q = -R; q <= R; q++)
    for (var r = -R; r <= R; r++)
      if (Math.max(Math.abs(q), Math.abs(r), Math.abs(-q - r)) <= R) cells.push([q, r]);
  var cellSet = {};
  cells.forEach(function (c) { cellSet[key(c[0], c[1])] = true; });
  var NEI = [[1, 0], [1, -1], [0, -1], [-1, 0], [-1, 1], [0, 1]];

  function byAngle(list) {
    return list.map(function (c) {
      var p = axToPx(c[0], c[1]);
      return [(deg(Math.atan2(p[1], p[0])) + 360) % 360, c[0], c[1]];
    }).sort(function (a, b) { return a[0] - b[0]; });
  }
  var cornerCells = cells.filter(function (c) {
    var a = [Math.abs(c[0]), Math.abs(c[1]), Math.abs(-c[0] - c[1])].sort(function (x, y) { return x - y; });
    return a[0] === 0 && a[1] === R && a[2] === R;
  });
  var cornersSorted = byAngle(cornerCells);
  var vert = {};
  cornersSorted.forEach(function (c, i) { vert[i] = [c[1], c[2]]; });
  var nbSorted = byAngle(NEI);

  // ---- palettes ----
  // order = 6 keys in angular order, ALTERNATING primary,secondary,primary,...
  // secondary at idx 1,3,5 = mix of its two flanking primaries.
  var PALETTES = {
    RYB: {
      order: ["R", "O", "Y", "G", "B", "P"],
      trans: {
        R: [0.92, 0.30, 0.33], O: [0.95, 0.58, 0.26], Y: [0.97, 0.92, 0.32],
        G: [0.30, 0.84, 0.46], B: [0.30, 0.46, 0.93], P: [0.62, 0.34, 0.86]
      },
      chip: { R: "#e23b3b", O: "#ef8a2b", Y: "#f4d030", G: "#3fae54", B: "#2f6fd0", P: "#7a3fb0" },
      name: { R: "Red", O: "Orange", Y: "Yellow", G: "Green", B: "Blue", P: "Purple" }
    },
    CMY: {
      // angular order alternating prim(C,M,Y) / sec(B,R,G): C,B,M,R,Y,G
      order: ["C", "B", "M", "R", "Y", "G"],
      trans: {
        C: [0.10, 0.90, 0.92], B: [0.12, 0.14, 0.92], M: [0.92, 0.12, 0.90],
        R: [0.92, 0.13, 0.12], Y: [0.94, 0.92, 0.12], G: [0.12, 0.90, 0.20]
      },
      chip: { C: "#22c3d6", M: "#d63cae", Y: "#f4d030", R: "#e23b3b", G: "#3fae54", B: "#2f6fd0" },
      name: { C: "Cyan", M: "Magenta", Y: "Yellow", R: "Red", G: "Green", B: "Blue" }
    }
  };
  var TRANS_FIXED = { W: [1, 1, 1], K: [0.05, 0.05, 0.05] }; // W=clear, K=black

  var active = "CMY", PAL = PALETTES.CMY, seed = {}; // CMY default (cleaner closure)
  function buildSeed() {
    var o = PAL.order; seed = {};
    seed[key(0, 0)] = "K";
    nbSorted.forEach(function (c, i) { seed[key(c[1], c[2])] = o[i]; });
    var edges = [[1, 3, 2], [3, 5, 4], [5, 1, 0]];
    edges.forEach(function (e) {
      var A = vert[e[0]], Bc = vert[e[1]], prim = o[e[2]];
      var pa = axToPx(A[0], A[1]), pb = axToPx(Bc[0], Bc[1]);
      for (var t = 0; t <= 600; t++) {
        var f = t / 600;
        var cell = pxToCell(pa[0] + (pb[0] - pa[0]) * f, pa[1] + (pb[1] - pa[1]) * f);
        var k = key(cell[0], cell[1]);
        if (cellSet[k] && k !== key(0, 0)) seed[k] = prim;
      }
    });
    [1, 3, 5].forEach(function (i) { seed[key(vert[i][0], vert[i][1])] = o[i]; });
  }
  function setPalette(nm) { if (PALETTES[nm]) { active = nm; PAL = PALETTES[nm]; buildSeed(); } return active; }
  buildSeed();

  function sector(qq, rr) {
    var p = axToPx(qq, rr);
    var a = (deg(Math.atan2(p[1], p[0])) + 1e-6 + 360) % 360;
    return Math.floor(((a + 30) % 360) / 60);
  }
  // uniform depth 3 (capacity = 3 chits per cell; on a SEEDED cell the seed
  // counts as 1 of the 3 -> 2 placeable on top, blank cells take 3). This is
  // the LIVE value — chroma-mock.html calls setDepth(3); keep this default in
  // sync so headless sims match the real game. (Untrimmed board: 90 non-center
  // cells x 3 = 270 capacity; the 9-corner trim drops it to 81 x 3 = 243.)
  var DEPTH = 3;
  // Removed cells (board-shape experiment, Mohammad 2026-06-14): a set of cell
  // keys that are NOT part of the playable board — never seeded, never placeable,
  // capacity 0. Empty set = the full board (no behavior change). Used to prototype
  // trimming corner blanks to add a little placement friction.
  var removed = {};
  function setRemoved(keys) { removed = {}; (keys || []).forEach(function (k) { removed[k] = true; }); }
  function isRemoved(k) { return !!removed[k]; }
  function capacity(k) { return removed[k] ? 0 : DEPTH; }
  function setDepth(d) { DEPTH = d; }

  // ---- mixing ----
  var REL_LUM_MIN = 0.14;  // perceived luminance below this => too dark => mud
  var PURITY_MAX = 0.105;  // normalized distance to nearest base above this => ambiguous => mud
  function relLum(rgb) { return 0.2126 * rgb[0] + 0.7152 * rgb[1] + 0.0722 * rgb[2]; }
  function trans(ck) { return PAL.trans[ck] || TRANS_FIXED[ck] || [1, 1, 1]; }
  function mixStack(stack) {
    var rgb = [1, 1, 1];
    stack.forEach(function (ck) { var t = trans(ck); rgb[0] *= t[0]; rgb[1] *= t[1]; rgb[2] *= t[2]; });
    return rgb;
  }
  function classify(rgb) {
    var mx = Math.max(rgb[0], rgb[1], rgb[2]);
    if (relLum(rgb) < REL_LUM_MIN) return "mud";
    if (rgb[0] > 0.85 && rgb[1] > 0.85 && rgb[2] > 0.85) return "white";
    var nr = [rgb[0] / mx, rgb[1] / mx, rgb[2] / mx], best = null, bd = 1e9;
    PAL.order.forEach(function (c) {
      var b = PAL.trans[c], bmx = Math.max(b[0], b[1], b[2]);
      var nb = [b[0] / bmx, b[1] / bmx, b[2] / bmx];
      var d = (nr[0] - nb[0]) * (nr[0] - nb[0]) + (nr[1] - nb[1]) * (nr[1] - nb[1]) + (nr[2] - nb[2]) * (nr[2] - nb[2]);
      if (d < bd) { bd = d; best = c; }
    });
    return bd > PURITY_MAX ? "mud" : best;
  }
  // independent re-derivation (different code path) for the verifier:
  // works in absorption space, sums, reconstructs transmission, classifies.
  function classifyIndependent(stack) {
    var absR = 0, absG = 0, absB = 0;
    stack.forEach(function (ck) {
      var t = trans(ck);
      absR += -Math.log(Math.max(t[0], 1e-4));
      absG += -Math.log(Math.max(t[1], 1e-4));
      absB += -Math.log(Math.max(t[2], 1e-4));
    });
    var rgb = [Math.exp(-absR), Math.exp(-absG), Math.exp(-absB)];
    return classify(rgb);
  }
  function resultOf(stack) {
    var rgb = mixStack(stack);
    return { rgb: rgb, color: classify(rgb), check: classifyIndependent(stack) };
  }
  function displayRGB(stack) {
    if (!stack.length) return [245, 245, 242];
    var rgb = mixStack(stack);
    return [Math.round(rgb[0] * 255), Math.round(rgb[1] * 255), Math.round(rgb[2] * 255)];
  }

  // ---- scoring ----
  function boardColors(stacks) {
    var m = {};
    cells.forEach(function (c) {
      var k = key(c[0], c[1]), st = stacks[k] || [];
      m[k] = st.length ? classify(mixStack(st)) : "white";
    });
    return m;
  }
  function largestRegions(stacks) {
    var col = boardColors(stacks), seen = {}, best = {};
    PAL.order.forEach(function (c) { best[c] = 0; });
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
      if (size > best[color]) best[color] = size;
    });
    return best;
  }
  // top-2 largest region sizes per color (op2b needs the 2nd for tie payouts)
  function largestTwo(stacks) {
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
  // mud as a scorable quantity: TOTAL mud cells (all mud anywhere, contiguous or
  // not, incl. the black center) and the LARGEST contiguous mud region.
  function mudRegions(stacks) {
    var col = boardColors(stacks), seen = {}, total = 0, largest = 0;
    cells.forEach(function (c) { if (col[key(c[0], c[1])] === "mud") total++; });
    cells.forEach(function (c) {
      var k0 = key(c[0], c[1]);
      if (seen[k0] || col[k0] !== "mud") return;
      var st = [[c[0], c[1]]], size = 0; seen[k0] = true;
      while (st.length) {
        var cur = st.pop(); size++;
        NEI.forEach(function (d) {
          var nk = key(cur[0] + d[0], cur[1] + d[1]);
          if (cellSet[nk] && !seen[nk] && col[nk] === "mud") { seen[nk] = true; st.push([cur[0] + d[0], cur[1] + d[1]]); }
        });
      }
      if (size > largest) largest = size;
    });
    return { total: total, largest: largest };
  }
  function tier(n) { return (n >= 1 ? 1 : 0) + (n >= 4 ? 1 : 0) + (n >= 8 ? 1 : 0) + (n >= 12 ? 1 : 0); }
  // scoreHand (the OLD `base` rule, kept for reference/back-compat ONLY): a color
  // scores its largest-region size if you hold ≥1 chit. NOT the live rule.
  function scoreHand(hand, regions) {
    var size = 0, add = 0, cnt = {};
    hand.forEach(function (c) { cnt[c] = (cnt[c] || 0) + 1; });
    PAL.order.forEach(function (c) { if ((cnt[c] || 0) > 0) { size += regions[c]; add += tier(regions[c]); } });
    return { mult: size, add: add, size: size };
  }
  // === LIVE scoring rule: op2b (Mohammad, designed+tested 2026-06-09, merged
  // 2026-06-14). A color's region scores ONLY for the player holding the MOST
  // chits of that color. A sole leader scores the FULL largest region; on a tie
  // for most, each tied player scores the SECOND-largest region of that color
  // (the "second-largest-region tiebreaker"). A color no one holds scores for no
  // one. This is a GAME-WIDE pass — it must see every player's hand — which is
  // why per-hand scoreHand could never express it. Returns one row per player.
  function scoreGame(players, stacks) {
    var reg = largestRegions(stacks), two = largestTwo(stacks), ord = PAL.order, NP = players.length;
    var cnts = players.map(function (p) { var c = {}; ord.forEach(function (x) { c[x] = 0; }); p.hand.forEach(function (x) { if (c[x] !== undefined) c[x]++; }); return c; });
    var mult = players.map(function () { return 0; }), add = players.map(function () { return 0; });
    var colorBreak = players.map(function () { return []; });   // per-player [{color,value,tie}]
    ord.forEach(function (color) {
      var mx = 0; for (var i = 0; i < NP; i++) if (cnts[i][color] > mx) mx = cnts[i][color];
      if (mx <= 0) return;                                   // nobody holds it -> unclaimed
      var holders = []; for (var j = 0; j < NP; j++) if (cnts[j][color] === mx) holders.push(j);
      if (holders.length === 1) { var w = holders[0]; mult[w] += reg[color]; add[w] += tier(reg[color]); colorBreak[w].push({ color: color, value: reg[color], tie: false }); }
      else holders.forEach(function (h) { var v = two.second[color]; mult[h] += v; add[h] += tier(v); colorBreak[h].push({ color: color, value: v, tie: true }); });   // tie -> 2nd-largest each
    });
    // Mud scoring (Mohammad, 2026-06-14, v2): mud is a sixth "region" owned by the
    // player with the FEWEST chits in hand (the most spent-down / aggressive
    // player). A sole fewest-hand player scores the TOTAL mud-cell count (all mud
    // anywhere, contiguous or not, incl. the center). On a TIE for fewest, each
    // tied player instead scores only the LARGEST contiguous mud region. Total mud
    // is small (~2), so this BREAKS TIES between close region players rather than
    // being a standalone scoring juggernaut. No other player scores mud.
    var mr = mudRegions(stacks);
    var minHand = Infinity; for (var a = 0; a < NP; a++) if (players[a].hand.length < minHand) minHand = players[a].hand.length;
    var mudHolders = []; for (var b = 0; b < NP; b++) if (players[b].hand.length === minHand) mudHolders.push(b);
    var mudScore = players.map(function () { return 0; });
    var mudBreak = players.map(function () { return null; });   // per-player {value,tie} or null
    var mudTie = mudHolders.length > 1;
    if (!mudTie) { mudScore[mudHolders[0]] = mr.total; mudBreak[mudHolders[0]] = { value: mr.total, tie: false }; }   // sole fewest -> ALL mud
    else mudHolders.forEach(function (h) { mudScore[h] = mr.largest; mudBreak[h] = { value: mr.largest, tie: true }; });   // tie -> largest mud region each
    return players.map(function (p, i) {
      // tiebreaker (Mohammad 2026-06-14): # of DIFFERENT region types actually scored
      // (colors with value>0, + mud if scored) — rewards breadth of scoring on a tie.
      var scoredTypes = colorBreak[i].filter(function (cb) { return cb.value > 0; }).length + ((mudBreak[i] && mudBreak[i].value > 0) ? 1 : 0);
      return { mult: mult[i] + mudScore[i], add: add[i], size: mult[i] + mudScore[i],
               regionScore: mult[i], mudScore: mudScore[i], boardMud: mr.total, mudLargest: mr.largest,
               breakdown: { colors: colorBreak[i], mud: mudBreak[i] }, scoredTypes: scoredTypes,
               distinct: new Set(p.hand).size, dry: p.dry, swaps: p.swaps, handLen: p.hand.length, target: p.target };
    });
  }

  // ---- color-wheel swaps (Mohammad, 2026-06-08) ----
  // PAL.order is the 6 colors in angular order, alternating primary/secondary.
  // opposite = 3 steps (180°). between = the single color on the short arc when
  // the two chits are exactly 2 steps apart (two primaries -> the secondary
  // between them, two secondaries -> the primary between them). Adjacent (1 step)
  // or opposite (3 steps) pairs have no clean "between" and return null.
  function wheelOpposite(c) { var o = PAL.order, i = o.indexOf(c); return i < 0 ? null : o[(i + 3) % 6]; }
  function wheelBetween(a, b) {
    var o = PAL.order, i = o.indexOf(a), j = o.indexOf(b);
    if (i < 0 || j < 0 || i === j) return null;
    var d = ((j - i) + 6) % 6;                 // forward distance i->j, 1..5
    if (d > 3) { var t = i; i = j; j = t; d = 6 - d; }  // fold to short arc, i = lower end
    if (d !== 2) return null;                  // only 2-apart pairs have a clean midpoint
    return o[(i + 1) % 6];
  }

  // ===========================================================================
  // ENGINE — the SINGLE shared path consumed by BOTH the live mock and the
  // headless GA bench. Humans and bots traverse the SAME functions:
  //   enumerateMoves  -> the presented choice set (what the human picks from
  //                      and what the bot scores — one move set, never two)
  //   decide          -> a PURE weighted selector (genome = input) that returns
  //                      a chosen move; it MUTATES NOTHING
  //   applyPlacement  -> THE ONE mutator: removes the placed chit from hand AND
  //                      pushes it onto the stack. Humans and bots both go
  //                      through it, so the hand can never diverge between them.
  // This is the Chroma port of Eridu's bot/human unification: the bot is a
  // decision tool over the human's choices, not a parallel code path.
  // ===========================================================================
  var MUD_LIMIT = 3, START_HAND = 6, PER_COLOR = 30;
  var PRIMARIES = ["C", "M", "Y"], SECONDARIES = ["R", "G", "B"];

  function mulberry32(a) {
    return function () {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      var t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }
  function basesFor(n) { var b = []; for (var i = 0; i < n; i++) b.push(Math.round(i * 6 / n) % 6); return b; }
  function isCenter(c) { return c[0] === 0 && c[1] === 0; }

  var RING = {}, maxRing = 0;
  cells.forEach(function (c) {
    var r = Math.max(Math.abs(c[0]), Math.abs(c[1]), Math.abs(-c[0] - c[1]));
    RING[key(c[0], c[1])] = r; if (r > maxRing) maxRing = r;
  });
  function wedgeOf(p, turn) { return (p.base + turn) % 6; }

  function seedCount() { var m = {}; PAL.order.forEach(function (c) { m[c] = 0; }); Object.keys(seed).forEach(function (k) { if (m[seed[k]] !== undefined) m[seed[k]]++; }); return m; }
  function bagTotal(bag) { var t = 0; PAL.order.forEach(function (c) { t += bag[c]; }); return t; }
  function drawWeighted(bag, rng) { var t = bagTotal(bag); if (t <= 0) return null; var r = rng() * t; for (var i = 0; i < PAL.order.length; i++) { var c = PAL.order[i]; r -= bag[c]; if (r < 0) { bag[c]--; return c; } } return null; }

  function legalCells(stacks, w) {
    var out = [];
    for (var i = 0; i < cells.length; i++) {
      var c = cells[i]; if (isCenter(c)) continue; if (sector(c[0], c[1]) !== w) continue;
      var k = key(c[0], c[1]); if (removed[k]) continue; if (stacks[k].length < capacity(k)) out.push(c);
    }
    return out;
  }

  // full connected-component region labelling (cellKey -> regionId, regionId -> size)
  function labelRegions(stacks) {
    var col = boardColors(stacks), lab = {}, size = [], id = 0;
    for (var ci = 0; ci < cells.length; ci++) {
      var c = cells[ci], k0 = key(c[0], c[1]);
      if (lab[k0] !== undefined) continue;
      var color = col[k0];
      if (PAL.order.indexOf(color) < 0) { lab[k0] = -1; continue; }
      var stack = [[c[0], c[1]]], n = 0; lab[k0] = id;
      while (stack.length) {
        var cur = stack.pop(); n++;
        for (var d = 0; d < NEI.length; d++) {
          var nk = key(cur[0] + NEI[d][0], cur[1] + NEI[d][1]);
          if (cellSet[nk] && lab[nk] === undefined && col[nk] === color) { lab[nk] = id; stack.push([cur[0] + NEI[d][0], cur[1] + NEI[d][1]]); }
        }
      }
      size[id] = n; id++;
    }
    return { col: col, lab: lab, size: size };
  }

  function majority(hand) { var cnt = {}, best = PAL.order[0], bv = -1; PAL.order.forEach(function (c) { cnt[c] = 0; }); hand.forEach(function (c) { cnt[c]++; }); PAL.order.forEach(function (c) { if (cnt[c] > bv) { bv = cnt[c]; best = c; } }); return best; }

  // ---- genome schema: the bot's weight vector is an INPUT to `decide` ----
  var GENES = {
    cmyFocus: [-1, 2], rgbFocus: [-1, 2], colorLock: [0, 1], mudRush: [0, 2],
    blankPriority: [-1, 2], capPriority: [-1, 2], edgeCenterPref: [-1, 1],
    bridgeWeight: [0, 3], growRegion: [0, 2], targetDraw: [0, 3], anyDraw: [0, 2],
    spendTargetPen: [0, 3], dryAversion: [0, 3], mudAversion: [0, 3],
    earlySwap: [0, 2], lateSwap: [0, 2],
    // placement-style (Mohammad 2026-06-14): high = tight A/B/A/B cycle (replay the
    // color you placed 2 turns ago, the one the no-same-color rule lets you reuse);
    // ~0 = broaden — use other colors to time which color lands on which wedge.
    cycleLock: [0, 2]
  };
  var GENE_KEYS = Object.keys(GENES);
  function clampGene(k, v) { var r = GENES[k]; return Math.max(r[0], Math.min(r[1], v)); }
  function randomGenome(rng) { var g = {}; GENE_KEYS.forEach(function (k) { var r = GENES[k]; g[k] = r[0] + (r[1] - r[0]) * rng(); }); return g; }
  // a balanced, competent default (used for mock bots): draw + survive backbone.
  function defaultGenome() {
    var g = {}; GENE_KEYS.forEach(function (k) { g[k] = 0; });
    g.anyDraw = 1; g.targetDraw = 2; g.mudAversion = 2; g.dryAversion = 1.5;
    g.growRegion = 1; g.spendTargetPen = 1.5; g.blankPriority = 1; g.bridgeWeight = 1;
    g.lateSwap = 1.2; g.earlySwap = 0.2;
    return g;
  }
  function chooseLockTarget(hand, g) {
    var cnt = {}; PAL.order.forEach(function (c) { cnt[c] = 0; }); hand.forEach(function (c) { cnt[c]++; });
    var best = null, bv = -1e9;
    PAL.order.forEach(function (c) {
      var s = cnt[c];
      if (PRIMARIES.indexOf(c) >= 0) s += g.cmyFocus;
      if (SECONDARIES.indexOf(c) >= 0) s += g.rgbFocus;
      if (s > bv) { bv = s; best = c; }
    });
    return best;
  }

  // ---- the presented choice set: legal cells (in this player's wedge) × the
  // distinct chits in hand. The SAME set the human UI renders and the bot scores.
  function enumerateMoves(G, pi) {
    var p = G.players[pi], w = wedgeOf(p, G.turn);
    var opts = legalCells(G.stacks, w);
    if (!opts.length || !p.hand.length) return [];
    var uniqAll = [], seen = {}; p.hand.forEach(function (c) { if (!seen[c]) { seen[c] = 1; uniqAll.push(c); } });
    // Rule (Mohammad, 2026-06-14): you may NOT place the same color you placed on
    // your previous turn — forces hand diversification + interaction. NO fallback:
    // if your whole hand is that one color you have NO legal placement and are
    // FORCED to skip placement+draw this turn. You can still SWAP afterward to
    // rebuild your hand and re-enter next turn (lastPlaced is unchanged by a skip).
    var uniq = uniqAll.filter(function (c) { return c !== p.lastPlaced; });
    var moves = [];
    for (var i = 0; i < opts.length; i++) { var c = opts[i], k = key(c[0], c[1]); for (var j = 0; j < uniq.length; j++) moves.push({ c: c, chit: uniq[j], k: k }); }
    return moves;
  }

  // ---- NO-CONSECUTIVE-PASS rule (Mohammad, 2026-06-14, closes the G3 "pass-to-win"
  // exploit). A player may NOT pass on two of their own turns in a row. The ONLY
  // legal back-to-back pass is a FORCED one: you hold 0 chits, OR (by the no-replay-
  // color rule) you have no legal placement at all. With 1+ chits in hand AND a legal
  // placement available, a second straight pass is ILLEGAL — you MUST place. p.passedLast
  // (set each turn in step()/the mock's finalizeSwaps) records whether THIS player's
  // previous turn was a pass; undefined (e.g. legacy saved games) reads as false = "may pass".
  // Returns true if player pi may legally pass this turn.
  function canPass(G, pi) {
    var p = G.players[pi];
    if (!p.passedLast) return true;                       // first pass of a streak is always fine
    if (p.hand.length === 0) return true;                 // forced: empty hand (the deliberate dead end)
    if (enumerateMoves(G, pi).length === 0) return true;  // forced: no legal placement (color-rule lockout)
    return false;                                         // holding chits + a legal move -> placement is MANDATORY
  }

  // ---- PURE weighted selector. weights live on p.g (the genome). NO mutation;
  // returns the chosen {c, chit} or null. (Mutation happens only in applyPlacement.)
  function decide(G, pi) {
    var p = G.players[pi], g = p.g;
    var moves = enumerateMoves(G, pi);
    if (!moves.length) return null;
    p.target = g.colorLock > 0.5 ? p.fixed : majority(p.hand);
    var tgt = p.target;
    var cnt = {}; p.hand.forEach(function (c) { cnt[c] = (cnt[c] || 0) + 1; });
    var ord = PAL.order, tp = ord.indexOf(tgt);
    var ingredients = {}; [wheelOpposite(tgt), ord[(tp + 5) % 6], ord[(tp + 1) % 6]].forEach(function (x) { if (x) ingredients[x] = 1; });
    var drv = swapDrive(G, g);
    var lr = labelRegions(G.stacks), col = lr.col, lab = lr.lab, size = lr.size;
    var bt = bagTotal(G.bag);
    var best = null, bestScore = -1e18;
    for (var mi = 0; mi < moves.length; mi++) {
      var c = moves[mi].c, chit = moves[mi].chit, k = moves[mi].k, st = G.stacks[k], cap = capacity(k);
      var R = classify(mixStack(st.concat([chit])));
      var s = 0;
      if (R === "mud") {
        s = -8 * g.mudAversion + 6 * g.mudRush;
      } else {
        s += g.anyDraw;
        if (R === tgt) s += g.targetDraw;
        if (chit === tgt) s -= g.spendTargetPen;
        if (PRIMARIES.indexOf(R) >= 0) s += g.cmyFocus;
        if (SECONDARIES.indexOf(R) >= 0) s += g.rgbFocus;
        var scarce = G.bag[R] / Math.max(1, bt);
        if (G.bag[R] <= 0) s -= 3 * g.dryAversion;
        else if (scarce < 0.06) s -= g.dryAversion;
        var adjRegions = {}, adjCount = 0, adjSize = 0;
        for (var d = 0; d < NEI.length; d++) {
          var nk = key(c[0] + NEI[d][0], c[1] + NEI[d][1]);
          if (cellSet[nk] && col[nk] === R && lab[nk] >= 0 && !adjRegions[lab[nk]]) { adjRegions[lab[nk]] = 1; adjCount++; adjSize += size[lab[nk]]; }
        }
        if (R === tgt && adjSize > 0) s += g.growRegion * Math.min(adjSize, 12) / 4;
        if (adjCount >= 2) s += g.bridgeWeight * (adjCount - 1);
      }
      if (drv > 0.5 && ingredients[chit] && (cnt[chit] || 0) <= 2) s -= 0.3 * drv;
      if (st.length === 0) s += g.blankPriority;
      if (st.length === cap - 1) s += g.capPriority;
      s += g.edgeCenterPref * ((RING[k] / maxRing) - 0.5) * 2;
      if (g.cycleLock && chit === p.lastPlaced2) s += g.cycleLock;   // tight A/B/A/B cycle vs broaden
      s += G.rng() * 0.01;
      if (s > bestScore) { bestScore = s; best = { c: c, chit: chit }; }
    }
    return best;
  }

  // ---- THE single mutator. Removes the placed chit from the player's hand and
  // pushes it onto the stack. Used by humans AND bots — there is no second path.
  function applyPlacement(G, pi, move) {
    var c = move.c, color = move.chit, k = key(c[0], c[1]);
    var prev = G.stacks[k].length ? classify(mixStack(G.stacks[k])) : "white";
    var hand = G.players[pi].hand, idx = hand.indexOf(color);
    if (idx >= 0) hand.splice(idx, 1);     // <- the line whose absence was the bug
    G.players[pi].lastPlaced2 = G.players[pi].lastPlaced;   // 2-turns-ago color (cycleLock gene)
    G.players[pi].lastPlaced = color;       // for the no-same-color-twice rule
    G.stacks[k].push(color);
    var Rr = resultOf(G.stacks[k]);
    return { pi: pi, k: k, c: c, color: color, prev: prev, res: Rr.color, drew: null, dry: null, mismatch: Rr.color !== Rr.check };
  }

  // Phase 2: resolve all draws together. If a color is over-claimed this turn the
  // bag can't satisfy, NO claimant draws it (conflict); a lone claimant of an
  // empty pile goes dry (empty); a mud result draws nothing (mud). All feed the
  // one dry clock (p.dry) and the dry-type tally.
  function resolveDraws(G, plc) {
    var demand = {};
    plc.forEach(function (pp) { if (PAL.order.indexOf(pp.res) >= 0) (demand[pp.res] = demand[pp.res] || []).push(pp); });
    Object.keys(demand).forEach(function (c) {
      var claim = demand[c], avail = G.bag[c];
      if (avail >= claim.length) { claim.forEach(function (pp) { G.bag[c]--; G.players[pp.pi].hand.push(c); pp.drew = c; }); }
      else { claim.forEach(function (pp) { pp.dry = (avail === 0 && claim.length === 1) ? "empty" : "conflict"; G.players[pp.pi].dry++; if (G.dryEvents) G.dryEvents[pp.dry]++; }); }
    });
    plc.forEach(function (pp) { if (pp.res === "mud") { pp.dry = "mud"; G.players[pp.pi].dry++; if (G.dryEvents) G.dryEvents.mud++; } });
  }

  // ---- color-wheel hand swaps (shared) ----
  function gameProgress(G) { return G.ending ? 1 : Math.min(1, G.turn / 20); }
  function swapDrive(G, g) { var t = gameProgress(G); return g.earlySwap * (1 - t) + g.lateSwap * t; }
  function coverage(hand, reg) { var s = 0; var seen = {}; hand.forEach(function (c) { if (!seen[c]) { seen[c] = 1; s += reg[c] || 0; } }); return s; }
  var SWAP_THRESH = 2.0;
  // best legal swap = only swap to gain a NEW color's region WITHOUT dropping any
  // held color to zero (surplus duplicates only) — correct under size-scoring.
  function wheelNeighbors(X) { var i = PAL.order.indexOf(X); return i < 0 ? [] : [PAL.order[(i + 5) % 6], PAL.order[(i + 1) % 6]]; }
  // mudded = made mud this turn -> swap is UPGRADED to discard only 1 (net-0):
  //   discard 1 X -> its wheel-opposite, OR discard 1 X -> either wheel-neighbor.
  // else normal: discard 2-same -> opposite, or 2-different-2-apart -> between.
  function bestSwap(hand, reg, bag, target, mudded) {
    var cnt = {}; PAL.order.forEach(function (c) { cnt[c] = 0; }); hand.forEach(function (c) { cnt[c]++; });
    var best = null;
    var consider = function (type, discards, get) {
      if (!get || !(bag[get] > 0) || cnt[get] > 0) return;       // gain a NEW color in stock
      var after = Object.assign({}, cnt);
      for (var i = 0; i < discards.length; i++) { if (!(after[discards[i]] > 0)) return; after[discards[i]]--; }
      for (var j = 0; j < discards.length; j++) if (after[discards[j]] === 0) return;  // surplus only (don't drop a held color)
      var gain = (reg[get] || 0) + (get === target ? 0.25 : 0);
      if (gain > 0 && (!best || gain > best.gain)) best = { type: type, discards: discards, get: get, gain: gain };
    };
    if (mudded) {
      PAL.order.forEach(function (X) { consider("opposite", [X], wheelOpposite(X)); });
      PAL.order.forEach(function (X) { wheelNeighbors(X).forEach(function (nb) { consider("adjacent", [X], nb); }); });
    } else {
      PAL.order.forEach(function (X) { if (cnt[X] >= 3) consider("opposite", [X, X], wheelOpposite(X)); });
      var held = PAL.order.filter(function (c) { return cnt[c] > 0; });
      for (var i = 0; i < held.length; i++) for (var j = i + 1; j < held.length; j++) consider("between", [held[i], held[j]], wheelBetween(held[i], held[j]));
    }
    return best;
  }
  function decideSwap(G, pi, reg, mudded) {
    var p = G.players[pi], g = p.g;
    // MUDDED upgrade: the swap is net-0 this turn (discard 1, gain 1), so take any
    // genuinely coverage-positive upgraded swap regardless of the usual hand-size
    // / drive thresholds — this is the "consolidate the cost" lateral pivot.
    if (mudded) {
      var swm = bestSwap(p.hand, reg, G.bag, p.target, true);
      if (swm && swm.gain >= 1) return { pi: pi, type: swm.type, discards: swm.discards, get: swm.get };
      return null;
    }
    // RESCUE: if the no-same-color rule left this player with NO legal placement
    // this turn (every distinct held color == lastPlaced) yet a cell was open,
    // use the swap to introduce a DIFFERENT color so they can re-enter next turn
    // — bypassing the normal hand-size/drive thresholds. This is the "cash in
    // chits after skipping to get back in" valve (Mohammad, 2026-06-14).
    var blocked = p.hand.length > 0 && enumerateMoves(G, pi).length === 0 && legalCells(G.stacks, wedgeOf(p, G.turn)).length > 0;
    if (blocked) {
      var rescue = bestSwap(p.hand, reg, G.bag, p.target);   // prefer a coverage-positive swap
      if (!rescue) {                                          // else any 2-same -> opposite (a new color)
        var rc = {}; PAL.order.forEach(function (c) { rc[c] = 0; }); p.hand.forEach(function (c) { rc[c]++; });
        for (var ri = 0; ri < PAL.order.length; ri++) { var X = PAL.order[ri], opp = wheelOpposite(X); if (rc[X] >= 2 && opp && G.bag[opp] > 0) { rescue = { type: "opposite", discards: [X, X], get: opp }; break; } }
      }
      if (rescue) return { pi: pi, type: rescue.type, discards: rescue.discards, get: rescue.get };
    }
    if (p.hand.length < 4) return null;
    var drive = swapDrive(G, g);
    if (drive <= 0) return null;
    var sw = bestSwap(p.hand, reg, G.bag, p.target);
    if (!sw || sw.gain < 1 || sw.gain * drive < SWAP_THRESH) return null;
    return { pi: pi, type: sw.type, discards: sw.discards, get: sw.get };
  }
  // every LEGAL swap available to a hand (for the human UI): 2-same -> opposite,
  // 2-different 2-apart -> between. `playable` = the gained color is in stock.
  function availableSwaps(hand, bag, stacks, mudded) {
    var cnt = {}; PAL.order.forEach(function (c) { cnt[c] = 0; }); hand.forEach(function (c) { cnt[c]++; });
    var out = [], seen = {}, reg = largestRegions(stacks);
    var add = function (type, discards, get) {
      if (!get) return;
      var kk = discards.slice().sort().join("+") + ">" + get + ":" + type; if (seen[kk]) return; seen[kk] = 1;
      out.push({ pi: 0, type: type, discards: discards, get: get, playable: bag[get] > 0, gain: (reg[get] || 0) + (cnt[get] ? 0 : 0.01) });
    };
    if (mudded) {   // UPGRADED (made mud this turn): 1-discard, net-0 swaps
      PAL.order.forEach(function (X) { if (cnt[X] >= 1) add("opposite", [X], wheelOpposite(X)); });
      PAL.order.forEach(function (X) { if (cnt[X] >= 1) wheelNeighbors(X).forEach(function (nb) { add("adjacent", [X], nb); }); });
    } else {
      PAL.order.forEach(function (X) { if (cnt[X] >= 2) add("opposite", [X, X], wheelOpposite(X)); });
      var held = PAL.order.filter(function (c) { return cnt[c] > 0; });
      for (var i = 0; i < held.length; i++) for (var j = i + 1; j < held.length; j++) add("between", [held[i], held[j]], wheelBetween(held[i], held[j]));
    }
    out.sort(function (a, b) { return (b.playable - a.playable) || (b.gain - a.gain); });
    return out;
  }
  // resolve all swaps together; gained chit drawn from the bag, so a contested
  // color short in supply is denied to everyone that turn (same ethos as draws).
  function resolveSwaps(G, plans) {
    var demand = {};
    plans.forEach(function (s) { (demand[s.get] = demand[s.get] || []).push(s); });
    var granted = [], denied = [];
    Object.keys(demand).forEach(function (c) {
      var claim = demand[c];
      if (G.bag[c] >= claim.length) claim.forEach(function (s) {
        var h = G.players[s.pi].hand;
        for (var d = 0; d < s.discards.length; d++) h.splice(h.indexOf(s.discards[d]), 1);
        G.bag[c]--; h.push(c);
        if (G.swapEvents) G.swapEvents[s.type] = (G.swapEvents[s.type] || 0) + 1;
        G.players[s.pi].swaps = (G.players[s.pi].swaps || 0) + 1;
        G.players[s.pi].discarded = (G.players[s.pi].discarded || 0) + s.discards.length;  // chits that left play (1 or 2)
        granted.push(s);
      });
      else claim.forEach(function (s) { if (G.swapEvents) G.swapEvents.denied++; denied.push(s); });
    });
    granted.denied = denied;     // attach for callers that want to log denials
    return granted;
  }

  // ---- game construction + a fully-autonomous turn (used by the bench) ----
  // playerSpecs: [{isBot, g}]. Humans pass isBot:false (their g is unused — they
  // pick a move via the UI, then feed it to the SAME applyPlacement).
  function newGame(playerSpecs, seedVal) {
    var rng = mulberry32(seedVal >>> 0), N = playerSpecs.length;
    var stacks = {}; cells.forEach(function (c) { var k = key(c[0], c[1]); stacks[k] = (seed[k] && !removed[k]) ? [seed[k]] : []; });
    var sc = seedCount(), bag = {}; PAL.order.forEach(function (c) { bag[c] = PER_COLOR - (sc[c] || 0); });
    var bs = basesFor(N);
    var players = playerSpecs.map(function (spec, i) { return { g: spec.g || defaultGenome(), hand: [], dry: 0, swaps: 0, discarded: 0, base: bs[i], isBot: !!spec.isBot, target: null, fixed: null, lastPlaced: null, lastPlaced2: null, passedLast: false }; });
    for (var n = 0; n < START_HAND; n++) players.forEach(function (p) { var dd = drawWeighted(bag, rng); if (dd) p.hand.push(dd); });
    // initialize fixed/target for EVERY seat (p.g is never null — humans get a
    // defaultGenome above). Unconditional so decide() is safe even if it is ever
    // called on a human/empty seat (AI-hint, fill-seat-with-bot), and so this
    // matches the pre-refactor init exactly. (adversarial-review hardening, 2026-06-13)
    players.forEach(function (p) { p.fixed = chooseLockTarget(p.hand, p.g); p.target = p.fixed; });
    return { rng: rng, N: N, stacks: stacks, bag: bag, players: players, turn: 0, over: false, ending: false, finalTurn: -1, moves: [], dryEvents: { mud: 0, empty: 0, conflict: 0 }, swapEvents: { opposite: 0, between: 0, denied: 0 } };
  }
  // one fully-autonomous simultaneous turn (all players are bots). The live mock
  // does NOT call this — it composes the same primitives around its async human
  // swap prompt — but it shares enumerate/decide/applyPlacement/resolveDraws/swaps.
  function step(G, record) {
    var plc = [];
    for (var i = 0; i < G.N; i++) { var d = decide(G, i); if (d) plc.push(applyPlacement(G, i, d)); }
    resolveDraws(G, plc);
    var mudded = {}; plc.forEach(function (p) { if (p.res === "mud") mudded[p.pi] = true; });  // made mud -> upgraded swap
    var reg = largestRegions(G.stacks);
    var swapPlans = [];
    for (var i2 = 0; i2 < G.N; i2++) { var s = decideSwap(G, i2, reg, !!mudded[i2]); if (s) swapPlans.push(s); }
    var swaps = swapPlans.length ? resolveSwaps(G, swapPlans) : [];
    // record who passed THIS turn (placed nothing) for next turn's no-consecutive-pass
    // check (canPass). Bots only ever pass when forced (decide returns null = no legal
    // move), so this is pure bookkeeping for them; it's the human Pass button that the
    // rule actually gates — but the engine tracks it for every seat uniformly.
    var placed = {}; plc.forEach(function (p) { placed[p.pi] = true; });
    for (var pj = 0; pj < G.N; pj++) G.players[pj].passedLast = !placed[pj];
    if (record) G.moves.push({ turn: G.turn, plays: plc.map(function (p) { return { pi: p.pi, cell: p.k, placed: p.color, prev: p.prev, res: p.res, drew: p.drew, dry: p.dry }; }), swaps: swaps.map(function (s) { return { pi: s.pi, type: s.type, discards: s.discards, get: s.get }; }) });
    G.turn++;
    if (G.over) return;
    if (G.ending && G.turn > G.finalTurn) { G.over = true; return; }
    var maxDry = Math.max.apply(null, G.players.map(function (p) { return p.dry; })), bt = bagTotal(G.bag);
    if (!G.ending && (maxDry >= MUD_LIMIT || bt === 0)) { G.ending = true; G.finalTurn = G.turn; }
  }

  var API = {
    SIZE: SIZE, cells: cells, cellSet: cellSet, key: key, axToPx: axToPx, NEI: NEI,
    sector: sector, capacity: capacity, setDepth: setDepth,
    setRemoved: setRemoved, isRemoved: isRemoved, get removed() { return removed; },
    get seed() { return seed; }, get palette() { return active; }, get PAL() { return PAL; },
    PALETTES: PALETTES, setPalette: setPalette,
    mixStack: mixStack, classify: classify, classifyIndependent: classifyIndependent,
    resultOf: resultOf, displayRGB: displayRGB,
    boardColors: boardColors, largestRegions: largestRegions, largestTwo: largestTwo, mudRegions: mudRegions, scoreHand: scoreHand, scoreGame: scoreGame,
    wheelOpposite: wheelOpposite, wheelBetween: wheelBetween,
    thresholds: function () { return { REL_LUM_MIN: REL_LUM_MIN, PURITY_MAX: PURITY_MAX }; },
    // ---- shared engine (one path for humans + bots, mock + bench) ----
    MUD_LIMIT: MUD_LIMIT, START_HAND: START_HAND, PER_COLOR: PER_COLOR,
    PRIMARIES: PRIMARIES, SECONDARIES: SECONDARIES,
    GENES: GENES, GENE_KEYS: GENE_KEYS, clampGene: clampGene,
    randomGenome: randomGenome, defaultGenome: defaultGenome,
    mulberry32: mulberry32, bases: basesFor, isCenter: isCenter, RING: RING, maxRing: maxRing,
    wedgeOf: wedgeOf, seedCount: seedCount, bagTotal: bagTotal, drawWeighted: drawWeighted,
    legalCells: legalCells, labelRegions: labelRegions, majority: majority, chooseLockTarget: chooseLockTarget,
    enumerateMoves: enumerateMoves, canPass: canPass, decide: decide, applyPlacement: applyPlacement, resolveDraws: resolveDraws,
    gameProgress: gameProgress, swapDrive: swapDrive, coverage: coverage,
    bestSwap: bestSwap, decideSwap: decideSwap, availableSwaps: availableSwaps, resolveSwaps: resolveSwaps,
    newGame: newGame, step: step
  };
  if (typeof module !== "undefined" && module.exports) module.exports = API;
  else root.Chroma = API;
})(typeof window !== "undefined" ? window : this);
