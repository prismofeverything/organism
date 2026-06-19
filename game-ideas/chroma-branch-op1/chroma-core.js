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
  // uniform depth 4 (seeded cells: seed counts as 1 of the 4 -> 3 placeable on top).
  // 90 non-center cells x 4 = 360 total chit capacity = 60 per color.
  var DEPTH = 4;
  function capacity(k) { return DEPTH; }
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
  function tier(n) { return (n >= 1 ? 1 : 0) + (n >= 4 ? 1 : 0) + (n >= 8 ? 1 : 0) + (n >= 12 ? 1 : 0); }
  // Scoring (Mohammad, 2026-06-08): a region scores its SIZE, and you claim it if
  // you still hold AT LEAST ONE chit of that color — holding more is NOT
  // multiplicative. So score = Σ over colors you hold (count>0) of largest-region
  // size for that color. (`mult`/`add` field names retained for back-compat with
  // the ranking/Elo code and the mock display; `mult` is now the size score and
  // `add` a tier tiebreak over the same held-color set.)
  function scoreHand(hand, regions, second) {
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

  var API = {
    SIZE: SIZE, cells: cells, cellSet: cellSet, key: key, axToPx: axToPx, NEI: NEI,
    sector: sector, capacity: capacity, setDepth: setDepth,
    get seed() { return seed; }, get palette() { return active; }, get PAL() { return PAL; },
    PALETTES: PALETTES, setPalette: setPalette,
    mixStack: mixStack, classify: classify, classifyIndependent: classifyIndependent,
    resultOf: resultOf, displayRGB: displayRGB,
    boardColors: boardColors, largestRegions: largestRegions, largestTwo: largestTwo, scoreHand: scoreHand,
    wheelOpposite: wheelOpposite, wheelBetween: wheelBetween,
    thresholds: function () { return { REL_LUM_MIN: REL_LUM_MIN, PURITY_MAX: PURITY_MAX }; }
  };
  if (typeof module !== "undefined" && module.exports) module.exports = API;
  else root.Chroma = API;
})(typeof window !== "undefined" ? window : this);
