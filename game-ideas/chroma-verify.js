/* Standalone mixing audit. Runs the core's mixing independently and flags
 * (a) classify vs classifyIndependent disagreements (code-path check)
 * (b) "surprise" results: dark/low stacks that still classify as a color. */
var C = require("./chroma-core.js");

function lum(rgb) { return 0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2]; }

["RYB", "CMY"].forEach(function (pal) {
  C.setPalette(pal);
  var cols = C.PAL.order.slice();
  console.log("\n===== palette " + pal + " (" + cols.join(",") + ") thresholds " +
    JSON.stringify(C.thresholds()) + " =====");
  var disagree = 0, surprises = [];

  function show(stack) {
    var r = C.resultOf(stack);
    if (r.color !== r.check) { disagree++; console.log("  DISAGREE", stack.join("+"), r.color, "vs", r.check); }
    return r;
  }
  // singles
  console.log("singles:");
  cols.forEach(function (c) {
    var r = show([c]);
    console.log("  " + c + " -> " + r.color + (r.color === c ? "" : "  <-- not self!"));
  });
  // pairs
  console.log("pairs:");
  for (var i = 0; i < 6; i++) for (var j = i; j < 6; j++) {
    var st = [cols[i], cols[j]], r = show(st);
    var l = lum(r.rgb).toFixed(2);
    console.log("  " + cols[i] + "+" + cols[j] + " -> " + r.color + "  (lum " + l + ")");
    if (r.color !== "mud" && r.color !== "white" && lum(r.rgb) < 0.22) surprises.push(st.join("+") + "->" + r.color + " lum" + l);
  }
  // a few triples
  console.log("triples (sample):");
  [["R", "G", "B"], [cols[0], cols[2], cols[4]], [cols[1], cols[3], cols[5]], [cols[0], cols[1], cols[2]]]
    .forEach(function (st) { var r = show(st); console.log("  " + st.join("+") + " -> " + r.color + " (lum " + lum(r.rgb).toFixed(2) + ")"); });

  console.log("disagreements (classify vs independent):", disagree);
  console.log("dark-but-colored surprises:", surprises.length, surprises.length ? surprises : "");
});
