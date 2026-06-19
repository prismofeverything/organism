const C = require("./chroma-core.js");
const fs = require("fs");
C.setPalette("CMY");
const removed = new Set(["5,0","4,1","5,-1","-5,5","-5,4","-4,5","0,-5","1,-5","-1,-4"]);
const sect = [], seed = [];
C.cells.forEach(c => {
  const k = C.key(c[0], c[1]);
  if (removed.has(k)) return;                       // only the 82 playable cells
  sect.push(`[${c[0]},${c[1]},${C.sector(c[0], c[1])}]`);
  const s = C.seed[k];
  if (s) seed.push(`[${c[0]},${c[1]},"${s}"]`);
});
let out = "// AUTO-GENERATED from chroma-core.js (CMY palette). Do not edit by hand.\n";
out += "// regenerate: node gen_seed_data.js\n";
out += "// [q, r, sector(0-5)] for the 82 playable cells:\n";
out += "SECTOR_DATA = [\n  " + sect.join(", ") + "\n];\n\n";
out += `// [q, r, seed-color-letter] for the ${seed.length} seeded cells:\n`;
out += "SEED_DATA = [\n  " + seed.join(", ") + "\n];\n";
fs.writeFileSync("chroma-board-model/chroma_seed_data.scad", out);
console.log(`wrote chroma_seed_data.scad | sectors: ${sect.length} | seeds: ${seed.length}`);
