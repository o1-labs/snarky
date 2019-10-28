const Snarky = require("js_snarky");
const snarky = new Snarky("./ex00_preimage.exe");

snarky.prove({
  "statement": ["16878804342167538767210670789235150761589353743644181347428772717242652062541"],
  "witness": "5"
}).then(function(proof) {
  console.log("Created proof:\n" + proof + "\n");
  return snarky.verify({
    "statement": ["16878804342167538767210670789235150761589353743644181347428772717242652062541"],
    "proof": proof
  });
}, console.log).then(function(verified) {
  console.log("Was the proof verified? " + verified);
  if (verified) {
    process.exit(0);
  } else {
    process.exit(1);
  }
}, function() { process.exit(1); });
