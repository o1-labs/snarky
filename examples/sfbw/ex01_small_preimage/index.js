const Snarky = require("js_snarky");
const snarky = new Snarky("./ex01_small_preimage.exe");

snarky.prove({
  "statement": ["2550535537361194515906102134927683340979585801486533570983056998398130111650"],
  "witness": /* 32 bits encoding 0x12345678 */
    [false, false, false, true, false, false, true,  false,
     false, false, true,  true, false, true,  false, false,
     false, true,  false, true, false, true,  true,  false,
     false, true,  true,  true, true,  false, false, false]
}).then(function(proof) {
  console.log("Created proof:\n" + proof + "\n");
  return snarky.verify({
    "statement": ["2550535537361194515906102134927683340979585801486533570983056998398130111650"],
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
