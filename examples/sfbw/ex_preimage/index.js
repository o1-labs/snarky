const { bn128 } = require('snarkyjs-crypto');
const Snarky = require('snarkyjs');
const snarky = new Snarky("./ex_preimage.exe");

const preImage = bn128.Field.ofInt(5);
const statement = bn128.Hash.hash([ preImage ]);

snarky.prove({
  statement: [ statement ],
  witness: preImage
}).then(function(proof) {
  console.log("Created proof:\n" + proof + "\n");
  return snarky.verify({
    "statement": [ statement ],
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
