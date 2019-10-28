const { bn128 } = require('snarkyjs-crypto');
const Snarky = require('snarkyjs');
const snarky = new Snarky("./ex01_small_preimage.exe");

const preImage = bn128.Field.ofInt(0x12345678);

const statement = bn128.Hash.hash([preImage]);

const witnessBits = [];
for (let i = 0; i < 32; ++i) {
  witnessBits.push(bn128.Field.testBit(preImage, i));
}

snarky.prove({
  "statement": [ bn128.Field.toString(statement) ],
  "witness": witnessBits
}).then(function(proof) {
  console.log("Created proof:\n" + proof + "\n");
  return snarky.verify({
    "statement": [ bn128.Field.toString(statement) ],
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
