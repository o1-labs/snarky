const { bn128 } = require('snarkyjs-crypto');
const Snarky = require("snarkyjs");
const snarky = new Snarky("./ex02_merkle_list.exe");

const merkleListLength = 32;
const data = [];
for (let i = 0; i < merkleListLength; ++i) {
  data.push(bn128.Field.ofInt(i));
}

const buriedData = bn128.Field.ofInt(0x1234567);

const root = data.reduce((acc, x) =>
  bn128.Hash.hash([x, acc]), buriedData);

const statement = [
    /* Merkle list root hash */
    bn128.Field.toString(root),
    /* Data */
    bn128.Field.toString(buriedData)
];

snarky.prove({
  "statement": statement,
  "witness": data.map(bn128.Field.toString)
}).then(function(proof) {
  console.log("Created proof:\n" + proof + "\n");
  return snarky.verify({
    "statement": statement,
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
