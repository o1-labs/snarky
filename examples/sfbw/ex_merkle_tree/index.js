const { bn128 } = require('snarkyjs-crypto');
const Snarky = require("snarkyjs");
const snarky = new Snarky("./ex03_merkle_tree.exe");

const depth = 8;
const numElts = 1 << depth;
const data = []

for (let i = 0; i < numElts; ++i) {
  data.push(bn128.Field.ofInt(i));
}

/* No need to hash leaves which are already field elements */
const hashElt = (x) => x;
const defaultElt = bn128.Field.ofInt(0);

const tree = bn128.MerkleTree.ofArray(hashElt, defaultElt, data);

const root = bn128.MerkleTree.root(tree);
const indexToProve = 17;
const proof = bn128.MerkleTree.MembershipProof.create(tree, indexToProve);

console.log(root);
console.log(data[indexToProve]);
console.log(bn128.MerkleTree.MembershipProof.check(proof, root, hashElt(data[indexToProve])));

const statement = [
  /* Root hash */
  bn128.Field.toString(root),
  /* Data */
  bn128.Field.toString(data[indexToProve])
];

snarky.prove({
  statement: statement,
  witness: [
    proof.index,
    proof.path.map(bn128.Field.toString)
  ]
}).then(function(proof) {
  console.log("Created proof:\n" + proof + "\n");
  return snarky.verify({
    statement: statement,
    proof: proof
  });
}, console.log).then(function(verified) {
  console.log("Was the proof verified? " + verified);
  if (verified) {
    process.exit(0);
  } else {
    process.exit(1);
  }
}, function() { process.exit(1); });
