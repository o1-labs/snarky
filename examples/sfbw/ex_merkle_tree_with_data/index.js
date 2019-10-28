const { bn128 } = require('snarkyjs-crypto');
const Snarky = require("snarkyjs");
const snarky = new Snarky("./ex_merkle_tree_with_data.exe");

const somePerson = {
  age: bn128.Field.ofInt(67),
  favoriteColor: [ 0, 0, 255 ]
};

const byteBits = (x) => {
  const res = [];
  for (let i = 0; i < 8; ++i) {
    res.push( ((x >> i) & 1) === 1 );
  }
  return res;
};

const hashPerson = ({age, favoriteColor}) =>
  bn128.Hash.hash([ age, bn128.Field.ofBits(favoriteColor.flatMap(byteBits)) ]);

const depth = 8;
const numElts = 1 << depth;
const data = []

for (let i = 0; i < numElts; ++i) {
  /* Not the most interesting data... :) */
  data.push( somePerson );
}

const tree = bn128.MerkleTree.ofArray(hashPerson, somePerson, data);

const root = bn128.MerkleTree.root(tree);
const indexToProve = 17;
const merkleProof = bn128.MerkleTree.MembershipProof.create(tree, indexToProve);

const statement = [
  /* Root hash */
  bn128.Field.toString(root),
  /* Color */
  data[indexToProve].favoriteColor.map(byteBits)
];

const witness = [
  [ somePerson.age.toString(), somePerson.favoriteColor.map(byteBits) ],
  merkleProof
];

snarky.prove({
  statement: statement,
  witness: witness
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
