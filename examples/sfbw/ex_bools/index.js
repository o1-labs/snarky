const Snarky = require('snarkyjs');
const { bn128 } = require('snarkyjs-crypto');
const snarky = new Snarky("./ex_bools.exe");

const specialNumbers = [ 7, 30, 1024 ].map(bn128.Field.ofInt);

const makeStatementAndWitness = (specialNumber) => {
  return {
    statement: [ bn128.Field.toString(bn128.Hash.hash([ specialNumber ])) ],
    witness: bn128.Field.toString(specialNumber)
  }
};

let run = function(specialNumber) {
  const statementAndWitness = makeStatementAndWitness(specialNumber);

  console.log("Proving preimage " + statementAndWitness.witness + "\n");
  return snarky.prove(statementAndWitness).then(function(proof) {
    console.log("Created proof:\n" + proof + "\n");
    return snarky.verify({
      "statement": statementAndWitness.statement,
      "proof": proof
    });
  }, console.log).then(function(verified) {
    console.log("Was the proof verified? " + verified);
  }, function() { process.exit(1); });
}

run(specialNumbers[0])
.then(() => run(specialNumbers[1]), console.log)
.then(() => run(specialNumbers[2]), console.log)
.then(() => process.exit(0), () => process.exit(1))
