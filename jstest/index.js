const Snarky = require("js_snarky");
const snarky = new Snarky("src/run_snarky.exe");

var prove_and_verify = function(statement, witness) {
  return snarky.prove({
    "statement": statement,
    "witness": witness
  }).then(function(proof) {
    console.log("Created proof for statement " + JSON.stringify(statement) + ":\n" + proof + "\n");
    return snarky.verify({
      "statement": statement,
      "proof": proof
    });
  }, console.log).then(function (verified) {
    console.log("Was the proof verified as correct? " + verified);
  }, console.log);
};

prove_and_verify(["2", true], "9").then(function() {
  return prove_and_verify(["4", true], "10");
}, console.log).then(function() {
  return prove_and_verify(["8", true], "12");
}, console.log).then(function() {
  process.exit(0);
}, function() {
  process.exit(1);
});
