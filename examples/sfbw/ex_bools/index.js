const Snarky = require("js_snarky");
const snarky = new Snarky("./ex_bools.exe");

let run = function(data) {
  console.log("Proving preimage " + data.witness + "\n");
  return snarky.prove({
    "statement": data.statement,
    "witness": data.witness
  }).then(function(proof) {
    console.log("Created proof:\n" + proof + "\n");
    return snarky.verify({
      "statement": data.statement,
      "proof": proof
    });
  }, console.log).then(function(verified) {
    console.log("Was the proof verified? " + verified);
  }, function() { process.exit(1); });
}

run({
  "statement": [
    "493360206186551699850930471382527704320153382588025911514311564864447225142"
  ],
  "witness": "7"
}).then(function() {
  return run({
    "statement": [
      "7127756033753096230854631514848773854696474340909605600143755533068927075188"
    ],
    "witness": "30"
  })
}).then(function() {
  return run({
    "statement": [
      "736172886722736280065715348741339967685234645072191547344310985667416558423"
    ],
    "witness": "1024"
  })
}, console.log).then(function() {
  process.exit(0);
}, function() { process.exit(1); });
