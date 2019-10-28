const Snarky = require("js_snarky");
const snarky = new Snarky("./ex03_merkle_tree.exe");

snarky.prove({
  "statement": [
    /* Hash */
    "13233041925772981287263381855432307403434594187545931284654254679248761729236",
    /* Data */
    "255"
  ],
  "witness": [
    65535,
    [
      "64",
      "62",
      "60",
      "58",
      "56",
      "54",
      "52",
      "50",
      "48",
      "46",
      "44",
      "42",
      "40",
      "38",
      "36",
      "34",
      "32",
      "30",
      "28",
      "26",
      "24",
      "22",
      "20",
      "18",
      "16",
      "14",
      "12",
      "10",
      "8",
      "6",
      "4",
      "2"
    ]
  ]
}).then(function(proof) {
  console.log("Created proof:\n" + proof + "\n");
  return snarky.verify({
    "statement": [
      "13233041925772981287263381855432307403434594187545931284654254679248761729236",
      "255"
    ],
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
