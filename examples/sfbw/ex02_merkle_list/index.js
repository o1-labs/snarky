const Snarky = require("js_snarky");
const snarky = new Snarky("./ex02_merkle_list.exe");

snarky.prove({
  "statement": [
    /* Hash */
    "7042570718949472988806264380637625507179598854752747064560805511579737198879",
    /* Data */
    "255"
  ],
  "witness": [
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15",
    "16",
    "17",
    "18",
    "19",
    "21",
    "22",
    "23",
    "24",
    "25",
    "26",
    "27",
    "28",
    "29",
    "30",
    "31",
    "32",
    "33"
  ]
}).then(function(proof) {
  console.log("Created proof:\n" + proof + "\n");
  return snarky.verify({
    "statement": [
      "7042570718949472988806264380637625507179598854752747064560805511579737198879",
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
