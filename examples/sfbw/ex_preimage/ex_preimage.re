module Universe = (val Snarky_universe.default());
open! Universe.Impl;
open! Universe;

let input = InputSpec.[(module Hash)];

module Witness = Field;

let main = (preimage: Witness.t, h, ()) =>
  Field.assertEqual(Hash.hash([|preimage|]), h);

runMain(input, (module Witness), main);
