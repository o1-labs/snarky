module Universe = (val Snarky_universe.default());
open! Universe.Impl;
open! Universe;

module Witness = Field;

let input = InputSpec.[(module Field)];

let main = (preimage: Witness.t, h, ()) =>
  Field.assertEqual(Hash.hash([|preimage|]), h);

InputSpec.run_main(input, (module Witness), main);
