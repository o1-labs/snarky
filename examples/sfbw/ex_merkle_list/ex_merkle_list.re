module Universe = (val Snarky_universe.default());
open! Universe.Impl;
open! Universe;

module Witness = {
  let length = 32;

  type t = array(Hash.t);

  module Constant = {
    [@deriving yojson]
    type t = array(Hash.Constant.t);
  };

  let typ = Typ.array(~length, Hash.typ);
};

let input = InputSpec.[(module Field), (module Field)];

/* Proves that there is a path [hn, ..., h1] such that
   hash (h1, hash(h2, hash(h3, ..., hash(hn, x) ... ))) = root */
let main = (path: Witness.t, supposed_root, x, ()) => {
  let actual_root =
    Array.fold_left((acc, h) => Hash.hash([|h, acc|]), x, path);

  Hash.assertEqual(actual_root, supposed_root);
};

InputSpec.run_main(input, (module Witness), main);
