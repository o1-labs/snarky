module Universe = (val Snarky_universe.default());
open! Universe.Impl;
open! Universe;

let depth = 32;

module Witness = {
  type t = (array(Bool.t), array(Hash.t));

  module Constant = {
    [@deriving yojson]
    type t = (int, array(Hash.Constant.t));
  };

  let typ =
    Typ.tuple2(MerkleTree.Index.typ(~depth), MerkleTree.Path.typ(~depth));
};

let input = InputSpec.[(module Hash), (module Field)];

let main = ((index, path): Witness.t, supposed_root, elt, ()) => {
  let acc = ref(elt);
  for (i in 0 to depth - 1) {
    let bit = index[i];
    let left = Hash.(bit -? path[i] -: acc^);
    let right = Hash.(bit -? acc^ -: path[i]);
    acc := Hash.hash([|left, right|]);
  };
  Hash.assertEqual(acc^, supposed_root);
};

InputSpec.run_main(input, (module Witness), main);
