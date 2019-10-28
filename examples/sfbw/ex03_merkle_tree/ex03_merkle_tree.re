module rec Universe:
  Snarky_universe.Intf.S with type Impl.prover_state = Prover_state.t =
  Snarky_universe.Default(
    Prover_state,
    {},
  )
and Prover_state: {
  [@deriving yojson]
  type t = (int, array(Universe.Hash.Constant.t));

  let depth: int;

  let typ:
    Universe.Impl.Typ.t(
      (array(Universe.Bool.t), array(Universe.Hash.t)),
      t,
    );
} = {
  open! Universe.Impl;
  open! Universe;

  [@deriving yojson]
  type t = (int, array(Hash.Constant.t));

  let depth = 32;

  let typ =
    Typ.tuple2(MerkleTree.Index.typ(~depth), MerkleTree.Path.typ(~depth));
};

open! Universe.Impl;
open! Universe;

let depth = Prover_state.depth;

let input = InputSpec.[(module Hash), (module Field)];

let main = (supposed_root, elt, ()) => {
  let (index, path) =
    exists(Prover_state.typ, ~compute=() => {As_prover.get_state()});
  let acc = ref(elt);
  for (i in 0 to depth - 1) {
    let bit = index[i];
    let left = Hash.(bit -? path[i] -: acc^);
    let right = Hash.(bit -? acc^ -: path[i]);
    acc := Hash.hash([|left, right|]);
  };
  Hash.assertEqual(acc^, supposed_root);
};

InputSpec.run_main(input, Prover_state.of_yojson, main);
