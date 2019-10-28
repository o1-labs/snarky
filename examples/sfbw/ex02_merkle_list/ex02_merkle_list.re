module rec Universe:
  Snarky_universe.Intf.S with type Impl.prover_state = Prover_state.t =
  Snarky_universe.Default(
    Prover_state,
    {},
  )
and Prover_state: {
  [@deriving yojson]
  type t = array(Universe.Hash.Constant.t);

  let length: int;

  let typ: Universe.Impl.Typ.t(array(Universe.Hash.t), t);
} = {
  open! Universe.Impl;
  open! Universe;

  [@deriving yojson]
  type t = array(Hash.Constant.t);

  let length = 32;

  let typ = Typ.array(~length, Hash.typ);
};

open! Universe.Impl;
open! Universe;

let input = InputSpec.[(module Field), (module Field)];

/* Proves that there is a path [hn, ..., h1] such that
   hash (h1, hash(h2, hash(h3, ..., hash(hn, x) ... ))) = root */
let main = (supposed_root, x, ()) => {
  let path = exists(Prover_state.typ, ~compute=() => {As_prover.get_state()});
  let actual_root =
    Array.fold_left((acc, h) => Hash.hash([|h, acc|]), x, path);

  Hash.assertEqual(actual_root, supposed_root);
};

InputSpec.run_main(input, Prover_state.of_yojson, main);
