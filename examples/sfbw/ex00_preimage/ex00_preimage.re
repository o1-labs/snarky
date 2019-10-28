module rec Universe:
  Snarky_universe.Intf.S with type Impl.prover_state = Prover_state.t =
  Snarky_universe.Default(
    Prover_state,
    {},
  )
and Prover_state: {
  [@deriving yojson]
  type t = Universe.Field.Constant.t;
} = {
  [@deriving yojson]
  type t = Universe.Field.Constant.t;
};

open! Universe.Impl;
open! Universe;

let input = InputSpec.[(module Field)];

let main = (h: Hash.t, ()) => {
  let preimage = exists(Field.typ, ~compute=() => {As_prover.get_state()});
  Field.assertEqual(Hash.hash([|preimage|]), h);
};

InputSpec.run_main(input, Prover_state.of_yojson, main);
