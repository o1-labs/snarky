module rec Universe:
  Snarky_universe.Intf.S with type Impl.prover_state = Prover_state.t =
  Snarky_universe.Default(
    Prover_state,
    {},
  )
and Prover_state: {
  [@deriving yojson]
  type t = array(bool);

  let length: int;

  let typ: Universe.Impl.Typ.t(array(Universe.Bool.t), t);
} = {
  open! Universe.Impl;
  open! Universe;

  [@deriving yojson]
  type t = array(bool);

  let length = 32;

  let typ = Typ.array(~length, Bool.typ);
};

open! Universe.Impl;
open! Universe;

let input = InputSpec.[(module Field)];

/* Proves that there is a 32-bit preimage to the hash */
let main = (h, ()) => {
  let preimage =
    exists(Prover_state.typ, ~compute=() => {As_prover.get_state()});
  Field.assertEqual(Hash.hash([|Field.ofBits(preimage)|]), h);
};

InputSpec.run_main(input, Prover_state.of_yojson, main);
