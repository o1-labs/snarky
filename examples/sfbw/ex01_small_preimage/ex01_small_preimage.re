module Universe =
  Snarky_universe.Bn128({});
open! Universe.Impl;
open! Universe;

module Witness = {
  let length = 32;

  type t = array(Bool.t);

  module Constant = {
    [@deriving yojson]
    type t = array(bool);
  };

  let typ = Typ.array(~length, Bool.typ);
};

/* Proves that there is a 32-bit preimage to the hash */
let main = (preimage: Witness.t, h) =>
  Field.assertEqual(Hash.hash([|Field.ofBits(preimage)|]), h);
