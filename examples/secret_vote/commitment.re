open Core;
open Backend;
open Fold_lib;

module Value = Randomness
module Randomness = Randomness

type t = Field.t

let init = Fold.(to_list(string_triples("comm")));

type Snarky.Request.t(_) +=
  | Commitment_randomness : Snarky.Request.t(Randomness.Constant.t)

let check = {
  let init = boolean_triples(init);
  (comm : t, value : Value.t) => {
    let r = exists(Randomness.typ, ~request=() => Commitment_randomness);
    Field.Assert.equal(
      comm,
      Pedersen.digest(init @ Value.to_triples(value) @ Randomness.to_triples(r)) )
  };
}

let to_triples (t : t) = Pedersen.Digest.to_triples(t);

module Constant = {
  type t = Field.Constant.t

  let commit(value : Value.Constant.t) : t = {
    let r = Randomness.Constant.create();
    Pedersen.Constant.digest (
      init @ Value.Constant.to_triples(value) @ Randomness.Constant.to_triples(r))
  };
};

let typ = Field.typ;
