open Core;
open Backend;
open Fold_lib;

module Randomness = Randomness;

type t = Field.t;

let init = Fold.(to_list(string_triples("comm")));

type Snarky.Request.t(_) +=
  | Commitment_randomness: Snarky.Request.t(Randomness.Constant.t);

/* Check that comm == commit(vk, r) for some randomness r */
let check = {
  let init = boolean_triples(init);
  (comm: t, vk: Voting_key.t) => {
    let r = exists(Randomness.typ, ~request=() => Commitment_randomness);
    Field.Assert.equal(
      comm,
      Pedersen.digest(
        init @ Voting_key.to_triples(vk) @ Randomness.to_triples(r),
      ),
    );
  };
};

let to_triples = (t: t) => Pedersen.Digest.to_triples(t);

module Constant = {
  type t = Field.Constant.t;

  let empty = Field.Constant.zero;

  let commit = (vk: Voting_key.Constant.t): t => {
    let r = Randomness.Constant.create();
    Pedersen.Constant.digest(
      init
      @ Voting_key.Constant.to_triples(vk)
      @ Randomness.Constant.to_triples(r),
    );
  };
  let to_triples = (t: t) => Pedersen.Digest.Constant.to_triples(t);
};

let typ = Field.typ;
