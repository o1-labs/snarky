open Backend;
open Fold_lib;

type t = Random_oracle.Digest.t;

let init = Fold.(to_list(string_triples("null")));

let create = {
  let init = boolean_triples(init);

  (v : Voting_key.t) =>
    Random_oracle.digest(init @ Voting_key.to_triples(v))
}

let typ = Random_oracle.Digest.typ;

let assert_equal = Field.Assert.equal

module Constant = {
  type t = Random_oracle.Digest.Constant.t
};

