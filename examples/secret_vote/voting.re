open Core;
open Fold_lib;
open Backend;

module Nullifier = {
  type t = Random_oracle.Digest.t;

  let init = Fold.(to_list(string_triples("null")));

  let create = {
    let init = boolean_triples(init);

    (v : Commitment.Value.t) =>
      Random_oracle.digest(init @ Commitment.Value.to_triples(v))
  }

  let typ = Random_oracle.Digest.typ;

  let assert_equal = Field.Assert.equal

  module Constant = {
    type t = Random_oracle.Digest.Constant.t
  };
};

module Shield_proof = {
  type t =
    { public_key : Signature.Public_key.Constant.t
    , commitment : Commitment.Constant.t
    , signature : Signature.Constant.t
    };
};

module Vote = {
  /* false = Pepperoni
     true = Mushroom */

  type t = Boolean.var;

  module Constant = {
    type t = Pepperoni | Mushroom

    let of_bool = fun
      | false => Pepperoni
      | true => Mushroom
    let to_bool = fun
      | Pepperoni => false
      | Mushroom => true
  };

  let typ : Typ.t (t, Constant.t) = Typ.transport(
    Boolean.typ, ~there=Constant.to_bool, ~back=Constant.of_bool);
};

module Vote_proof = {
  module Witness = {
    type t = {
      merkle_index : Merkle_tree.Index.Constant.t,
      commitment_randomness : Commitment.Randomness.Constant.t,
      commitment_value : Commitment.Value.Constant.t
    };
  };

  type Snarky.Request.t(_) +=
    | Merkle_index : Snarky.Request.t(Merkle_tree.Index.Constant.t)
    | Commitment_value : Snarky.Request.t(Commitment.Randomness.Constant.t)

  let public_input () =
    Data_spec.
      [ Merkle_tree.typ
      , Nullifier.typ
      , Vote.typ
      ];

  let main(root, nullifier, _vote : Vote.t, ()) = {
    let index =
      exists(Merkle_tree.Index.typ, ~request=() => Merkle_index);
    let commitment = Merkle_tree.lookup(root, index);
    let value =
      exists(Commitment.Value.typ, ~request=() => Commitment_value);
    Commitment.check(commitment, value);
    Nullifier.assert_equal(Nullifier.create(value), nullifier);
  };

  let proof_system =
    Proof_system.create(~public_input=public_input(), main);
};
