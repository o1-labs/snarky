open Core;
open Backend;

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
  type Snarky.Request.t(_) +=
    | Merkle_index : Snarky.Request.t(Merkle_tree.Index.Constant.t)
    | Voting_key : Snarky.Request.t(Commitment.Randomness.Constant.t)

  module Witness = {
    type t = {
      merkle_index : Merkle_tree.Index.Constant.t,
      commitment_randomness : Commitment.Randomness.Constant.t,
      voting_key : Voting_key.Constant.t
    };

    let handler({ merkle_index, commitment_randomness, voting_key }) = {
      (Snarky.Request.With ({ request, respond})) => {
        switch (request) {
          | Merkle_index => respond (Provide (merkle_index))
          | Voting_key => respond (Provide (voting_key))
          | Commitment.Commitment_randomness => respond (Provide (commitment_randomness))
          | _ => failwith("TODO")
        }
      }
    };
  };

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
    let voting_key =
      exists(Voting_key.typ, ~request=() => Voting_key);
    Commitment.check(commitment, voting_key);
    Nullifier.assert_equal(Nullifier.create(voting_key), nullifier);
  };

  let proof_system =
    Proof_system.create(
      ~proving_key_path="proving_key",
      ~verification_key_path="verification_key",
      ~public_input=public_input(), main
    );
};

let _ = List.map
