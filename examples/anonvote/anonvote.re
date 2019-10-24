module Universe = (val Snarky_universe.default());

open! Universe.Impl;
open! Universe;
module PrivateKey = Field;

let attributeCount = 10;

let merkleTreeDepth = 25;

module Voter = {
  [@deriving yojson]
  type t_('priv_key, 'attr) = {
    privateKey: 'priv_key,
    attributes: array('attr),
  };

  module Attribute_commitment = Hash;

  type t = t_(Field.t, Attribute_commitment.t);

  module Constant = {
    [@deriving yojson]
    type t = t_(PrivateKey.Constant.t, Attribute_commitment.Constant.t);
  };

  let typ = {
    open Snarky.H_list;
    let to_hlist = ({privateKey, attributes}) => [privateKey, attributes];
    let of_hlist = ([privateKey, attributes]: t(unit, _)) => {
      privateKey,
      attributes,
    };

    Typ.of_hlistable(
      [
        PrivateKey.typ,
        Typ.array(~length=attributeCount, Attribute_commitment.typ),
      ],
      ~var_to_hlist=to_hlist,
      ~var_of_hlist=of_hlist,
      ~value_to_hlist=to_hlist,
      ~value_of_hlist=of_hlist,
    );
  };

  let commit = ({privateKey, attributes}) =>
    Hash.hash(Array.append([|privateKey|], attributes));
};

module Witness = {
  type t = (Voter.t, MerkleTree.MembershipProof.t);

  module Constant = {
    type t = (Voter.Constant.t, MerkleTree.MembershipProof.Constant.t);
  };

  let typ =
    Typ.tuple2(
      Voter.typ,
      MerkleTree.MembershipProof.typ(~depth=merkleTreeDepth),
    );
};

module Nullifier = {
  include Field;

  let create = (privateKey, electionDescription) =>
    Hash.hash([|privateKey, electionDescription|]);
};

let nullAttr = Field.zero;

let main =
    (
      (voter, merkleProof): Witness.t,
      votersRoot,
      nullifier,
      _vote,
      maskedAttributes,
      election,
      (),
    ) => {
  let comm = Voter.commit(voter);
  MerkleTree.MembershipProof.check(merkleProof, votersRoot, comm);
  for (i in 0 to attributeCount - 1) {
    let claimedAttr = maskedAttributes[i];
    let attr = voter.attributes[i];
    Bool.assertAny([
      Voter.Attribute_commitment.equal(claimedAttr, attr),
      Voter.Attribute_commitment.equal(claimedAttr, nullAttr),
    ]);
  };
  Nullifier.assertEqual(
    nullifier,
    Nullifier.create(voter.privateKey, election),
  );
};
