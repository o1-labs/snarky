module Universe = (val Snarky_universe.default ())

open! Universe.Impl
open! Universe
module PrivateKey = Field

let attributeCount = 10

let merkleTreeDepth = 25

module Voter = struct
  type ('priv_key, 'attr) t_ = {privateKey: 'priv_key; attributes: 'attr array}
  [@@deriving yojson]

  module Attribute_commitment = Field

  type t = (Field.t, Attribute_commitment.t) t_

  module Constant = struct
    type t = (PrivateKey.Constant.t, Attribute_commitment.Constant.t) t_
    [@@deriving yojson]
  end

  let typ =
    let open Snarky.H_list in
    let to_hlist {privateKey; attributes} = [privateKey; attributes] in
    let of_hlist ([privateKey; attributes] : (unit, _) t) =
      {privateKey; attributes}
    in
    Typ.of_hlistable
      [ PrivateKey.typ
      ; Typ.array ~length:attributeCount Attribute_commitment.typ ]
      ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
      ~value_of_hlist:of_hlist

  let commit {privateKey; attributes} =
    Hash.hash (Array.append [|privateKey|] attributes)
end

module Witness = struct
  type t = Voter.t * MerkleTree.MembershipProof.t

  module Constant = struct
    type t = Voter.Constant.t * MerkleTree.MembershipProof.Constant.t
  end

  let typ =
    Typ.tuple2 Voter.typ
      (MerkleTree.MembershipProof.typ ~depth:merkleTreeDepth)
end

module Nullifier = struct
  include Field

  let create privateKey electionDescription =
    Hash.hash [|privateKey; electionDescription|]
end

let nullAttr = Field.zero

let main ((voter, merkleProof) : Witness.t) votersRoot nullifier _vote
    maskedAttributes election () =
  let comm = Voter.commit voter in
  MerkleTree.MembershipProof.check merkleProof votersRoot comm ;
  for i = 0 to attributeCount - 1 do
    let claimedAttr = maskedAttributes.(i) in
    let attr = voter.attributes.(i) in
    Bool.assertAny
      [ Voter.Attribute_commitment.equal claimedAttr attr
      ; Voter.Attribute_commitment.equal claimedAttr nullAttr ]
  done ;
  Nullifier.assertEqual nullifier (Nullifier.create voter.privateKey election)
