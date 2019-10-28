module Universe = (val Snarky_universe.default ())
open! Universe.Impl
open! Universe

let depth = 32

module Witness = struct
  type t = Bool.t array * Hash.t array

  module Constant = struct
    type t = int * Hash.Constant.t array [@@deriving yojson]
  end

  let typ =
    Typ.tuple2
      (MerkleTree.Index.typ ~depth)
      (MerkleTree.Path.typ ~depth)
end

let main ((index, path) : Witness.t) supposed_root elt =
  let acc = ref elt in
  for i = 0 to depth - 1 do
      let bit = index.(i) in
      let left = Hash.( bit -? path.(i) -: !acc ) in
      let right = Hash.( bit -? !acc -: path.(i) ) in
      acc := Hash.hash [| left; right |]
  done;
  Hash.assertEqual !acc supposed_root
