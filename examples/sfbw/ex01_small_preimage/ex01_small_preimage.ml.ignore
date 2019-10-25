module Universe = Snarky_universe.Bn128 ()
open! Universe.Impl
open! Universe

module Witness = struct
  let length = 32

  type t = Bool.t array

  module Constant = struct
    type t = bool array [@@deriving yojson]
  end

  let typ = Typ.array ~length Bool.typ
end

(* Proves that there is a 32-bit preimage to the hash *)
let main (preimage : Witness.t) h  =
  Field.assertEqual (Hash.hash [|Field.ofBits preimage|]) h
