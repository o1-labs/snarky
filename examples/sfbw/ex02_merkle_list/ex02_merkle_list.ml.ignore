module Universe = (val Snarky_universe.default ())
open! Universe.Impl
open! Universe

module Witness = struct
  let length = 32

  type t = Hash.t array

  module Constant = struct
    type t = Hash.Constant.t array [@@deriving yojson]
  end

  let typ = Typ.array ~length Bool.typ
end

(* Proves that there is a path [hn, ..., h1] such that
   hash (h1, hash(h2, hash(h3, ..., hash(hn, x) ... ))) = root *)
let main (path : Witness.t) supposed_root x =
  let actual_root =
    Array.fold_left (fun acc h ->
        Hash.hash [| h; acc |])
        x
        path
  in
  Hash.assertEqual actual_root supposed_root
