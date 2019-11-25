module Universe = (val Snarky_universe.default ())

open! Universe.Impl
open! Universe

(* Each input needs a jsonifier and a type *)
let input : _ InputSpec.t = [(module Field); (module Bool)]

module Witness = struct
  type t = Field.t

  module Constant = struct
    type t = Field.Constant.t [@@deriving yojson]
  end

  let typ = Field.typ
end

(* Test function, asserts that [Field.( * )] and [Field.inv] commute. *)
let assert_inverse_square_commutes field_elt =
  let field_elt_2 = Field.(field_elt * field_elt) in
  let field_elt_inv = Field.invert field_elt in
  let field_elt_inv_2 = Field.(field_elt_inv * field_elt_inv) in
  let product = Field.(field_elt_2 * field_elt_inv_2) in
  Field.assertEqual product Field.one

(* The main function. This is executed to build a proof *)
let main witness field_elt _bit () =
  assert_inverse_square_commutes field_elt ;
  assert_inverse_square_commutes witness
