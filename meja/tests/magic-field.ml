open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

type field_pair = Field.t * Field.t

type field_pair_alias = field_pair

type record_containing_field = {a: int * (int * Field.t)}

type 'a with_param = unit * 'a

type field_param = Field.t with_param

type variant_containing_field = A of Field.t | B

type _ variant_with_field_param =
  | A : int variant_with_field_param
  | B : Field.t variant_with_field_param

let add3 (add : Field.t -> Field.t -> Field.t) =
  add
    (add
       (Field.constant (Field.Constant.of_string "1"))
       (Field.constant (Field.Constant.of_string "2")))
    (Field.constant (Field.Constant.of_string "3"))
