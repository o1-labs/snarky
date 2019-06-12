module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type field_pair = field * field

type field_pair_alias = field_pair

type record_containing_field = {a: int * (int * field)}

type 'a with_param = unit * 'a

type field_param = field with_param

type variant_containing_field = A of field | B

type _ variant_with_field_param =
  | A : int variant_with_field_param
  | B : field variant_with_field_param

let add3 (add : Field.t -> Field.t -> Field.t) =
  add
    (add
       (Field.constant (Field.Constant.of_string "1"))
       (Field.constant (Field.Constant.of_string "2")))
    (Field.constant (Field.Constant.of_string "3"))
