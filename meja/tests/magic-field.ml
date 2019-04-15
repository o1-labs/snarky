module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type 'field field_pair = 'field * 'field

type 'field field_pair_alias = 'field field_pair

type 'field record_containing_field = {a: int * (int * 'field)}

type 'a with_param = unit * 'a

type 'field field_param = 'field with_param

type 'field variant_containing_field = A of 'field | B

type (_, 'field) variant_with_field_param =
  | A : (int, 'field) variant_with_field_param
  | B : ('field, 'field) variant_with_field_param

let add3 (add : 'field -> 'field -> 'field) (x : 'field) (y : 'field)
    (z : 'field) =
  add (add x y) z

let test_add3 (add : int -> int -> int) = add3 add 1 2 3
