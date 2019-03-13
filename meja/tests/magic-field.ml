type 'field field_pair = 'field * 'field

type 'field field_pair_alias = 'field field_pair

type 'field record_containing_field = {a: int * (int * 'field)}

type 'a with_param = unit * 'a

type 'field field_param = 'field with_param

let add3 (add : field -> field -> field) (x : field) (y : field) (z : field) =
  add (add x y) z

let test_add3 (add : int -> int -> int) = add3 add 1 2 3
