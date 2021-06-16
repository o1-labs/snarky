module Field = struct
  type t = int

  let to_field_elements i = [|i|]

  let size_in_field_elements = 1

  let of_field_elements_indexed ~offset x =
    let x = x.(!offset) in
    incr offset ; x

  let of_field_elements = function
    | [|x|] ->
        x
    | _ ->
        raise (Invalid_argument "Field.of_field_elements")
end

module Boolean = struct
  type t = bool

  let to_field_elements b = [|(if b then 1 else 0)|]

  let size_in_field_elements = 1

  let of_field_elements_indexed ~offset x =
    let x = x.(!offset) in
    incr offset ;
    match x with 0 -> false | 1 -> true | _ -> assert false

  let of_field_elements = function
    | [|x|] ->
        x
    | _ ->
        raise (Invalid_argument "Boolean.of_field_elements")
end

type ('a, 'b) t = {a: 'a * Field.t; b: 'b * Boolean.t * Field.t}
[@@deriving snarky_typ]

module type S = sig
  module M : sig
    type ('a, 'b) t [@@deriving snarky_typ]
  end

  type t [@@deriving snarky_typ]
end

module Test : S = struct
  module M = struct
    type ('a, 'b) t = {a: 'a * Field.t; b: 'b * Boolean.t * Field.t}
    [@@deriving snarky_typ]
  end

  include Field
end

let size_in_field_elements =
  [%size_in_field_elements: (Field.t, Field.t) Test.M.t]

let to_field_elements = [%to_field_elements: (Field.t, Field.t) Test.M.t]

let of_field_elements = [%of_field_elements: (Field.t, Field.t) Test.M.t]
