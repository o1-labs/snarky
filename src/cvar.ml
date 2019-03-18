open Core_kernel

type 'f t =
  | Constant of 'f
  | Var of int
  | Add of 'f t * 'f t
  | Scale of 'f * 'f t
[@@deriving sexp]

type 'f cvar = 'f t [@@deriving sexp]

module Make
    (Field : Field_intf.Extended) (Var : sig
        include Comparable.S

        include Sexpable.S with type t := t

        val create : int -> t
    end) =
struct
  type t = Field.t cvar [@@deriving sexp]

  let length _ = failwith "TODO"

  module Unsafe = struct
    let of_index v = Var v
  end

  let eval context t0 =
    let open Field in
    let res = of_int 0 in
    let rec go scale = function
      | Constant c -> res += mul scale c
      | Var v -> res += mul scale (context v)
      | Scale (s, t) -> go (mul s scale) t
      | Add (t1, t2) -> go scale t1 ; go scale t2
    in
    go one t0 ; res

  let constant c = Constant c

  let to_constant_and_terms =
    let rec go scale constant terms = function
      | Constant c -> (Field.add constant (Field.mul scale c), terms)
      | Var v -> (constant, (scale, Var.create v) :: terms)
      | Scale (s, t) -> go (Field.mul s scale) constant terms t
      | Add (x1, x2) ->
          let c1, terms1 = go scale constant terms x1 in
          go scale c1 terms1 x2
    in
    fun t ->
      let c, ts = go Field.one Field.zero [] t in
      (Some c, ts)

  let add x y =
    match (x, y) with
    | Constant x, Constant y -> Constant (Field.add x y)
    | _, _ -> Add (x, y)

  let scale x s =
    match x with
    | Constant x -> Constant (Field.mul x s)
    | Scale (sx, x) -> Scale (Field.mul sx s, x)
    | _ -> Scale (s, x)

  let neg_one = Field.(sub zero one)

  let sub t1 t2 = add t1 (scale t2 neg_one)

  let linear_combination (terms : (Field.t * t) list) : t =
    List.fold terms ~init:(constant Field.zero) ~f:(fun acc (c, t) ->
        add acc (scale t c) )

  let sum vs = linear_combination (List.map vs ~f:(fun v -> (Field.one, v)))

  module Infix = struct
    let ( + ) = add

    let ( - ) = sub

    let ( * ) c x = scale x c
  end
end
