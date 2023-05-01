open Core_kernel

type 'f t =
  | Constant of 'f
  | Var of int
  | Add of 'f t * 'f t
  | Scale of 'f * 'f t
[@@deriving sexp]

type 'f cvar = 'f t [@@deriving sexp]

let to_constant_and_terms ~equal ~add ~mul ~zero ~one =
  let rec go scale constant terms = function
    | Constant c ->
        (add constant (mul scale c), terms)
    | Var v ->
        (constant, (scale, v) :: terms)
    | Scale (s, t) ->
        go (mul s scale) constant terms t
    | Add (x1, x2) ->
        let c1, terms1 = go scale constant terms x1 in
        go scale c1 terms1 x2
  in
  fun t ->
    let c, ts = go one zero [] t in
    let c = if equal c zero then None else Some c in
    (c, ts)

module Unsafe = struct
  let of_index v = Var v
end

module Make (Field : Snarky_intf.Field.Extended) = struct
  type t = Field.t cvar [@@deriving sexp]

  let length _ = failwith "TODO"

  module Unsafe = Unsafe

  let scratch = Field.of_int 0

  let eval (`Return_values_will_be_mutated context) t0 =
    let open Field in
    let rec go = function
      | Constant c, Some scale ->
          c * scale
      | Constant c, None ->
          c
      | Var v, Some scale ->
          context v * scale
      | Var v, None ->
          context v
      | Scale (s, t), Some scale ->
          go (t, Some (scale * s))
      | Scale (s, t), None ->
          go (t, Some s)
      | Add (t1, t2), Some scale ->
          (go (t1, None) + go (t2, None)) * scale
      | Add (t1, t2), None ->
          go (t1, None) + go (t2, None)
    in
    go (t0, None)

  let constant c = Constant c

  let to_constant_and_terms =
    let rec go scale constant terms = function
      | Constant c ->
          (Field.add constant (Field.mul scale c), terms)
      | Var v ->
          (constant, (scale, v) :: terms)
      | Scale (s, t) ->
          go (Field.mul s scale) constant terms t
      | Add (x1, x2) ->
          let c1, terms1 = go scale constant terms x1 in
          go scale c1 terms1 x2
    in
    fun t ->
      let c, ts = go Field.one Field.zero [] t in
      let c = if Field.equal c Field.zero then None else Some c in
      (c, ts)

  let add x y =
    match (x, y) with
    | Constant x, _ when Field.(equal x zero) ->
        y
    | _, Constant y when Field.(equal y zero) ->
        x
    | Constant x, Constant y ->
        Constant (Field.add x y)
    | _, _ ->
        Add (x, y)

  let scale x s =
    if Field.(equal s zero) then Constant Field.zero
    else if Field.(equal s one) then x
    else
      match x with
      | Constant x ->
          Constant (Field.mul x s)
      | Scale (sx, x) ->
          Scale (Field.mul sx s, x)
      | _ ->
          Scale (s, x)

  let neg_one = Field.(sub zero one)

  let sub t1 t2 =
    match (t1, t2) with
    | Constant x, Constant y ->
        Constant (Field.sub x y)
    | _ ->
        add t1 (scale t2 neg_one)

  let linear_combination (terms : (Field.t * t) list) : t =
    List.fold terms ~init:(constant Field.zero) ~f:(fun acc (c, t) ->
        add acc (scale t c) )

  let sum vs = linear_combination (List.map vs ~f:(fun v -> (Field.one, v)))

  let ( + ) = add

  let ( - ) = sub

  let ( * ) c x = scale x c

  let negate x = scale x neg_one

  let to_json x =
    let singleton = Map.singleton (module Int) in
    let join = Map.merge_skewed ~combine:(fun ~key:_ -> Field.add) in
    let rec go scale = function
      | Constant f ->
          singleton 0 (Field.mul scale f)
      | Var i ->
          singleton i scale
      | Add (x, y) ->
          join (go scale x) (go scale y)
      | Scale (s, x) ->
          go Field.(scale * s) x
    in
    let map = go Field.one x in
    `Assoc
      (List.filter_map (Map.to_alist map) ~f:(fun (i, f) ->
           if Field.(equal f zero) then None
           else Some (Int.to_string i, `String (Field.to_string f)) ) )
end
