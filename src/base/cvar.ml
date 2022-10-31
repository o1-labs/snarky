open Core_kernel

(** Main type representing a [Cvar.t]. It represents a small AST over a field ['f]. 
    We build this AST instead of directly constraining [Cvar.t]s in order to
    perform some optimizations (mostly merging generic gates).
    *)
type 'f t =
  | Constant of 'f
  | Var of int
  | Add of 'f t * 'f t
  | Scale of 'f * 'f t
[@@deriving sexp]

(** Alias to [t]. *)
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
  (** Directly converts a number into a variable.
      This operation is considered unsafe as you would normally increment a counter to obtain a variable that doesn't collide with another one.
    *)
  let of_index v = Var v
end

(***********************
 *  SIGS FOR FUNCTORS  *
 ***********************)

module type cvar_module_type = sig
  type field

  type var

  type t = field cvar [@@deriving sexp]

  val length : t -> int

  module Unsafe : sig
    val of_index : int -> t
  end

  val eval : [ `Return_values_will_be_mutated of int -> field ] -> t -> field

  val constant : field -> t

  val to_constant_and_terms : t -> field option * (field * var) list

  val add : t -> t -> t

  val negate : t -> t

  val scale : t -> field -> t

  val sub : t -> t -> t

  val linear_combination : (field * t) list -> t

  val sum : t list -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : field -> t -> t
end

(** An extension of [cvar_module_type]. *)
module type cvar_module_type_extended = sig
  include cvar_module_type

  val var_indices : t -> int list

  val to_constant : t -> field option
end

(******************
 *    FUNCTOR     *
 ******************)

module Make
    (Field : Snarky_intf.Field.Extended) (Var : sig
      include Comparable.S

      include Sexpable.S with type t := t

      val create : int -> t
    end) : cvar_module_type with type field := Field.t and type var := Var.t =
struct
  type t = Field.t cvar [@@deriving sexp]

  let length _ = failwith "TODO"

  module Unsafe = Unsafe

  let scratch = Field.of_int 0

  let eval (`Return_values_will_be_mutated context) t0 =
    let open Field in
    let res = of_int 0 in
    let rec go ~can_mutate_scale scale = function
      | Constant c when can_mutate_scale ->
          scale *= c ; res += scale
      | Constant c ->
          Mutable.copy ~over:scratch c ;
          scratch *= scale ;
          res += scratch
      | Var v ->
          let v = context v in
          v *= scale ; res += v
      | Scale (s, t) when can_mutate_scale ->
          scale *= s ;
          go scale ~can_mutate_scale t
      | Scale (s, t) ->
          go (mul s scale) ~can_mutate_scale:true t
      | Add (t1, t2) ->
          go scale ~can_mutate_scale:false t1 ;
          go scale ~can_mutate_scale t2
    in
    go one ~can_mutate_scale:false t0 ;
    res

  let constant c = Constant c

  let to_constant_and_terms =
    let rec go scale constant terms = function
      | Constant c ->
          (Field.add constant (Field.mul scale c), terms)
      | Var v ->
          (constant, (scale, Var.create v) :: terms)
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
end
