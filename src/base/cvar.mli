type 'f t =
  | Constant of 'f
  | Var of int
  | Add of 'f t * 'f t
  | Scale of 'f * 'f t
[@@deriving sexp]

type 'f cvar = 'f t [@@deriving sexp]

val to_constant_and_terms :
     equal:('a -> 'a -> bool)
  -> add:('a -> 'b -> 'a)
  -> mul:('b -> 'b -> 'b)
  -> zero:'a
  -> one:'b
  -> 'b t
  -> 'a option * ('b * int) list

module type Intf = sig
  type field

  type t = field cvar [@@deriving sexp]

  module Unsafe : sig
    val of_index : int -> t
  end

  val eval : [ `Return_values_will_be_mutated of int -> field ] -> t -> field

  val constant : field -> t

  val to_constant_and_terms : t -> field option * (field * int) list

  val add : t -> t -> t

  val negate : t -> t

  val scale : t -> field -> t

  val sub : t -> t -> t

  val linear_combination : (field * t) list -> t

  val sum : t list -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : field -> t -> t

  val to_json : t -> Yojson.Safe.t

  val var_indices : t -> int list

  val to_constant : t -> field option
end

module Unsafe : sig
  val of_index : int -> 'field t
end

module Make (Field : Snarky_intf.Field.Extended) :
  Intf with type field := Field.t
