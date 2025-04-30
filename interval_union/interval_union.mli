open Core_kernel

module Interval : sig
  type t = int * int [@@deriving eq, sexp]

  val gen : t Quickcheck.Generator.t

  val gen_from : int -> t Quickcheck.Generator.t

  val before : t -> t -> bool
end

type t = Interval.t list [@@deriving eq, sexp]

val empty : t

val of_interval : Interval.t -> t

val of_intervals_exn : Interval.t list -> t

val disjoint_union_exn : t -> t -> t

val disjoint : t -> t -> bool

val to_interval : t -> Interval.t Or_error.t

val right_endpoint : t -> int option

val left_endpoint : t -> int option

(* For testing *)
val canonicalize : t -> t

val invariant : t -> unit

val gen : t Quickcheck.Generator.t

val gen_from : ?min_size:int -> int -> t Quickcheck.Generator.t

val gen_disjoint_pair : (t * t) Quickcheck.Generator.t
