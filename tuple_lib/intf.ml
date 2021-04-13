open Core_kernel

module type S = sig
  type 'a t [@@deriving bin_io, sexp, eq, compare]

  val iter : 'a t -> f:('a -> unit) -> unit

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
end
