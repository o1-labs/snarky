open Core_kernel

module type S = sig
type 'a t [@@deriving bin_io, sexp, eq, compare]

val map : 'a t -> f:('a -> 'b) -> 'b t
    end
