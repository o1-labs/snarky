module type S = sig
  type t [@@deriving sexp, bin_io]

  val of_int : int -> t

  val one : t

  val zero : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val inv : t -> t

  val square : t -> t

  val sqrt : t -> t

  val is_square : t -> bool

  val equal : t -> t -> bool

  val size_in_bits : int

  val print : t -> unit

  val random : unit -> t

  module Mutable : sig
    val add : t -> other:t -> unit

    val mul : t -> other:t -> unit

    val sub : t -> other:t -> unit

    val copy : over:t -> t -> unit
  end

  val ( += ) : t -> t -> unit

  val ( -= ) : t -> t -> unit

  val ( *= ) : t -> t -> unit

  module Vector : Snarky_intf.Vector.S with type elt = t
end

module type Extended = sig
  include S

  val to_string : t -> string

  val negate : t -> t

  val ( + ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( / ) : t -> t -> t
end

module type Full = sig
  type t [@@deriving bin_io, sexp, hash, compare]

  include Extended with type t := t

  include Core_kernel.Stringable.S with type t := t

  val size : Bigint.t

  val unpack : t -> bool list

  val project_reference : bool list -> t

  val project : bool list -> t
end
