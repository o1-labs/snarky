module type S = sig
  type t [@@deriving sexp, bin_io]

  val of_int : int -> t

  val of_string : string -> t

  val to_string : t -> string

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

  module Vector : Vector.S with type elt = t
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
