open Core_kernel

module type S = sig
  type t [@@deriving eq, bin_io, sexp, yojson, compare, hash]

  include Stringable.S with type t := t

  val of_int : int -> t

  val to_int_exn : t -> int

  val ( < ) : t -> t -> bool

  val ( + ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( // ) : t -> t -> t

  val ( % ) : t -> t -> t

  val shift_left : t -> int -> t

  val shift_right : t -> int -> t

  val log_and : t -> t -> t

  val log_or : t -> t -> t

  val test_bit : t -> int -> bool

  val num_bits : t -> int

  val of_bytes : string -> t

  val to_bytes : t -> string
end
