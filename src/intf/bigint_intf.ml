module Bignum_bigint = Bigint

module type Basic = sig
  type t

  val test_bit : t -> int -> bool
end

module type Convertible = sig
  include Basic

  val of_data : Core_kernel.Bigstring.t -> bitcount:int -> t

  val length_in_bytes : int

  val of_decimal_string : string -> t

  val of_numeral : string -> base:int -> t
end

module type S = sig
  include Basic

  type field

  val of_field : field -> t
end

module type Extended = sig
  include Convertible

  include Core_kernel.Binable.S with type t := t

  type field

  val of_field : field -> t

  val to_field : t -> field

  val compare : t -> t -> int
end

module type Bignum_bigint_conv = sig
  type t

  val of_bignum_bigint : Bignum_bigint.t -> t

  val to_bignum_bigint : t -> Bignum_bigint.t
end

module type Full = sig
  include Extended

  include Bignum_bigint_conv with type t := t
end
