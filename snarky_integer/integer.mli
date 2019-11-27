(** Positive integers as field elements.

    These operations assert that the value of the field element does not exceed
    the largest element in the field -- ie. that the operations do not
    overflow.

    Whenever possible, the bit representation is cached to avoid recomputing
    it.
*)

open Snarky
open Snark
open Bitstring_lib

type 'f t

val constant : ?length:int -> m:'f m -> Bigint.t -> 'f t
(** Create an value representing the given constant value.

    The bit representation of the constant is cached, and is padded to [length]
    when given.
*)

val shift_left : m:'f m -> 'f t -> int -> 'f t
(** [shift_left ~m x k] is equivalent to multiplying [x] by [2^k].

    The result has a cached bit representation whenever the given [x] had a
    cached bit representation.
*)

val of_bits : m:'f m -> 'f Cvar.t Boolean.t Bitstring.Lsb_first.t -> 'f t
(** Create a value from the given bit string.

    The given bit representation is cached.
*)

val to_bits :
  ?length:int -> m:'f m -> 'f t -> 'f Cvar.t Boolean.t Bitstring.Lsb_first.t
(** Compute the bit representation of the given integer.

    If the bit representation has already been cached, it is returned and no
    additional constraints are added. If the representation is computed, the
    value is updated to include the cache.
*)

val to_bits_exn : 'f t -> 'f Cvar.t Boolean.t Bitstring.Lsb_first.t
(** Return the cached bit representation, or raise an exception if the bit
    representation has not been cached.
*)

val to_bits_opt : 'f t -> 'f Cvar.t Boolean.t Bitstring.Lsb_first.t option
(** Returns [Some bs] for [bs] the cached bit representation, or [None] if the
    bit representation has not been cached.
*)

val div_mod : m:'f m -> 'f t -> 'f t -> 'f t * 'f t
(** [div_mod ~m a b = (q, r)] such that [a = q * b + r] and [r < b].

    The bit representations of [q] and [r] are calculated and cached.

    NOTE: This uses approximately [log2(a) + 2 * log2(b)] constraints.
*)

val to_field : 'f t -> 'f Cvar.t

val create : value:'f Cvar.t -> upper_bound:Bigint.t -> 'f t

val min : m:'f m -> 'f t -> 'f t -> 'f t
(** [min ~m x y] returns a value equal the lesser of [x] and [y].

    The result does not carry a cached bit representation.
*)

val if_ : m:'f m -> 'f Cvar.t Boolean.t -> then_:'f t -> else_:'f t -> 'f t

val succ : m:'f m -> 'f t -> 'f t
(** [succ ~m x] computes the successor [x+1] of [x].

    The result does not carry a cached bit representation.
*)

val succ_if : m:'f m -> 'f t -> 'f Cvar.t Boolean.t -> 'f t
(** [succ_if ~m x b] computes the integer [x+1] if [b] is [true], or [x]
    otherwise.

    The result does not carry a cached bit representation.
*)

val equal : m:'f m -> 'f t -> 'f t -> 'f Cvar.t Boolean.t

val lt : m:'f m -> 'f t -> 'f t -> 'f Cvar.t Boolean.t

val lte : m:'f m -> 'f t -> 'f t -> 'f Cvar.t Boolean.t

val gt : m:'f m -> 'f t -> 'f t -> 'f Cvar.t Boolean.t

val gte : m:'f m -> 'f t -> 'f t -> 'f Cvar.t Boolean.t

val add : m:'f m -> 'f t -> 'f t -> 'f t
(** [add ~m x y] computes [x + y].

    The result does not carry a cached bit representation.
*)

val mul : m:'f m -> 'f t -> 'f t -> 'f t
(** [mul ~m x y] computes [x * y].

    The result does not carry a cached bit representation.
*)

val subtract_unpacking : m:'f m -> 'f t -> 'f t -> 'f t
(** [subtract_unpacking ~m x y] computes [x - y].

    The bit representation is calculated to ensure that [0 <= x - y], and is
    cached in the result.

    NOTE: This uses approximately [log2(x)] constraints.
*)
