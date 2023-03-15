(** Positive integers as field elements.

    These operations assert that the value of the field element does not exceed
    the largest element in the field -- ie. that the operations do not
    overflow.

    Whenever possible, the bit representation is cached to avoid recomputing
    it.
*)

open Snarky_backendless
open Snark
open Bitstring_lib

module Interval : sig
  open Util

  type t = Constant of B.t | Less_than of B.t
end

type ('f, 'field_var) t =
  { value : 'field_var
  ; interval : Interval.t
  ; mutable bits : 'field_var Boolean.t list option
  }

(** Create an value representing the given constant value.

    The bit representation of the constant is cached, and is padded to [length]
    when given.
*)
val constant :
  ?length:int -> m:('f, 'field_var, 'state) m -> Bigint.t -> ('f, 'field_var) t

(** [shift_left ~m x k] is equivalent to multiplying [x] by [2^k].

    The result has a cached bit representation whenever the given [x] had a
    cached bit representation.
*)
val shift_left :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> int
  -> ('f, 'field_var) t

(** Create a value from the given bit string.

    The given bit representation is cached.
*)
val of_bits :
     m:('f, 'field_var, 'state) m
  -> 'field_var Boolean.t Bitstring.Lsb_first.t
  -> ('f, 'field_var) t

(** Compute the bit representation of the given integer.

    If the bit representation has already been cached, it is returned and no
    additional constraints are added. If the representation is computed, the
    value is updated to include the cache.
*)
val to_bits :
     ?length:int
  -> m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> 'field_var Boolean.t Bitstring.Lsb_first.t

(** Return the cached bit representation, or raise an exception if the bit
    representation has not been cached.
*)
val to_bits_exn :
  ('f, 'field_var) t -> 'field_var Boolean.t Bitstring.Lsb_first.t

(** Returns [Some bs] for [bs] the cached bit representation, or [None] if the
    bit representation has not been cached.
*)
val to_bits_opt :
  ('f, 'field_var) t -> 'field_var Boolean.t Bitstring.Lsb_first.t option

(** [div_mod ~m a b = (q, r)] such that [a = q * b + r] and [r < b].

    The bit representations of [q] and [r] are calculated and cached.

    NOTE: This uses approximately [log2(a) + 2 * log2(b)] constraints.
*)
val div_mod :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t * ('f, 'field_var) t

val to_field : ('f, 'field_var) t -> 'field_var

val create : value:'field_var -> upper_bound:Bigint.t -> ('f, 'field_var) t

(** [min ~m x y] returns a value equal the lesser of [x] and [y].

    The result does not carry a cached bit representation.
*)
val min :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t

val if_ :
     m:('f, 'field_var, 'state) m
  -> 'field_var Boolean.t
  -> then_:('f, 'field_var) t
  -> else_:('f, 'field_var) t
  -> ('f, 'field_var) t

(** [succ ~m x] computes the successor [x+1] of [x].

    The result does not carry a cached bit representation.
*)
val succ :
  m:('f, 'field_var, 'state) m -> ('f, 'field_var) t -> ('f, 'field_var) t

(** [succ_if ~m x b] computes the integer [x+1] if [b] is [true], or [x]
    otherwise.

    The result does not carry a cached bit representation.
*)
val succ_if :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> 'field_var Boolean.t
  -> ('f, 'field_var) t

val equal :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> 'field_var Boolean.t

val lt :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> 'field_var Boolean.t

val lte :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> 'field_var Boolean.t

val gt :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> 'field_var Boolean.t

val gte :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> 'field_var Boolean.t

(** [add ~m x y] computes [x + y].

    The result does not carry a cached bit representation.
*)
val add :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t

(** [mul ~m x y] computes [x * y].

    The result does not carry a cached bit representation.
*)
val mul :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t

(** [subtract_unpacking ~m x y] computes [x - y].

    The bit representation is calculated to ensure that [0 <= x - y], and is
    cached in the result.

    NOTE: This uses approximately [log2(x)] constraints.
*)
val subtract_unpacking :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t

(** [subtract_unpacking_or_zero ~m x y] computes [x - y].

    - If the argument to [`Underflow] is true, [x < y] and the returned integer
      value is pinned to [zero].
    - If the argument to [`Underflow] is false, [x >= y] and the returned
      integer value is equal to [x - y].

    The bit representation is calculated to check for underflow.

    NOTE: This uses approximately [log2(x)] constraints.
*)
val subtract_unpacking_or_zero :
     m:('f, 'field_var, 'state) m
  -> ('f, 'field_var) t
  -> ('f, 'field_var) t
  -> [ `Underflow of 'field_var Boolean.t ] * ('f, 'field_var) t
