open Core_kernel
open Tuple_lib

(** The [Bitstring_lib] module provides simple utilities for working with
    lists used to represent sequences of bits or other elements.
    It includes different orderings (endianness) and padding utilities.
*)

module type S = sig
  type 'a t = private 'a list

  include Container.S1 with type 'a t := 'a t

  (** [of_list lst] constructs a bitstring from a list.
      Identity function: no transformation is applied. *)
  val of_list : 'a list -> 'a t

  (** [init n ~f] creates a bitstring of length [n] with elements [f 0], ..., [f (n-1)]. *)
  val init : int -> f:(int -> 'a) -> 'a t

  (** [map t ~f] applies [f] to each element in the bitstring. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** [pad ~padding_length ~zero t] pads the bitstring [t] with [zero] elements
      at the correct side based on ordering until it reaches length [padding_length].
      If already equal or longer, returns unchanged. *)
  val pad : padding_length:int -> zero:'a -> 'a t -> 'a t
end

(** [Msb_first] treats lists as having the most significant element at the head.
    Padding is added to the right end of the list. *)
module rec Msb_first : sig
  include S

  (** [of_lsb_first xs] converts a least-significant-bit-first list to MSB-first. *)
  val of_lsb_first : 'a Lsb_first.t -> 'a t
end

(** [Lsb_first] treats lists as having the least significant element at the head.
    Padding is added to the left end of the list. *)
and Lsb_first : sig
  include S

  (** [of_msb_first xs] converts a most-significant-bit-first list to LSB-first. *)
  val of_msb_first : 'a Msb_first.t -> 'a t
end

(** [pad_to_triple_list ~default xs] converts a list [xs] to a list of 3-tuples.
    If [xs] does not divide evenly into triples, missing values are filled in with [default].
*)
val pad_to_triple_list : default:'a -> 'a list -> 'a Triple.t list
