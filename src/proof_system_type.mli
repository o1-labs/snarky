open Core_kernel

type ('a, 'b) t =
  | Standard of ('b, 'a) Type_equal.t
  | Signature_of_knowledge of ('b, ?message:bool array -> 'a) Type_equal.t

val standard : ('a, 'a) t

val signature_of_knowledge : ('a, ?message:bool array -> 'a) t
