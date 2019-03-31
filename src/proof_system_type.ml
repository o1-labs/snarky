open Core_kernel

type ('a, 'b) t =
  | Standard of ('b, 'a) Type_equal.t
  | Signature_of_knowledge of ('b, ?message:bool array -> 'a) Type_equal.t

let standard : ('a, 'a) t = Standard Type_equal.T

let signature_of_knowledge : type a. (a, ?message:bool array -> a) t =
  Signature_of_knowledge Type_equal.T
