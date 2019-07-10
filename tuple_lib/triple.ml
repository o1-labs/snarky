open Core_kernel

type 'a t = 'a * 'a * 'a [@@deriving bin_io, sexp, eq, compare]

let map (x1, x2, x3) ~f = (f x1, f x2, x3)
