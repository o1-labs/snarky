open Core_kernel

type 'a t = 'a * 'a [@@deriving bin_io, sexp, eq, compare]

let map (x1, x2) ~f = (f x1, f x2)

let map2 (x1, x2) (y1, y2) ~f = (f x1 y1, f x2 y2)

let iter (x1, x2) ~f = f x1 ; f x2
