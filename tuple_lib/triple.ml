open Core_kernel

type 'a t = 'a * 'a * 'a [@@deriving bin_io, sexp, eq, compare]

let map (x1, x2, x3) ~f = (f x1, f x2, f x3)

let map2 (x1, x2, x3) (y1, y2, y3) ~f = (f x1 y1, f x2 y2, f x3 y3)

let iter (x1, x2, x3) ~f = f x1 ; f x2 ; f x3
