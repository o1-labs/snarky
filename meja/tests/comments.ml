module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x = (1, 2, 7)

let y =
  let i, j, k = x in
  (i, j)

let f = 1

let g = f + 2

let h = ()

let i = ()
