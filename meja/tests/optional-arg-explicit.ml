module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let f ?(x : int) = x

let g = f ~x:12
