module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type nonrec 'a t = int

let x : bool t = 15

let f (y : int t) = y

let g = f x
