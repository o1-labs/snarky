module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let f x = ()

let ignore x = ()

let g x = ignore x
