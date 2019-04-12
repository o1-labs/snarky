module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x, y, _ = (1, 2, 3)

let (1 | _) = 2
