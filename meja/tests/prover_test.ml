module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let f x = ()

let ignore (x : Field.t) = ()

let g (x : Field.t) =
  let f (x : Field.Constant.t) = () in
  ()
