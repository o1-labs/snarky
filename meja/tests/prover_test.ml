open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let f x = ()

let ignore (x : Field.t) = ()

let g (x : Field.t) =
  let f (x : Field.Constant.t) = () in
  ()
