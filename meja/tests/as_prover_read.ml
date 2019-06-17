open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let f (x : Boolean.var) =
  let g () =
    let (y : bool) = (As_prover.read Boolean.typ) x in
    ()
  in
  g ()
