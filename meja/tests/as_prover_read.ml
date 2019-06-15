module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let f (x : Boolean.var) =
  let g () =
    let (y : bool) = (As_prover.read Boolean.typ) x in
    ()
  in
  g ()
