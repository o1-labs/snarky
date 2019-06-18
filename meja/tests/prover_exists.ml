open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let x (x : Field.t) =
  let y = exists Field.typ ~compute:(fun () -> (As_prover.read Field.typ) x) in
  Field.Assert.equal x y
