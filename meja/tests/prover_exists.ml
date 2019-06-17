open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let x __implicit1__ __implicit2__ (x : Field.t) =
  let y =
    exists __implicit1__ ~compute:(fun () -> (As_prover.read __implicit2__) x)
  in
  Field.Assert.equal x y
