open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let foo () =
  let (x : int) = failwith "TODO" in
  let (y : string) = failwith "TODO" in
  ()
