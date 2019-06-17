open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let x = if true then true else false

let y = if true then ()

let z x y z = if x then y else if true then z else y
