open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

module A = struct
  type t = int
end

let (x : A.t) = 15

open A

let (y : A.t) = 20
