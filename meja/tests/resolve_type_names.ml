open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

module A = struct
  include struct
    type t = int

    type var = int
  end
end

let (x : A.var) = 15

open A

let (y : var) = 20
