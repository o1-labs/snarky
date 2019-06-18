open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

module type S = sig
  val x : int

  type x

  val x_inst : x

  module X : sig
    type t = ..

    type t += B | A
  end

  type X.t += E | D | C

  module Y : sig
    type t = A | B

    open X

    type t += H | G | F
  end
end
