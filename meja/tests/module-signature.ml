module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
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
