module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type t = (_, int) Snarky.Handle.t

type 'a u = (_, 'a option) Snarky.Handle.t

type ('a, 'b) v = (_, 'a * 'b) Snarky.Handle.t

include struct
  type prover = A
end

type t = (_, prover) Snarky.Handle.t
