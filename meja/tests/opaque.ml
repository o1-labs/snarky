module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type t = (_, int) Snarky.As_prover.Ref.t

type 'a u = (_, 'a option) Snarky.As_prover.Ref.t

type ('a, 'b) v = (_, 'a * 'b) Snarky.As_prover.Ref.t

include struct
  type prover = A
end

type w = (_, prover) Snarky.As_prover.Ref.t

module A = struct
  type ('a, 'b) opaque = 'a * 'b

  type t = (int, bool) opaque

  include struct
    type u = (int, bool) opaque
  end

  let opaque = ()
end

type x = (_, prover) Snarky.As_prover.Ref.t
