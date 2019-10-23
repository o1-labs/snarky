module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type nonrec t = int Snarky.As_prover.Ref.t

type nonrec 'a u = 'a option Snarky.As_prover.Ref.t

type nonrec ('a, 'b) v = ('a * 'b) Snarky.As_prover.Ref.t

include struct
  type nonrec prover = A
end

type nonrec w = prover Snarky.As_prover.Ref.t

module A = struct
  type nonrec ('a, 'b) opaque = 'a * 'b

  type nonrec t = (int, bool) opaque

  include struct
    type nonrec u = (int, bool) opaque
  end

  let opaque = ()
end

type nonrec x = prover Snarky.As_prover.Ref.t
