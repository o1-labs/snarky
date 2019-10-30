module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

include struct
  type nonrec t = int Snarky.As_prover.Ref.t

  and t = int

  let typ = Snarky.Typ.Internal.ref ()
end

type nonrec 'a u = 'a option Snarky.As_prover.Ref.t

type nonrec ('a, 'b) v = ('a * 'b) Snarky.As_prover.Ref.t

include struct
  type nonrec prover = A
end

include struct
  type nonrec w = prover Snarky.As_prover.Ref.t

  and w = prover

  let w_typ = Snarky.Typ.Internal.ref ()
end

module A = struct
  type nonrec ('a, 'b) opaque = 'a * 'b

  type nonrec t = (int, bool) opaque

  include struct
    type nonrec u = (int, bool) opaque
  end

  let opaque = ()
end

include struct
  type nonrec x = prover Snarky.As_prover.Ref.t

  and x = prover

  let x_typ = w_typ
end
