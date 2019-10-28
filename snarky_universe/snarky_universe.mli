module Intf = Intf

type proof_system = Groth16 | GrothMaller17

module Make (C : sig
  type field

  val curve : field Curve.t

  val system : proof_system
end) (Prover_state : sig
  type t
end)
() : Intf.S with type Impl.prover_state = Prover_state.t

val create :
     'f Curve.t
  -> proof_system
  -> (module Intf.S with type Impl.field = 'f and type Impl.prover_state = 'a)

val default :
     unit
  -> (module Intf.S
        with type Impl.field = Snarky.Backends.Bn128.Field.t
         and type Impl.prover_state = 'a)

module Default (Prover_state : sig
  type t
end)
() :
  Intf.S
  with type Impl.prover_state = Prover_state.t
   and type Impl.field = Snarky.Backends.Bn128.Field.t
