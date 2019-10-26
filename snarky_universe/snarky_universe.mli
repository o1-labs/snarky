type proof_system = Groth16 | GrothMaller17

module Make (C : sig
  type field

  val curve : field Curve.t

  val system : proof_system
end)
() : Intf.S

val create :
  'f Curve.t -> proof_system -> (module Intf.S with type Impl.field = 'f)

val default :
  unit -> (module Intf.S with type Impl.field = Snarky.Backends.Bn128.Field.t)
