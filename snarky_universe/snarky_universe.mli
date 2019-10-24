type proof_system = Groth16 | GrothMaller17

module Make (C : sig
  val curve : Curve.t

  val system : proof_system
end)
() : Intf.S

val create : Curve.t -> proof_system -> (module Intf.S)

val default : unit -> (module Intf.S)
