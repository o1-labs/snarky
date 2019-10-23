module Make (C : sig
  val curve : Curve.t
end)
() : Intf.S

module Bn128 () : Intf.S
