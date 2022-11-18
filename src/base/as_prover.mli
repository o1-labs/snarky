open As_prover_intf

module Ref0 : sig
  type 'a t
end

module type S = sig
  include
    Basic
      with type ('a, 'f) t = ('a, 'f) Types.As_prover.t
       and type ('a, 'f) Provider.t = ('a, 'f) Types.Provider.t

  module Ref : sig
    type 'a t = 'a Ref0.t

    val create :
      ('a, 'f field) Types.As_prover.t -> ('a t, 'f field) Types.Checked.t

    val get : 'a t -> ('a, 'f field) Types.As_prover.t

    val set : 'a t -> 'a -> (unit, 'f field) Types.As_prover.t

    val typ : ('a t, 'a, 'f field) Types.Typ.t
  end
end

module type Extended = sig
  type field

  include
    S
      with type 'f field := field
       and type ('a, 'f) t := ('a, 'f) Types.As_prover.t

  type 'a t = ('a, field) Types.As_prover.t
end

module Make_ref_typ (Checked : Monad_let.S2) : sig
  val typ : ('a Ref0.t, 'a, 'b, (unit, 'c) Checked.t) Types.Typ.typ
end

module Make
    (Checked : Checked_intf.S)
    (As_prover : Basic with type 'f field := 'f Checked.field) :
  S with type 'f field = 'f Checked.field

include S with type 'f field := 'f

module Make_extended (Env : sig
  type field
end)
(Checked : Checked_intf.S with type 'f field := Env.field)
(As_prover : S with type 'f field := Env.field) :
  Extended
    with type field := Env.field
     and type 'a t = ('a, Env.field) As_prover.t
