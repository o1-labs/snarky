open As_prover_intf

module Ref0 : sig
  type 'a t
end

module type S = sig
  module Types : Types.Types

  include Basic with type ('a, 'f) Provider.t = ('a, 'f) Types.Provider.t

  module Ref : sig
    type 'a t = 'a Ref0.t

    val create : ('a, 'f field) As_prover0.t -> ('a t, 'f field) Types.Checked.t

    val get : 'a t -> ('a, 'f field) As_prover0.t

    val set : 'a t -> 'a -> (unit, 'f field) As_prover0.t

    val typ : ('a t, 'a, 'f field) Types.Typ.t
  end
end

module type Extended = sig
  type field

  module Types : Types.Types

  include
    S
      with module Types := Types
      with type 'f field := field
       and type ('a, 'f) t := ('a, 'f) As_prover0.t

  type 'a t = ('a, field) As_prover0.t
end

module Make_ref_typ (Checked : Monad_let.S2) : sig
  val typ : ('a Ref0.t, 'a, 'b, (unit, 'c) Checked.t) Types.Typ.typ
end

module Make
    (Checked : Checked_intf.S)
    (As_prover : Basic
                   with type 'f field := 'f Checked.field
                    and type ('a, 'f) Provider.t =
                     ('a, 'f) Checked.Types.Provider.t) :
  S with module Types = Checked.Types with type 'f field = 'f Checked.field

include S with module Types = Checked_ast.Types with type 'f field := 'f

module Make_extended (Env : sig
  type field
end)
(Checked : Checked_intf.S with type 'f field := Env.field)
(As_prover : S
               with module Types := Checked.Types
               with type 'f field := Env.field) :
  Extended
    with module Types = Checked.Types
    with type field := Env.field
     and type 'a t = ('a, Env.field) As_prover.t
