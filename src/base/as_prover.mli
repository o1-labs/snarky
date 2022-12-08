open As_prover_intf

module Ref0 : sig
  type 'a t
end

module type S = sig
  module Types : Types.Types

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

  module Types : Types.Types

  include
    S
      with module Types := Types
      with type 'f field := field
       and type ('a, 'f) t := ('a, 'f) Types.As_prover.t

  type 'a t = ('a, field) Types.As_prover.t
end

module Make
    (Checked : Checked_intf.S)
    (As_prover : Basic
                   with type ('a, 'f) t := ('a, 'f) Checked.Types.As_prover.t
                    and type 'f field := 'f Checked.field
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
