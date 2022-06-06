open As_prover_intf

module Make
    (Checked : Checked_intf.S)
    (As_prover : Basic
                   with type ('a, 'f) t := ('a, 'f) Checked.Types.As_prover.t
                    and type 'f field := 'f Checked.field
                    and type ('a, 'f) Provider.t =
                     ('a, 'f) Checked.Types.Provider.t) :
  S with module Types = Checked.Types with type 'f field = 'f Checked.field

include S with module Types = Checked.Types with type 'f field := 'f

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
