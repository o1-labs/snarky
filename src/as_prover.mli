open As_prover_intf

module Make
    (Checked : Checked_intf.S)
    (As_prover : Basic
                 with type ('a, 'f, 's) t :=
                             ('a, 'f, 's) Checked.Types.As_prover.t
                  and type 'f field := 'f Checked.field
                  and type ('a, 'f, 's) Provider.t =
                             ('a, 'f, 's) Checked.Types.Provider.t) :
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
   and type ('a, 's) t = ('a, Env.field, 's) As_prover.t
