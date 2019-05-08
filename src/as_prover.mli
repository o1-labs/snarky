open As_prover_intf

module Make_basic (Checked : Checked_intf.S) :
  Basic with module Types = Checked.Types with type 'f field = 'f Checked.field

include Basic with module Types = Checked.Types with type 'f field := 'f

module Make (Env : sig
  type field
end)
(Checked : Checked_intf.S with type 'f field := Env.field)
(Basic : Basic
         with module Types := Checked.Types
         with type 'f field := Env.field) :
  S
  with module Types = Checked.Types
  with type field := Env.field
   and type ('a, 's) t = ('a, Env.field, 's) Basic.t
