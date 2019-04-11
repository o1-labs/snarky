module Make
    (Checked : Checked_intf.S)
    (As_prover : As_prover_intf.Basic
                 with type 'f field := 'f Checked.field
                 with module Types := Checked.Types) :
  As_prover_intf.S
  with type 'f field = 'f Checked.field
  with module Types = Checked.Types

include
  As_prover_intf.S with type 'f field := 'f with module Types := Checked.Types

module Make_extended (Env : sig
  type field
end)
(As_prover : As_prover_intf.S with type 'f field := Env.field) :
  As_prover_intf.Extended
  with type field = Env.field
  with module Types = As_prover.Types
