
module Make
    (Backend : Backend_intf.S)
    (Types : Types.Types with type field_var = Backend.Cvar.t)
    (Basic : Checked_intf.Basic
               with type constraint_ = Backend.Constraint.t
               with module Types := Types)
    (As_prover : As_prover_intf.S with module Types := Types) :
  Checked_intf.S
    with module Types := Types
    with type run_state = Basic.run_state
     and type constraint_ = Basic.constraint_