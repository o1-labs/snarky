module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  val field_size : Bigint.t

  module R1CS_constraint_system :
    Constraint_system.S
      with module Field := Field
      with type constraint_ = (Field.t Cvar.t, Field.t) Constraint.t

  module Run_state :
    Run_state_intf.S
      with type field := Field.t
       and type constraint_ := R1CS_constraint_system.constraint_
end
