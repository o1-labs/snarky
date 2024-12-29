module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  val field_size : Bigint.t

  module R1CS_constraint_system : Constraint_system.S with module Field := Field

  module Run_state : Run_state_intf.S
end
