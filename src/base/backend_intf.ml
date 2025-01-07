module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  val field_size : Bigint.t

  module Constraint : sig
    type t [@@deriving sexp]

    val boolean : Field.t Cvar.t -> t

    val equal : Field.t Cvar.t -> Field.t Cvar.t -> t

    val r1cs : Field.t Cvar.t -> Field.t Cvar.t -> Field.t Cvar.t -> t

    val square : Field.t Cvar.t -> Field.t Cvar.t -> t

    val eval : t -> (Field.t Cvar.t -> Field.t) -> bool

    val log_constraint : t -> (Field.t Cvar.t -> Field.t) -> string
  end

  module R1CS_constraint_system :
    Constraint_system.S
      with module Field := Field
      with type constraint_ = Constraint.t

  module Run_state :
    Run_state_intf.S
      with type field := Field.t
       and type constraint_ := Constraint.t
end
