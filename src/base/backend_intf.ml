module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  val field_size : Bigint.t

  module Cvar : Cvar.Intf with type field := Field.t and type t = Field.t Cvar.t

  module Constraint : sig
    type t [@@deriving sexp]

    val boolean : Cvar.t -> t

    val equal : Cvar.t -> Cvar.t -> t

    val r1cs : Cvar.t -> Cvar.t -> Cvar.t -> t

    val square : Cvar.t -> Cvar.t -> t

    val eval : t -> (Cvar.t -> Field.t) -> bool

    val log_constraint : t -> (Cvar.t -> Field.t) -> string
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
