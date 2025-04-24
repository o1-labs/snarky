module type Constraint_intf = sig
  type var

  type field

  type t [@@deriving sexp]

  val boolean : var -> t

  val equal : var -> var -> t

  val r1cs : var -> var -> var -> t

  val square : var -> var -> t

  val eval : t -> (var -> field) -> bool

  val log_constraint : t -> (var -> field) -> string

end

module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  val field_size : Bigint.t

  module Cvar :
    Cvar.Intf
      with type field := Field.t
      and type t = Field.t Cvar.t

  module Constraint :
    Constraint_intf
       with type var := Cvar.t
       and type field := Field.t

  module R1CS_constraint_system :
    Constraint_system.S
      with module Field := Field
      with type constraint_ = Constraint.t

  module Run_state :
    Run_state_intf.S
      with type field := Field.t
       and type constraint_ := Constraint.t
end
