open Core_kernel

module type Constraint_system_intf = sig
  module Field : sig
    type t
  end

  type t

  type cvar

  val create : unit -> t

  val finalize : t -> unit

  val digest : t -> Md5.t

  val set_primary_input_size : t -> int -> unit

  val get_primary_input_size : t -> int

  val get_rows_len : t -> int
end

module type Cvar_intf = sig
  type t

  type field

  module Unsafe : sig
    val of_index : int -> t
  end

  val eval : [ `Return_values_will_be_mutated of int -> field ] -> t -> field

  val constant : field -> t

  val add : t -> t -> t

  val negate : t -> t

  val scale : t -> field -> t

  val sub : t -> t -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : field -> t -> t

  val to_constant : t -> field option
end

module type Run_state_intf = sig
  module Field : sig
    type t

    module Vector : T
  end

  type t

  type cvar

  type constraint_system

  val make : int -> Field.Vector.t -> Field.Vector.t -> bool -> bool -> t

  val make_system : int -> Field.Vector.t -> Field.Vector.t -> bool -> bool -> t

  val add_constraint :
    t -> string option -> (cvar, Field.t) Constraint.basic -> unit

  val get_variable_value : t -> int -> Field.t

  val store_field_elt : t -> Field.t -> cvar

  val alloc_var : t -> unit -> cvar

  val has_witness : t -> bool

  val as_prover : t -> bool

  val set_as_prover : t -> bool -> unit

  val eval_constraints : t -> bool

  val next_auxiliary : t -> int
end

module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  module Cvar : Cvar_intf with type field := Field.t

  val field_size : Bigint.t

  module R1CS_constraint_system :
    Constraint_system_intf with module Field := Field and type cvar := Cvar.t

  module Run_state :
    Run_state_intf
      with module Field := Field
       and type cvar := Cvar.t
       and type constraint_system := R1CS_constraint_system.t
end
