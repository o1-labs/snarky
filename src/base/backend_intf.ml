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

  val to_json : t -> string

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

  val make : int -> bool -> bool -> t

  val make_system : int -> bool -> bool -> t

  val add_constraint :
    ?label:string -> t -> (cvar, Field.t) Constraint.basic -> unit

  val get_variable_value : t -> int -> Field.t

  val store_field_elt : t -> Field.t -> cvar

  val alloc_var : t -> cvar

  val has_witness : t -> bool

  val set_public_inputs : t -> Field.Vector.t -> unit

  val get_private_inputs : t -> Field.Vector.t

  val as_prover : t -> bool

  val set_as_prover : t -> bool -> unit

  val eval_constraints : t -> bool

  val system : t -> constraint_system option

  val finalize : t -> unit

  val next_auxiliary : t -> int
end

module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  module Cvar : Cvar_intf with type field := Field.t

  val field_size : Bigint.t

  module Constraint_system :
    Constraint_system_intf with module Field := Field and type cvar := Cvar.t

  module Run_state :
    Run_state_intf
      with module Field := Field
       and type cvar := Cvar.t
       and type constraint_system := Constraint_system.t
end
