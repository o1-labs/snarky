open Core_kernel

module type Constraint_system_intf = sig
  module Field : sig
    type t
  end

  type t

  type cvar

  val create : unit -> t

  val finalize : t -> unit

  val add_constraint :
    ?label:string -> t -> (cvar, Field.t) Constraint.basic -> unit

  val digest : t -> Md5.t

  val set_primary_input_size : t -> int -> unit

  val set_auxiliary_input_size : t -> int -> unit

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

  val make :
       num_inputs:int
    -> input:Field.Vector.t
    -> next_auxiliary:int ref
    -> aux:Field.Vector.t
    -> ?system:constraint_system
    -> eval_constraints:bool
    -> ?log_constraint:
         (   ?at_label_boundary:[ `End | `Start ] * string
          -> (cvar, 'field) Constraint.t option
          -> unit )
    -> ?handler:Request.Handler.t
    -> with_witness:bool
    -> ?stack:string list
    -> ?is_running:bool
    -> unit
    -> t

  (** dumps some information about a state [t] *)
  val dump : t -> string

  val get_variable_value : t -> int -> Field.t

  val store_field_elt : t -> Field.t -> cvar

  val alloc_var : t -> unit -> cvar

  val has_witness : t -> bool

  val as_prover : t -> bool

  val set_as_prover : t -> bool -> unit

  val stack : t -> string list

  val set_stack : t -> string list -> t

  val log_constraint :
       t
    -> (   ?at_label_boundary:[ `Start | `End ] * string
        -> (cvar, Field.t) Constraint.t option
        -> unit )
       option

  val eval_constraints : t -> bool

  val system : t -> constraint_system option

  val handler : t -> Request.Handler.t

  val set_handler : t -> Request.Handler.t -> t

  val is_running : t -> bool

  val set_is_running : t -> bool -> t

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
