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

  val get_public_input_size : t -> int Core_kernel.Set_once.t

  val get_rows_len : t -> int
end

module Constraint_system_GADT = struct
  type ('f, 'cvar) t =
    | T :
        (module Constraint_system_intf
           with type Field.t = 'f
            and type t = 't
            and type cvar = 'cvar )
        * 't
        -> ('f, 'cvar) t
end

module type Cvar_intf = sig
  type t

  type field

  val length : t -> int

  module Unsafe : sig
    val of_index : int -> t
  end

  val eval : [ `Return_values_will_be_mutated of int -> field ] -> t -> field

  val constant : field -> t

  val to_constant_and_terms : t -> field option * (field * int) list

  val add : t -> t -> t

  val negate : t -> t

  val scale : t -> field -> t

  val sub : t -> t -> t

  val linear_combination : (field * t) list -> t

  val sum : t list -> t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : field -> t -> t

  val var_indices : t -> int list

  val to_constant : t -> field option
end

module type Run_state_intf = sig
  module Vector : sig
    type 'elt t =
      | T :
          (module Snarky_intf.Vector.S with type elt = 'elt and type t = 't)
          * 't Base.Type_equal.Id.t
          * 't
          -> 'elt t

    val unit : unit Base.Type_equal.Id.t

    val null : 'a t

    val get : 'x t -> int -> 'x

    val emplace_back : 'x t -> 'x -> unit
  end

  type t

  type cvar

  type field

  val make :
       num_inputs:int
    -> input:'field Vector.t
    -> next_auxiliary:int ref
    -> aux:'field Vector.t
    -> ?system:(field, cvar) Constraint_system_GADT.t
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

  val get_variable_value : t -> int -> field

  val store_field_elt : t -> field -> cvar

  val alloc_var : t -> unit -> cvar

  val has_witness : t -> bool

  val as_prover : t -> bool

  val set_as_prover : t -> bool -> unit

  val stack : t -> string list

  val set_stack : t -> string list -> t

  val log_constraint :
       t
    -> (   ?at_label_boundary:[ `Start | `End ] * string
        -> (cvar, field) Constraint.t option
        -> unit )
       option

  val eval_constraints : t -> bool

  val system : t -> (field, cvar) Constraint_system_GADT.t option

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
    Run_state_intf with type field := Field.t and type cvar := Cvar.t
end
