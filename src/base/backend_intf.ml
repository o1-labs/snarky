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

module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : Snarky_intf.Bigint_intf.Extended with type field := Field.t

  module Cvar : Cvar_intf with type field := Field.t

  val field_size : Bigint.t

  module R1CS_constraint_system :
    Constraint_system_intf with module Field := Field and type cvar := Cvar.t
end
