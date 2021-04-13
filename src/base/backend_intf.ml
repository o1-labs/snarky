open Core_kernel

module type Constraint_system_intf = sig
  module Field : sig
    type t
  end

  type t

  val create : unit -> t

  val finalize : t -> unit

  val add_constraint :
    ?label:string -> t -> (Field.t Cvar.t, Field.t) Constraint.basic -> unit

  val digest : t -> Md5.t

  val set_primary_input_size : t -> int -> unit

  val set_auxiliary_input_size : t -> int -> unit

  val get_primary_input_size : t -> int

  val get_auxiliary_input_size : t -> int

  val to_json :
       t
    -> ([> `String of string | `Assoc of (string * 'a) list | `List of 'a list]
        as
        'a)
end

module type Libsnark_constraint_system_intf = sig
  module Field : sig
    type t

    module Vector : sig
      type t
    end
  end

  module Var : sig
    type t
  end

  module Linear_combination : sig
    type t

    val create : unit -> t

    val of_var : Var.t -> t

    val of_field : Field.t -> t

    val add_term : t -> Field.t -> Var.t -> unit

    module Term : sig
      type t

      val create : Field.t -> Var.t -> t

      val coeff : t -> Field.t

      val var : t -> Var.t

      module Vector : Snarky_intf.Vector.S with type elt = t
    end

    val terms : t -> Term.Vector.t
  end

  module R1CS_constraint : sig
    type t

    val create :
      Linear_combination.t -> Linear_combination.t -> Linear_combination.t -> t

    val set_is_square : t -> bool -> unit

    val a : t -> Linear_combination.t

    val b : t -> Linear_combination.t

    val c : t -> Linear_combination.t
  end

  module R1CS_constraint_system : sig
    type t

    val create : unit -> t

    val finalize : t -> unit

    val add_constraint : t -> R1CS_constraint.t -> unit

    val set_primary_input_size : t -> int -> unit

    val set_auxiliary_input_size : t -> int -> unit

    val get_primary_input_size : t -> int

    val get_auxiliary_input_size : t -> int

    val report_statistics : t -> unit

    val add_constraint_with_annotation :
      t -> R1CS_constraint.t -> string -> unit

    val check_exn : t -> unit

    val is_satisfied :
         t
      -> primary_input:Field.Vector.t
      -> auxiliary_input:Field.Vector.t
      -> bool

    val digest : t -> Md5.t

    val iter_constraints : f:(R1CS_constraint.t -> unit) -> t -> unit

    val fold_constraints :
      f:('a -> R1CS_constraint.t -> 'a) -> init:'a -> t -> 'a
  end
end

module type S = sig
  module Field : Snarky_intf.Field.S

  module Bigint : sig
    module R : Snarky_intf.Bigint_intf.Extended with type field := Field.t
  end

  val field_size : Bigint.R.t

  module Var : sig
    type t

    val index : t -> int

    val create : int -> t
  end

  module R1CS_constraint_system :
    Constraint_system_intf with module Field := Field

  module Proving_key : sig
    type t [@@deriving bin_io]

    val is_initialized : t -> [`Yes | `No of R1CS_constraint_system.t]

    val set_constraint_system : t -> R1CS_constraint_system.t -> unit

    val to_string : t -> string

    val of_string : string -> t
  end

  module Verification_key : sig
    type t

    include Stringable.S with type t := t
  end

  module Proof : sig
    type message

    type t

    include Binable.S with type t := t

    val create :
         ?message:message
      -> Proving_key.t
      -> primary:Field.Vector.t
      -> auxiliary:Field.Vector.t
      -> t

    val verify :
      ?message:message -> t -> Verification_key.t -> Field.Vector.t -> bool
  end

  module Keypair : sig
    type t

    val pk : t -> Proving_key.t

    val vk : t -> Verification_key.t

    val create : R1CS_constraint_system.t -> t
  end
end
