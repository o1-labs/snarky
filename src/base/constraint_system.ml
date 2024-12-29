open Core_kernel

module type S = sig
  module Field : sig
    type t
  end

  type t

  val create : unit -> t

  val finalize : t -> unit

  val add_constraint : t -> (Field.t Cvar.t, Field.t) Constraint.basic -> unit

  val digest : t -> Md5.t

  val set_primary_input_size : t -> int -> unit

  val set_auxiliary_input_size : t -> int -> unit

  val get_public_input_size : t -> int Core_kernel.Set_once.t

  val get_rows_len : t -> int
end

type 'f t = T : (module S with type Field.t = 'f and type t = 't) * 't -> 'f t
