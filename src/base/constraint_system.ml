open Core_kernel

module type S = sig
  module Field : sig
    type t
  end

  type t

  type constraint_

  val create : unit -> t

  val finalize : t -> unit

  val add_constraint : t -> constraint_ -> unit

  val digest : t -> Md5.t

  val set_primary_input_size : t -> int -> unit

  val set_auxiliary_input_size : t -> int -> unit

  val get_public_input_size : t -> int Core_kernel.Set_once.t

  val get_rows_len : t -> int
end

type ('f, 'constraint_) t =
  | T :
      (module S
         with type Field.t = 'f
          and type t = 't
          and type constraint_ = 'constraint_ )
      * 't
      -> ('f, 'constraint_) t
