module Vector : sig
  type 'elt t =
    | T :
        (module Snarky_intf.Vector.S with type elt = 'elt and type t = 't)
        * 't Base.Type_equal.Id.t
        * 't
        -> 'elt t
end

module type S = sig
  module Vector : sig
    type 'elt t = 'elt Vector.t =
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

  type field

  type t

  type constraint_

  val make :
       num_inputs:int
    -> input:field Vector.t
    -> next_auxiliary:int ref
    -> aux:field Vector.t
    -> ?system:(field, constraint_) Constraint_system.t
    -> eval_constraints:bool
    -> ?log_constraint:
         (   ?at_label_boundary:[ `End | `Start ] * string
          -> constraint_ option
          -> unit )
    -> ?handler:Request.Handler.t
    -> with_witness:bool
    -> ?stack:string list
    -> ?is_running:bool
    -> unit
    -> t

  (** dumps some information about a state [t] *)
  val dump : t -> string

  val get_variable_value : t -> int -> field

  val store_field_elt : t -> field -> field Cvar.t

  val alloc_var : t -> unit -> field Cvar.t

  val id : t -> int

  val has_witness : t -> bool

  val as_prover : t -> bool

  val set_as_prover : t -> bool -> unit

  val stack : t -> string list

  val set_stack : t -> string list -> t

  val log_constraint :
       t
    -> (   ?at_label_boundary:[ `Start | `End ] * string
        -> constraint_ option
        -> unit )
       option

  val eval_constraints : t -> bool

  val system : t -> (field, constraint_) Constraint_system.t option

  val handler : t -> Request.Handler.t

  val set_handler : t -> Request.Handler.t -> t

  val is_running : t -> bool

  val set_is_running : t -> bool -> t

  val next_auxiliary : t -> int
end
