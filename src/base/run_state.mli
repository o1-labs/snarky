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

type ('field, 'field_var) t

val make :
     num_inputs:int
  -> input:'field Vector.t
  -> next_auxiliary:int ref
  -> aux:'field Vector.t
  -> ?system:('field, 'field_var) Constraint_system.t
  -> eval_constraints:bool
  -> ?log_constraint:
       (   ?at_label_boundary:[ `End | `Start ] * string
        -> ('field_var, 'field) Constraint.t option
        -> unit )
  -> ?handler:Request.Handler.t
  -> with_witness:bool
  -> ?stack:string list
  -> ?is_running:bool
  -> unit
  -> ('field, 'field_var) t

(** dumps some information about a state [t] *)
val dump : ('field, 'field_var) t -> string

val get_variable_value : ('field, 'field_var) t -> int -> 'field

val store_field_elt : ('field, 'field_var) t -> 'field -> 'field_var

val alloc_var : ('field, 'field_var) t -> unit -> 'field_var

val has_witness : _ t -> bool

val as_prover : _ t -> bool

val set_as_prover : _ t -> bool -> unit

val stack : _ t -> string list

val set_stack : ('field, 'field_var) t -> string list -> ('field, 'field_var) t

val log_constraint :
     ('field, 'field_var) t
  -> (   ?at_label_boundary:[ `Start | `End ] * string
      -> ('field_var, 'field) Constraint.t option
      -> unit )
     option

val eval_constraints : ('field, 'field_var) t -> bool

val system :
  ('field, 'field_var) t -> ('field, 'field_var) Constraint_system.t option

val handler : _ t -> Request.Handler.t

val set_handler :
  ('field, 'field_var) t -> Request.Handler.t -> ('field, 'field_var) t

val is_running : _ t -> bool

val set_is_running : ('field, 'field_var) t -> bool -> ('field, 'field_var) t

val next_auxiliary : _ t -> int
