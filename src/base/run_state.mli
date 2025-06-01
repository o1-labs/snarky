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

type 'field t

val make :
     num_inputs:int
  -> input:'field Vector.t
  -> next_auxiliary:int ref
  -> aux:'field Vector.t
  -> ?system:'field Constraint_system.t
  -> eval_constraints:bool
  -> ?log_constraint:
       (   ?at_label_boundary:[ `End | `Start ] * string
        -> ('field Cvar.t, 'field) Constraint.t option
        -> unit )
  -> ?handler:Request.Handler.t
  -> with_witness:bool
  -> ?stack:string list
  -> unit
  -> 'field t

(** dumps some information about a state [t] *)
val dump : 'field t -> string

val get_variable_value : 'field t -> int -> 'field

val store_field_elt : 'field t -> 'field -> 'field Cvar.t

val alloc_var : 'field t -> unit -> 'field Cvar.t

val id : _ t -> int

val has_witness : _ t -> bool

val as_prover : _ t -> bool

val set_as_prover : _ t -> bool -> unit

val stack : _ t -> string list

val set_stack : 'field t -> string list -> 'field t

val log_constraint :
     'field t
  -> (   ?at_label_boundary:[ `Start | `End ] * string
      -> ('field Cvar.t, 'field) Constraint.t option
      -> unit )
     option

val eval_constraints : 'field t -> bool

val system : 'field t -> 'field Constraint_system.t option

val handler : _ t -> Request.Handler.t

val set_handler : 'field t -> Request.Handler.t -> 'field t

val next_auxiliary : _ t -> int
