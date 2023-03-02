open Core_kernel

module type S = sig
  include Backend_intf.Run_state_intf

  val make :
       num_inputs:int
    -> input:Field.Vector.t
    -> next_auxiliary:int ref
    -> aux:Field.Vector.t
    -> ?system:constraint_system
    -> eval_constraints:bool
    -> ?log_constraint:
         (   ?at_label_boundary:[ `End | `Start ] * string
          -> (cvar, Field.t) Constraint.t option
          -> unit )
    -> ?handler:Request.Handler.t
    -> with_witness:bool
    -> ?stack:string list
    -> ?is_running:bool
    -> unit
    -> t

  val add_constraint :
    ?label:string -> t -> (cvar, Field.t) Constraint.basic -> unit

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

  val handler : t -> Request.Handler.t

  val set_handler : t -> Request.Handler.t -> t

  val is_running : t -> bool

  val set_is_running : t -> bool -> t

  val next_auxiliary : t -> int
end

module Make
    (Cvar : T) (Field : sig
      type t

      module Vector : T
    end)
    (CS : T)
    (Run_state : Backend_intf.Run_state_intf
                   with module Field := Field
                    and type cvar := Cvar.t
                    and type constraint_system := CS.t) :
  S
    with module Field := Field
     and type cvar := Cvar.t
     and type constraint_system := CS.t = struct
  include Run_state

  (* We wrap the state coming from Rust, and we add some values that are only relevant for the OCaml implementation. *)

  type t =
    { state : Run_state.t
    ; stack : string list
    ; handler : Request.Handler.t
    ; is_running : bool
    ; log_constraint :
        (   ?at_label_boundary:[ `Start | `End ] * string
         -> (Cvar.t, Field.t) Constraint.t option
         -> unit )
        option
    }

  (* We redefine a number of functions but on the wrapper. *)

  let get_variable_value t idx = get_variable_value t.state idx

  let store_field_elt t field = store_field_elt t.state field

  let alloc_var t _ = alloc_var t.state ()

  let has_witness t = has_witness t.state

  let as_prover t = as_prover t.state

  let set_as_prover t b = set_as_prover t.state b

  let eval_constraints t = eval_constraints t.state

  let next_auxiliary t = next_auxiliary t.state

  let add_constraint ?label t (cstr : (Cvar.t, Field.t) Constraint.basic) =
    add_constraint t.state label cstr

  (* We redefine the [make] function with the wrapper in mind. *)

  let make :
         num_inputs:int
      -> input:Field.Vector.t
      -> next_auxiliary:int ref
      -> aux:Field.Vector.t
      -> ?system:CS.t
      -> eval_constraints:bool
      -> ?log_constraint:
           (   ?at_label_boundary:[ `End | `Start ] * string
            -> (Cvar.t, Field.t) Constraint.t option
            -> unit )
      -> ?handler:Request.Handler.t
      -> with_witness:bool
      -> ?stack:string list
      -> ?is_running:bool
      -> unit
      -> t =
   fun ~num_inputs ~input ~next_auxiliary ~aux ?system ~eval_constraints
       ?log_constraint ?handler ~with_witness ?(stack = []) ?(is_running = true)
       () ->
    next_auxiliary := 1 + num_inputs ;
    (* We can't evaluate the constraints if we are not computing over a value. *)
    let eval_constraints = eval_constraints && with_witness in
    let as_prover = false in
    let state =
      Run_state.make ~num_inputs ~input ~next_auxiliary ~aux ~system
        ~eval_constraints ~with_witness ~as_prover ()
    in
    { state
    ; stack
    ; handler = Option.value handler ~default:Request.Handler.fail
    ; is_running
    ; log_constraint
    }

  (* We define a number of functions that are only relevant for the extra fields we added in the wrapper. *)

  let stack { stack; _ } = stack

  let set_stack t stack = { t with stack }

  let log_constraint { log_constraint; _ } = log_constraint

  let handler { handler; _ } = handler

  let set_handler t handler = { t with handler }

  let is_running { is_running; _ } = is_running

  let set_is_running t is_running = { t with is_running }
end
