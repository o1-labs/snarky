open Core_kernel

module type S = sig
  module Field : sig
    type t

    module Vector : T
  end

  type t

  type cvar

  type constraint_system

  val make :
       num_inputs:int
    -> system:bool
    -> eval_constraints:bool
    -> ?log_constraint:
         (   ?at_label_boundary:[ `End | `Start ] * string
          -> (cvar, Field.t) Constraint.t option
          -> unit )
    -> ?handler:Request.Handler.t
    -> ?stack:string list
    -> ?is_running:bool
    -> unit
    -> t

  val add_constraint :
    ?label:string -> t -> (cvar, Field.t) Constraint.basic -> unit

  val get_variable_value : t -> int -> Field.t

  val store_field_elt : t -> Field.t -> cvar

  val alloc_var : t -> cvar

  val has_witness : t -> bool

  val set_public_inputs : t -> Field.Vector.t -> unit

  val get_private_inputs : t -> Field.Vector.t

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

  val finalize : t -> unit

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

  let alloc_var t = alloc_var t.state

  let has_witness t = has_witness t.state

  (* TODO: make sure that the public input is not already set in there, and that this is the expected public input size *)
  let set_public_inputs t public_inputs =
    set_public_inputs t.state public_inputs

  (* TODO: we should make sure that the state was finalized before this can be called *)
  let get_private_inputs t = get_private_inputs t.state

  let as_prover t = as_prover t.state

  let set_as_prover t b = set_as_prover t.state b

  let eval_constraints t = eval_constraints t.state

  let system t = system t.state

  let next_auxiliary t = next_auxiliary t.state

  let add_constraint ?label t (cstr : (Cvar.t, Field.t) Constraint.basic) =
    add_constraint t.state ?label cstr

  let finalize t = finalize t.state

  (* We redefine the [make] function with the wrapper in mind. *)

  let make :
         num_inputs:int
      -> system:bool
      -> eval_constraints:bool
      -> ?log_constraint:
           (   ?at_label_boundary:[ `End | `Start ] * string
            -> (Cvar.t, Field.t) Constraint.t option
            -> unit )
      -> ?handler:Request.Handler.t
      -> ?stack:string list
      -> ?is_running:bool
      -> unit
      -> t =
   fun ~num_inputs ~system ~eval_constraints ?log_constraint ?handler
       ?(stack = []) ?(is_running = true) () ->
    (* create the inner Rust state *)
    let state : Run_state.t =
      if system then Run_state.make_system num_inputs eval_constraints
      else Run_state.make num_inputs eval_constraints
    in

    (* create the wrapper state *)
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
