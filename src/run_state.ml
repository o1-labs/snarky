(** The internal state used to run a checked computation. *)
type ('prover_state, 'field) t =
  { system: 'field Backend_types.R1CS_constraint_system.t option
  ; input: 'field Vector.t
  ; aux: 'field Vector.t
  ; eval_constraints: bool
  ; num_inputs: int
  ; next_auxiliary: int ref
  ; prover_state: 'prover_state option
  ; stack: string list
  ; handler: Request.Handler.t
  ; as_prover: bool ref
  ; log_constraint: ('field Cvar.t Constraint.t -> unit) option }

let set_prover_state prover_state
    { system
    ; input
    ; aux
    ; eval_constraints
    ; num_inputs
    ; next_auxiliary
    ; prover_state= _
    ; stack
    ; handler
    ; as_prover
    ; log_constraint } =
  { system
  ; input
  ; aux
  ; eval_constraints
  ; num_inputs
  ; next_auxiliary
  ; prover_state
  ; stack
  ; handler
  ; as_prover
  ; log_constraint }
