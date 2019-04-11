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
  ; is_running: bool
  ; as_prover: bool ref }

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
    ; is_running
    ; as_prover } =
  { system
  ; input
  ; aux
  ; eval_constraints
  ; num_inputs
  ; next_auxiliary
  ; prover_state
  ; stack
  ; handler
  ; is_running
  ; as_prover }

let dummy_state () =
  { system= None
  ; input= Vector.null
  ; aux= Vector.null
  ; eval_constraints= false
  ; num_inputs= 0
  ; next_auxiliary= ref 1
  ; prover_state= None
  ; stack= []
  ; handler= Request.Handler.fail
  ; is_running= true
  ; as_prover= ref false }
