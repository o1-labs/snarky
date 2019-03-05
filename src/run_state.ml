type ('prover_state, 'system, 'vector) t =
  { system: 'system option
  ; input: 'vector
  ; aux: 'vector
  ; eval_constraints: bool
  ; num_inputs: int
  ; next_auxiliary: int ref
  ; prover_state: 'prover_state option
  ; stack: string list
  ; handler: Request.Handler.t }
