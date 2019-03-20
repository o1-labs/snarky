type ('prover_state, 'system, 'field, 'vector) t =
  { system: 'system option
  ; input: 'vector
  ; aux: 'vector
  ; eval_constraints: bool
  ; num_inputs: int
  ; next_auxiliary: int ref
  ; prover_state: 'prover_state option
  ; stack: string list
  ; handler: Request.Handler.t
  ; is_running: bool
  ; as_prover: bool ref
  ; run_special:
      'a 's 's1.
      (   ('a, 's, 'field, ('s1, 'system, 'field, 'vector) t) Types.Checked.t
       -> 'a) option }
