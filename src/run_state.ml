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
  ; run_special:
      'a 's.
      (   ('a, 's, 'field, (unit, 'system, 'field, 'vector) t) Types.Checked.t
       -> 'a) option }
