type ('prover_state, 'system, 'field) t =
  { system: 'system option
  ; input: 'field Vector.t
  ; aux: 'field Vector.t
  ; eval_constraints: bool
  ; num_inputs: int
  ; next_auxiliary: int ref
  ; prover_state: 'prover_state option
  ; stack: string list
  ; handler: Request.Handler.t
  ; is_running: bool
  ; as_prover: bool ref
  ; run_special:
      'a 's.
      (('a, 's, 'field, (unit, 'system, 'field) t) Types.Checked.t -> 'a)
      option }
