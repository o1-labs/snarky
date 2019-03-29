module rec Typ : sig
  open Typ_monads

  (** The type [('var, 'value, 'field, 'runner_state) t] describes a mapping from OCaml types
      to the variables and constraints they represent:
      - ['value] is the OCaml type
      - ['field] is the type of the field elements
      - ['var] is some other type that contains some R1CS variables
      - ['runner_state] is the internal type of the checked computation evaluator.

      For convenience and readability, it is usually best to have the ['var]
      type mirror the ['value] type in structure, for example:
{[
  type t = {b1 : bool; b2 : bool} (* 'value *)

  let or (x : t) = x.b1 || x.b2

  module Checked = struct
    type t = {b1 : Snark.Boolean.var; b2 : Snark.Boolean.var} (* 'var *)

    let or (x : t) = Snark.Boolean.(x.b1 || x.b2)
  end
]}*)
  type ('var, 'value, 'field) t =
    { store: 'value -> ('var, 'field) Store.t
    ; read: 'var -> ('value, 'field) Read.t
    ; alloc: ('var, 'field) Alloc.t
    ; check: 'var -> (unit, unit, 'field) Checked.t }
end =
  Typ

and Checked : sig
  (* TODO-someday: Consider having an "Assembly" type with only a store constructor for straight up Var.t's
    that this gets compiled into. *)

  (** The type [('ret, 'state, 'field, 'runner_state) t] represents a checked computation,
      where
      - ['state] is the type that holds the state used by [As_prover]
        computations
      - ['state -> 'ret] is the type of the computation
      - ['field] is the type of the field elements. *)
  type ('a, 's, 'f) t =
    | Pure : 'a -> ('a, 's, 'f) t
    | Direct :
        (('s, 'f) Run_state.t -> ('s, 'f) Run_state.t * 'a)
        * ('a -> ('b, 's, 'f) t)
        -> ('b, 's, 'f) t
    | Reduced :
        ('a, 's, 'f) t
        * (('s, 'f) Run_state.t -> ('s, 'f) Run_state.t)
        * ('b, 's, 'f) t
        -> ('b, 's, 'f) t
    | Add_constraint :
        'f Cvar.t Constraint.t * ('a, 's, 'f) t
        -> ('a, 's, 'f) t
    | As_prover :
        (unit, 'f, 's) As_prover0.t * ('a, 's, 'f) t
        -> ('a, 's, 'f) t
    | With_label :
        string * ('a, 's, 'f) t * ('a -> ('b, 's, 'f) t)
        -> ('b, 's, 'f) t
    | With_state :
        ('s1, 'f, 's) As_prover0.t
        * ('s1 -> (unit, 'f, 's) As_prover0.t)
        * ('b, 's1, 'f) t
        * ('b -> ('a, 's, 'f) t)
        -> ('a, 's, 'f) t
    | With_handler :
        Request.Handler.single * ('a, 's, 'f) t * ('a -> ('b, 's, 'f) t)
        -> ('b, 's, 'f) t
    | Clear_handler : ('a, 's, 'f) t * ('a -> ('b, 's, 'f) t) -> ('b, 's, 'f) t
    | Exists :
        ('var, 'value, 'f) Typ.t
        * ('value, 'f, 's) Provider.t
        * (('var, 'value) Handle.t -> ('a, 's, 'f) t)
        -> ('a, 's, 'f) t
    | Next_auxiliary : (int -> ('a, 's, 'f) t) -> ('a, 's, 'f) t
end =
  Checked

and Run_state : sig
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
    ; as_prover: bool ref
    ; run_special: 'a 's. (('a, 's, 'field) Checked.t -> 'a) option }
end =
  Run_state
