module rec Typ : sig
  open Typ_monads

  (** The type [('var, 'value, 'field, 'field_var, 'sys) t] describes a mapping
      from OCaml types to the variables and constraints they represent:
      - ['value] is the OCaml type
      - ['field] is the type of the field elements
      - ['field_var] is the type of variables within the R1CS
      - ['sys] is the type of the R1CS
      - ['var] is some other type that contains some ['field_var] values.

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
  type ('var, 'value, 'field, 'field_var, 'sys) t =
    { store: 'value -> ('var, 'field, 'field_var) Store.t
    ; read: 'var -> ('value, 'field, 'field_var) Read.t
    ; alloc: ('var, 'field_var) Alloc.t
    ; check: 'var -> (unit, unit, 'field, 'field_var, 'sys) Checked.t }
end =
  Typ

and Checked : sig
  (* TODO-someday: Consider having an "Assembly" type with only a store constructor for straight up Var.t's
    that this gets compiled into. *)

  (** The type [('ret, 'state, 'field, 'field_var', 'sys) t] represents a
      checked computation, where
      - ['state] is the type that holds the state used by [As_prover] computations
      - ['state -> 'ret] is the type of the computation
      - ['field] is the type of the field elements
      - ['sys] is the type of the R1CS
      - ['field_var] is the type of variables within the R1CS. *)
  type ('a, 's, 'f, 'v, 'sys) t =
    | Pure : 'a -> ('a, 's, 'f, 'v, 'sys) t
    | Add_constraint :
        'v Constraint.t * ('a, 's, 'f, 'v, 'sys) t
        -> ('a, 's, 'f, 'v, 'sys) t
    | With_constraint_system :
        ('sys -> unit) * ('a, 's, 'f, 'v, 'sys) t
        -> ('a, 's, 'f, 'v, 'sys) t
    | As_prover :
        (unit, 'v -> 'f, 's) As_prover0.t * ('a, 's, 'f, 'v, 'sys) t
        -> ('a, 's, 'f, 'v, 'sys) t
    | With_label :
        string * ('a, 's, 'f, 'v, 'sys) t * ('a -> ('b, 's, 'f, 'v, 'sys) t)
        -> ('b, 's, 'f, 'v, 'sys) t
    | With_state :
        ('s1, 'v -> 'f, 's) As_prover0.t
        * ('s1 -> (unit, 'v -> 'f, 's) As_prover0.t)
        * ('b, 's1, 'f, 'v, 'sys) t
        * ('b -> ('a, 's, 'f, 'v, 'sys) t)
        -> ('a, 's, 'f, 'v, 'sys) t
    | With_handler :
        Request.Handler.single
        * ('a, 's, 'f, 'v, 'sys) t
        * ('a -> ('b, 's, 'f, 'v, 'sys) t)
        -> ('b, 's, 'f, 'v, 'sys) t
    | Clear_handler :
        ('a, 's, 'f, 'v, 'sys) t * ('a -> ('b, 's, 'f, 'v, 'sys) t)
        -> ('b, 's, 'f, 'v, 'sys) t
    | Exists :
        ('var, 'value, 'f, 'v, 'sys) Typ.t
        * ('value, 'v -> 'f, 's) Provider.t
        * (('var, 'value) Handle.t -> ('a, 's, 'f, 'v, 'sys) t)
        -> ('a, 's, 'f, 'v, 'sys) t
    | Next_auxiliary :
        (int -> ('a, 's, 'f, 'v, 'sys) t)
        -> ('a, 's, 'f, 'v, 'sys) t
end =
  Checked
