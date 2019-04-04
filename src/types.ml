module Typ = struct
  open Typ_monads

  module T = struct
    (** The type [('var, 'value, 'field, 'checked) t] describes a mapping from
      OCaml types to the variables and constraints they represent:
      - ['value] is the OCaml type
      - ['field] is the type of the field elements
      - ['var] is some other type that contains some R1CS variables
      - ['checked] is the type of checked computation that verifies the stored
        contents as R1CS variables.

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
    type ('var, 'value, 'field, 'checked) typ =
      { store: 'value -> ('var, 'field) Store.t
      ; read: 'var -> ('value, 'field) Read.t
      ; alloc: ('var, 'field) Alloc.t
      ; check: 'var -> 'checked }
  end

  include T

  type ('var, 'value, 'field, 'checked) t =
    ('var, 'value, 'field, 'checked) typ
end

module Data_spec = struct
  module T = struct
    (** A list of {!type:Type.Typ.t} values, describing the inputs to a checked
        computation. The type [('r_var, 'r_value, 'k_var, 'k_value, 'field) t]
        represents
        - ['k_value] is the OCaml type of the computation
        - ['r_value] is the OCaml type of the result
        - ['k_var] is the type of the computation within the R1CS
        - ['k_value] is the type of the result within the R1CS
        - ['field] is the field over which the R1CS operates
        - ['checked] is the type of checked computation that verifies the stored
          contents as R1CS variables.

        This functions the same as OCaml's default list type:
{[
  Data_spec.[typ1; typ2; typ3]

  Data_spec.(typ1 :: typs)

  let open Data_spec in
  [typ1; typ2; typ3; typ4; typ5]

  let open Data_spec in
  typ1 :: typ2 :: typs

]}
        all function as you would expect.
    *)
    type ('r_var, 'r_value, 'k_var, 'k_value, 'f, 'checked) data_spec =
      | ( :: ) :
          ('var, 'value, 'f, 'checked) Typ.t
          * ('r_var, 'r_value, 'k_var, 'k_value, 'f, 'checked) data_spec
          -> ( 'r_var
             , 'r_value
             , 'var -> 'k_var
             , 'value -> 'k_value
             , 'f
             , 'checked )
             data_spec
      | [] : ('r_var, 'r_value, 'r_var, 'r_value, 'f, 'checked) data_spec
  end

  include T

  type ('r_var, 'r_value, 'k_var, 'k_value, 'f, 'checked) t =
    ('r_var, 'r_value, 'k_var, 'k_value, 'f, 'checked) data_spec
end

module Provider = struct
  type ('a, 'f, 's) t =
    (('a Request.t, 'f, 's) As_prover0.t, ('a, 'f, 's) As_prover0.t) Provider.t

  module T = Provider.T
  include T
end

module Checked = struct
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
        * 'a
        * ('a -> ('b, 's, 'f) t)
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
        ('var, 'value, 'f, (unit, unit, 'f) t) Typ.t
        * ('value, 'f, 's) Provider.t
        * (('var, 'value) Handle.t -> ('a, 's, 'f) t)
        -> ('a, 's, 'f) t
    | Next_auxiliary : (int -> ('a, 's, 'f) t) -> ('a, 's, 'f) t
end
