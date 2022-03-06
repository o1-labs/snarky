module As_prover = struct
  type ('a, 'f, 's) t = ('f Cvar.t -> 'f) -> 's -> 's * 'a
end

module Provider = struct
  module T = struct
    type ('request, 'compute) provider =
      | Request of 'request
      | Compute of 'compute
      | Both of 'request * 'compute
  end

  include T

  type ('request, 'compute) t = ('request, 'compute) provider
end

module Typ = struct
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
    type ('var, 'value, 'aux, 'field, 'checked) typ' =
      { var_to_fields : 'var -> 'field Cvar.t array * 'aux
      ; var_of_fields : 'field Cvar.t array * 'aux -> 'var
      ; value_to_fields : 'value -> 'field array * 'aux
      ; value_of_fields : 'field array * 'aux -> 'value
      ; size_in_field_elements : int
      ; constraint_system_auxiliary : unit -> 'aux
      ; check : 'var -> 'checked
      }

    type ('var, 'value, 'field, 'checked) typ =
      | Typ :
          ('var, 'value, 'aux, 'field, 'checked) typ'
          -> ('var, 'value, 'field, 'checked) typ
  end

  include T

  type ('var, 'value, 'field, 'checked) t = ('var, 'value, 'field, 'checked) typ
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
        ('f Cvar.t, 'f) Constraint.t * ('a, 's, 'f) t
        -> ('a, 's, 'f) t
    | As_prover : (unit, 'f, 's) As_prover.t * ('a, 's, 'f) t -> ('a, 's, 'f) t
    | Lazy : ('a, unit, 'f) t * ('a Lazy.t -> ('b, 's, 'f) t) -> ('b, 's, 'f) t
    | With_label :
        string * ('a, 's, 'f) t * ('a -> ('b, 's, 'f) t)
        -> ('b, 's, 'f) t
    | With_state :
        ('s1, 'f, 's) As_prover.t
        * ('s1 -> (unit, 'f, 's) As_prover.t)
        * ('b, 's1, 'f) t
        * ('b -> ('a, 's, 'f) t)
        -> ('a, 's, 'f) t
    | With_handler :
        Request.Handler.single * ('a, 's, 'f) t * ('a -> ('b, 's, 'f) t)
        -> ('b, 's, 'f) t
    | Clear_handler : ('a, 's, 'f) t * ('a -> ('b, 's, 'f) t) -> ('b, 's, 'f) t
    | Exists :
        ('var, 'value, 'f, (unit, unit, 'f) t) Typ.t
        * ( ('value Request.t, 'f, 's) As_prover.t
          , ('value, 'f, 's) As_prover.t )
          Provider.t
        * (('var, 'value) Handle.t -> ('a, 's, 'f) t)
        -> ('a, 's, 'f) t
    | Next_auxiliary : (int -> ('a, 's, 'f) t) -> ('a, 's, 'f) t
end

module type Types = sig
  module Checked : sig
    type ('a, 's, 'f) t
  end

  module As_prover : sig
    type ('a, 'f, 's) t
  end

  module Typ : sig
    include module type of Typ.T

    type ('var, 'value, 'f) t =
      ('var, 'value, 'f, (unit, unit, 'f) Checked.t) Typ.t
  end

  module Provider : sig
    include module type of Provider.T

    type ('a, 'f, 's) t =
      (('a Request.t, 'f, 's) As_prover.t, ('a, 'f, 's) As_prover.t) provider
  end
end
