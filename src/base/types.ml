module As_prover = struct
  type ('a, 'f) t = ('f Cvar.t -> 'f) -> 'a
end

module Provider = struct
  module T = struct
    (** The different ways to generate a value for the circuit witness.

        If [Both], this attempts the request first, and falls back on compute
        if the request is unhandled or raises an exception.
    *)
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

module type Types = sig
  module Checked : sig
    type ('a, 'f) t
  end

  module As_prover : sig
    type ('a, 'f) t
  end

  module Typ : sig
    include module type of Typ.T

    type ('var, 'value, 'f) t = ('var, 'value, 'f, (unit, 'f) Checked.t) Typ.t
  end

  module Provider : sig
    include module type of Provider.T

    type ('a, 'f) t =
      (('a Request.t, 'f) As_prover.t, ('a, 'f) As_prover.t) provider
  end
end
