module Checked = struct
  type ('a, 'f) t = 'f Run_state.t -> 'f Run_state.t * 'a
end

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

  type ('var, 'value, 'field) t =
    ('var, 'value, 'field, (unit, 'field) Checked.t) typ
end

(* TODO: can we delete this module type? *)
module type Types = sig
  module Checked : sig
    type ('a, 'f) t
  end

  module As_prover : sig
    type ('a, 'f) t
  end

  module Typ : sig
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

    type ('var, 'value, 'f) t = ('var, 'value, 'f, (unit, 'f) Checked.t) Typ.typ
  end

  module Provider : sig
    include module type of Provider.T

    type ('a, 'f) t =
      (('a Request.t, 'f) As_prover.t, ('a, 'f) As_prover.t) provider
  end
end
