module As_prover = struct
  type ('a, 'f) t = ('f Cvar.t -> 'f) -> 'a
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
      { store : 'value -> ('var, 'field) Store.t
      ; read : 'var -> ('value, 'field) Read.t
      ; alloc : ('var, 'field) Alloc.t
      ; check : 'var -> 'checked
      }
  end

  include T

  type ('var, 'value, 'field, 'checked) t = ('var, 'value, 'field, 'checked) typ
end

module Checked = struct
  (* TODO-someday: Consider having an "Assembly" type with only a store constructor for straight up Var.t's
     that this gets compiled into. *)

  (** The type [('ret, 'field, 'runner_state) t] represents a checked computation,
      where
      - ['ret] is the return type of the computation
      - ['field] is the type of the field elements. *)
  type ('a, 'f) t =
    | Pure : 'a -> ('a, 'f) t
    | Direct :
        ('f Run_state.t -> 'f Run_state.t * 'a) * ('a -> ('b, 'f) t)
        -> ('b, 'f) t
    | Reduced :
        ('a, 'f) t
        * ('f Run_state.t -> 'f Run_state.t)
        * 'a
        * ('a -> ('b, 'f) t)
        -> ('b, 'f) t
    | Add_constraint : ('f Cvar.t, 'f) Constraint.t * ('a, 'f) t -> ('a, 'f) t
    | As_prover : (unit, 'f) As_prover.t * ('a, 'f) t -> ('a, 'f) t
    | Lazy : ('a, 'f) t * ('a Lazy.t -> ('b, 'f) t) -> ('b, 'f) t
    | With_label : string * ('a, 'f) t * ('a -> ('b, 'f) t) -> ('b, 'f) t
    | With_handler :
        Request.Handler.single * ('a, 'f) t * ('a -> ('b, 'f) t)
        -> ('b, 'f) t
    | Clear_handler : ('a, 'f) t * ('a -> ('b, 'f) t) -> ('b, 'f) t
    | Exists :
        ('var, 'value, 'f, (unit, 'f) t) Typ.t
        * ( ('value Request.t, 'f) As_prover.t
          , ('value, 'f) As_prover.t )
          Provider.t
        * (('var, 'value) Handle.t -> ('a, 'f) t)
        -> ('a, 'f) t
    | Next_auxiliary : (int -> ('a, 'f) t) -> ('a, 'f) t
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
