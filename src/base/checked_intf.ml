module type Basic = sig
  module Types : Types.Types

  type ('a, 'f, 'field_var) t = ('a, 'f, 'field_var) Types.Checked.t

  type 'f field

  type 'f field_var

  include
    Monad_let.S3 with type ('a, 'f, 'field_var) t := ('a, 'f, 'field_var) t

  val add_constraint :
    ('f field_var, 'f field) Constraint.t -> (unit, 'f field, 'f field_var) t

  val as_prover :
       (unit, 'f field, 'f field_var) As_prover0.t
    -> (unit, 'f field, 'f field_var) t

  val mk_lazy :
    (unit -> ('a, 'f, 'field_var) t) -> ('a Lazy.t, 'f, 'field_var) t

  val with_label :
       string
    -> (unit -> ('a, 'f field, 'f field_var) t)
    -> ('a, 'f field, 'f field_var) t

  val with_handler :
       Request.Handler.single
    -> (unit -> ('a, 'f field, 'f field_var) t)
    -> ('a, 'f field, 'f field_var) t

  val exists :
       ('var, 'value, 'f field, 'f field_var) Types.Typ.t
    -> ('value, 'f field, 'f field_var) Types.Provider.t
    -> (('var, 'value) Handle.t, 'f field, 'f field_var) t

  val next_auxiliary : unit -> (int, 'f field, 'f field_var) t

  val direct :
       (   ('f field, 'f field_var) Run_state.t
        -> ('f field, 'f field_var) Run_state.t * 'a )
    -> ('a, 'f field, 'f field_var) t

  val constraint_count :
       ?weight:(('f field_var, 'f field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, 'f field, 'f field_var) t)
    -> int
end

module type S = sig
  module Types : Types.Types

  type ('a, 'f, 'field_var) t = ('a, 'f, 'field_var) Types.Checked.t

  type 'f field

  type 'f field_var

  include
    Monad_let.S3 with type ('a, 'f, 'field_var) t := ('a, 'f, 'field_var) t

  val as_prover :
       (unit, 'f field, 'f field_var) As_prover0.t
    -> (unit, 'f field, 'f field_var) t

  val mk_lazy :
    (unit -> ('a, 'f, 'field_var) t) -> ('a Lazy.t, 'f, 'field_var) t

  val request_witness :
       ('var, 'value, 'f field, 'f field_var) Types.Typ.t
    -> ('value Request.t, 'f field, 'f field_var) As_prover0.t
    -> ('var, 'f field, 'f field_var) t

  val request :
       ?such_that:('var -> (unit, 'f field, 'f field_var) t)
    -> ('var, 'value, 'f field, 'f field_var) Types.Typ.t
    -> 'value Request.t
    -> ('var, 'f field, 'f field_var) t

  val exists_handle :
       ?request:('value Request.t, 'f field, 'f field_var) As_prover0.t
    -> ?compute:('value, 'f field, 'f field_var) As_prover0.t
    -> ('var, 'value, 'f field, 'f field_var) Types.Typ.t
    -> (('var, 'value) Handle.t, 'f field, 'f field_var) t

  val exists :
       ?request:('value Request.t, 'f field, 'f field_var) As_prover0.t
    -> ?compute:('value, 'f field, 'f field_var) As_prover0.t
    -> ('var, 'value, 'f field, 'f field_var) Types.Typ.t
    -> ('var, 'f field, 'f field_var) t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request : 'a Request.t; respond : 'a Request.Response.t -> response }
        -> request

  val handle :
       (unit -> ('a, 'f field, 'f field_var) t)
    -> (request -> response)
    -> ('a, 'f field, 'f field_var) t

  val handle_as_prover :
       (unit -> ('a, 'f field, 'f field_var) t)
    -> (request -> response, 'f field, 'f field_var) As_prover0.t
    -> ('a, 'f field, 'f field_var) t

  val next_auxiliary : unit -> (int, 'f field, 'f field_var) t

  val with_label :
       string
    -> (unit -> ('a, 'f field, 'f field_var) t)
    -> ('a, 'f field, 'f field_var) t

  val assert_ :
       ?label:Base.string
    -> ('f field_var, 'f field) Constraint.t
    -> (unit, 'f field, 'f field_var) t

  val assert_r1cs :
       ?label:Base.string
    -> 'f field_var
    -> 'f field_var
    -> 'f field_var
    -> (unit, 'f field, 'f field_var) t

  val assert_square :
       ?label:Base.string
    -> 'f field_var
    -> 'f field_var
    -> (unit, 'f field, 'f field_var) t

  val assert_all :
       ?label:Base.string
    -> ('f field_var, 'f field) Constraint.t list
    -> (unit, 'f field, 'f field_var) t

  val assert_equal :
       ?label:Base.string
    -> 'f field_var
    -> 'f field_var
    -> (unit, 'f field, 'f field_var) t

  val direct :
       (   ('f field, 'f field_var) Run_state.t
        -> ('f field, 'f field_var) Run_state.t * 'a )
    -> ('a, 'f field, 'f field_var) t

  val constraint_count :
       ?weight:(('f field_var, 'f field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, 'f field, 'f field_var) t)
    -> int
end

module type Extended = sig
  type field

  type field_var

  module Types : Types.Types

  type 'a t = ('a, field, field_var) Types.Checked.t

  include
    S
      with module Types := Types
      with type 'f field := field
       and type 'f field_var := field_var
       and type ('a, 'f, 'field_var) t := ('a, 'f, 'field_var) Types.Checked.t

  val run :
       'a t
    -> (field, field_var) Run_state.t
    -> (field, field_var) Run_state.t * 'a
end

module Unextend (Checked : Extended) :
  S
    with module Types = Checked.Types
    with type 'f field = Checked.field
     and type 'f field_var = Checked.field_var = struct
  include (
    Checked :
      S
        with module Types = Checked.Types
        with type 'f field := Checked.field
         and type 'f field_var := Checked.field_var
         and type ('a, 'f, 'v) t := ('a, 'f, 'v) Checked.Types.Checked.t )

  type 'f field = Checked.field

  type 'f field_var = Checked.field_var

  type ('a, 'f, 'v) t = ('a, 'f, 'v) Types.Checked.t
end
