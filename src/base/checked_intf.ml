module type Basic = sig
  module Types : Types.Types

  type ('a, 'run_state) t = ('a, 'run_state) Types.Checked.t

  type 'f field

  type 'f field_var

  type run_state

  include Monad_let.S2 with type ('a, 'run_state) t := ('a, 'run_state) t

  val add_constraint :
    ('f field_var, 'f field) Constraint.t -> (unit, 'run_state) t

  val as_prover :
    (unit, 'f field, 'f field_var) As_prover0.t -> (unit, 'run_state) t

  val mk_lazy : (unit -> ('a, 'run_state) t) -> ('a Lazy.t, 'run_state) t

  val with_label : string -> (unit -> ('a, 'run_state) t) -> ('a, 'run_state) t

  val with_handler :
    Request.Handler.single -> (unit -> ('a, 'run_state) t) -> ('a, 'run_state) t

  val exists :
       ('var, 'value, 'f field, 'f field_var, 'run_state) Types.Typ.t
    -> ('value, 'f field, 'f field_var) Types.Provider.t
    -> (('var, 'value) Handle.t, 'run_state) t

  val next_auxiliary : unit -> (int, 'run_state) t

  val direct : (run_state -> run_state * 'a) -> ('a, 'run_state) t

  val constraint_count :
       ?weight:(('f field_var, 'f field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, 'run_state) t)
    -> int
end

module type S = sig
  module Types : Types.Types

  type ('a, 'run_state) t = ('a, 'run_state) Types.Checked.t

  type 'f field

  type 'f field_var

  type run_state

  include Monad_let.S2 with type ('a, 'run_state) t := ('a, 'run_state) t

  val as_prover :
    (unit, 'f field, 'f field_var) As_prover0.t -> (unit, 'run_state) t

  val mk_lazy : (unit -> ('a, 'run_state) t) -> ('a Lazy.t, 'run_state) t

  val request_witness :
       ('var, 'value, 'f field, 'f field_var, 'run_state) Types.Typ.t
    -> ('value Request.t, 'f field, 'f field_var) As_prover0.t
    -> ('var, 'run_state) t

  val request :
       ?such_that:('var -> (unit, 'run_state) t)
    -> ('var, 'value, 'f field, 'f field_var, 'run_state) Types.Typ.t
    -> 'value Request.t
    -> ('var, 'run_state) t

  val exists_handle :
       ?request:('value Request.t, 'f field, 'f field_var) As_prover0.t
    -> ?compute:('value, 'f field, 'f field_var) As_prover0.t
    -> ('var, 'value, 'f field, 'f field_var, 'run_state) Types.Typ.t
    -> (('var, 'value) Handle.t, 'run_state) t

  val exists :
       ?request:('value Request.t, 'f field, 'f field_var) As_prover0.t
    -> ?compute:('value, 'f field, 'f field_var) As_prover0.t
    -> ('var, 'value, 'f field, 'f field_var, 'run_state) Types.Typ.t
    -> ('var, 'run_state) t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request : 'a Request.t; respond : 'a Request.Response.t -> response }
        -> request

  val handle :
    (unit -> ('a, 'run_state) t) -> (request -> response) -> ('a, 'run_state) t

  val handle_as_prover :
       (unit -> ('a, 'run_state) t)
    -> (request -> response, 'f field, 'f field_var) As_prover0.t
    -> ('a, 'run_state) t

  val next_auxiliary : unit -> (int, 'run_state) t

  val with_label : string -> (unit -> ('a, 'run_state) t) -> ('a, 'run_state) t

  val assert_ :
       ?label:Base.string
    -> ('f field_var, 'f field) Constraint.t
    -> (unit, 'run_state) t

  val assert_r1cs :
       ?label:Base.string
    -> 'f field_var
    -> 'f field_var
    -> 'f field_var
    -> (unit, 'run_state) t

  val assert_square :
    ?label:Base.string -> 'f field_var -> 'f field_var -> (unit, 'run_state) t

  val assert_all :
       ?label:Base.string
    -> ('f field_var, 'f field) Constraint.t list
    -> (unit, 'run_state) t

  val assert_equal :
    ?label:Base.string -> 'f field_var -> 'f field_var -> (unit, 'run_state) t

  val direct : (run_state -> run_state * 'a) -> ('a, 'run_state) t

  val constraint_count :
       ?weight:(('f field_var, 'f field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, 'run_state) t)
    -> int
end

module type Extended = sig
  type field

  type field_var

  type run_state

  module Types : Types.Types

  type 'a t = ('a, run_state) Types.Checked.t

  include
    S
      with module Types := Types
      with type 'f field := field
       and type 'f field_var := field_var
       and type run_state := run_state
       and type ('a, 'run_state) t := ('a, 'run_state) Types.Checked.t

  val run : 'a t -> run_state -> run_state * 'a
end

module Unextend (Checked : Extended) :
  S
    with module Types = Checked.Types
    with type 'f field = Checked.field
     and type 'f field_var = Checked.field_var
     and type run_state = Checked.run_state = struct
  include (
    Checked :
      S
        with module Types = Checked.Types
        with type 'f field := Checked.field
         and type 'f field_var := Checked.field_var
         and type run_state := Checked.run_state
         and type ('a, 'run_state) t := ('a, 'run_state) Checked.Types.Checked.t )

  type 'f field = Checked.field

  type 'f field_var = Checked.field_var

  type run_state = Checked.run_state

  type ('a, 'run_state) t = ('a, 'run_state) Types.Checked.t
end
