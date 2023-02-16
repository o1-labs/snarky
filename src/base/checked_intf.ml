module type Basic = sig
  module Types : Types.Types

  type ('a, 'f) t = ('a, 'f) Types.Checked.t

  type 'f field

  type cvar

  type run_state

  include Monad_let.S2 with type ('a, 'f) t := ('a, 'f) t

  val add_constraint : (cvar, 'f field) Constraint.t -> (unit, 'f field) t

  val as_prover : (unit, 'f field, cvar) As_prover0.t -> (unit, 'f field) t

  val mk_lazy : (unit -> ('a, 'f) t) -> ('a Lazy.t, 'f) t

  val with_label : string -> (unit -> ('a, 'f field) t) -> ('a, 'f field) t

  val with_handler :
    Request.Handler.single -> (unit -> ('a, 'f field) t) -> ('a, 'f field) t

  val exists :
       ('var, 'value, 'f field, cvar) Types.Typ.t
    -> ('value, 'f field, cvar) Types.Provider.t
    -> (('var, 'value) Handle.t, 'f field) t

  val next_auxiliary : unit -> (int, 'f field) t

  val direct : (run_state -> run_state * 'a) -> ('a, 'f field) t

  val constraint_count :
       ?weight:((cvar, 'f field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, 'f field) t)
    -> int
end

module type S = sig
  module Types : Types.Types

  type ('a, 'f) t = ('a, 'f) Types.Checked.t

  type 'f field

  type cvar

  type run_state

  include Monad_let.S2 with type ('a, 'f) t := ('a, 'f) t

  val as_prover : (unit, 'f field, cvar) As_prover0.t -> (unit, 'f field) t

  val mk_lazy : (unit -> ('a, 'f) t) -> ('a Lazy.t, 'f) t

  val request_witness :
       ('var, 'value, 'f field, cvar) Types.Typ.t
    -> ('value Request.t, 'f field, cvar) As_prover0.t
    -> ('var, 'f field) t

  val request :
       ?such_that:('var -> (unit, 'f field) t)
    -> ('var, 'value, 'f field, cvar) Types.Typ.t
    -> 'value Request.t
    -> ('var, 'f field) t

  val exists_handle :
       ?request:('value Request.t, 'f field, cvar) As_prover0.t
    -> ?compute:('value, 'f field, cvar) As_prover0.t
    -> ('var, 'value, 'f field, cvar) Types.Typ.t
    -> (('var, 'value) Handle.t, 'f field) t

  val exists :
       ?request:('value Request.t, 'f field, cvar) As_prover0.t
    -> ?compute:('value, 'f field, cvar) As_prover0.t
    -> ('var, 'value, 'f field, cvar) Types.Typ.t
    -> ('var, 'f field) t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request : 'a Request.t; respond : 'a Request.Response.t -> response }
        -> request

  val handle :
    (unit -> ('a, 'f field) t) -> (request -> response) -> ('a, 'f field) t

  val handle_as_prover :
       (unit -> ('a, 'f field) t)
    -> (request -> response, 'f field, cvar) As_prover0.t
    -> ('a, 'f field) t

  val next_auxiliary : unit -> (int, 'f field) t

  val with_label : string -> (unit -> ('a, 'f field) t) -> ('a, 'f field) t

  val assert_ :
    ?label:Base.string -> (cvar, 'f field) Constraint.t -> (unit, 'f field) t

  val assert_r1cs :
    ?label:Base.string -> cvar -> cvar -> cvar -> (unit, 'f field) t

  val assert_square : ?label:Base.string -> cvar -> cvar -> (unit, 'f field) t

  val assert_all :
       ?label:Base.string
    -> (cvar, 'f field) Constraint.t list
    -> (unit, 'f field) t

  val assert_equal : ?label:Base.string -> cvar -> cvar -> (unit, 'f field) t

  val direct : (run_state -> run_state * 'a) -> ('a, 'f field) t

  val constraint_count :
       ?weight:((cvar, 'f field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, 'f field) t)
    -> int
end

module type Extended = sig
  type field

  module Types : Types.Types

  type 'a t = ('a, field) Types.Checked.t

  include
    S
      with module Types := Types
      with type 'f field := field
       and type ('a, 'f) t := ('a, 'f) Types.Checked.t

  val run : 'a t -> run_state -> run_state * 'a
end

module Unextend (Checked : Extended) :
  S with module Types = Checked.Types with type 'f field = Checked.field =
struct
  include (
    Checked :
      S
        with module Types = Checked.Types
        with type 'f field := Checked.field
         and type ('a, 'f) t := ('a, 'f) Checked.Types.Checked.t )

  type 'f field = Checked.field

  type ('a, 'f) t = ('a, 'f) Types.Checked.t
end
