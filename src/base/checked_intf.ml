module type Basic = sig
  module Types : Types.Types

  type ('a, 'f) t = ('a, 'f) Types.Checked.t

  type field

  include Monad_let.S2 with type ('a, 'f) t := ('a, 'f) t

  val add_constraint : (field Cvar.t, field) Constraint.t -> (unit, field) t

  val as_prover : (unit, field) Types.As_prover.t -> (unit, field) t

  val mk_lazy : (unit -> ('a, 'f) t) -> ('a Lazy.t, 'f) t

  val with_label : string -> (unit -> ('a, field) t) -> ('a, field) t

  val with_handler :
    Request.Handler.single -> (unit -> ('a, field) t) -> ('a, field) t

  val exists :
       ('var, 'value, field) Types.Typ.t
    -> ('value, field) Types.Provider.t
    -> (('var, 'value) Handle.t, field) t

  val next_auxiliary : unit -> (int, field) t

  val direct : (field Run_state.t -> field Run_state.t * 'a) -> ('a, field) t

  val constraint_count :
       ?weight:((field Cvar.t, field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, field) t)
    -> int
end

module type S = sig
  module Types : Types.Types

  type ('a, 'f) t = ('a, 'f) Types.Checked.t

  type field

  include Monad_let.S2 with type ('a, 'f) t := ('a, 'f) t

  val as_prover : (unit, field) Types.As_prover.t -> (unit, field) t

  val mk_lazy : (unit -> ('a, 'f) t) -> ('a Lazy.t, 'f) t

  val request_witness :
       ('var, 'value, field) Types.Typ.t
    -> ('value Request.t, field) Types.As_prover.t
    -> ('var, field) t

  val request :
       ?such_that:('var -> (unit, field) t)
    -> ('var, 'value, field) Types.Typ.t
    -> 'value Request.t
    -> ('var, field) t

  val exists_handle :
       ?request:('value Request.t, field) Types.As_prover.t
    -> ?compute:('value, field) Types.As_prover.t
    -> ('var, 'value, field) Types.Typ.t
    -> (('var, 'value) Handle.t, field) t

  val exists :
       ?request:('value Request.t, field) Types.As_prover.t
    -> ?compute:('value, field) Types.As_prover.t
    -> ('var, 'value, field) Types.Typ.t
    -> ('var, field) t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request : 'a Request.t; respond : 'a Request.Response.t -> response }
        -> request

  val handle : (unit -> ('a, field) t) -> (request -> response) -> ('a, field) t

  val handle_as_prover :
       (unit -> ('a, field) t)
    -> (request -> response, field) Types.As_prover.t
    -> ('a, field) t

  val next_auxiliary : unit -> (int, field) t

  val with_label : string -> (unit -> ('a, field) t) -> ('a, field) t

  val assert_ :
    ?label:Base.string -> (field Cvar.t, field) Constraint.t -> (unit, field) t

  val assert_r1cs :
       ?label:Base.string
    -> field Cvar.t
    -> field Cvar.t
    -> field Cvar.t
    -> (unit, field) t

  val assert_square :
    ?label:Base.string -> field Cvar.t -> field Cvar.t -> (unit, field) t

  val assert_all :
       ?label:Base.string
    -> (field Cvar.t, field) Constraint.t list
    -> (unit, field) t

  val assert_equal :
    ?label:Base.string -> field Cvar.t -> field Cvar.t -> (unit, field) t

  val direct : (field Run_state.t -> field Run_state.t * 'a) -> ('a, field) t

  val constraint_count :
       ?weight:((field Cvar.t, field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, field) t)
    -> int
end

module type Extended = sig
  type field

  module Types : Types.Types

  type 'a t = ('a, field) Types.Checked.t

  include
    S
      with module Types := Types
      with type field := field
       and type ('a, 'f) t := ('a, 'f) Types.Checked.t

  val run : 'a t -> field Run_state.t -> field Run_state.t * 'a
end

module Unextend (Checked : Extended) :
  S with module Types = Checked.Types with type field = Checked.field = struct
  include (
    Checked :
      S
        with module Types = Checked.Types
        with type field := Checked.field
         and type ('a, 'f) t := ('a, 'f) Checked.Types.Checked.t )

  type field = Checked.field

  type ('a, 'f) t = ('a, 'f) Types.Checked.t
end
