module type Basic = sig
  type ('a, 'f) t = ('a, 'f) Types.Checked.t

  type 'f field

  include Monad_let.S2 with type ('a, 'f) t := ('a, 'f) t

  val add_constraint :
    ('f field Cvar.t, 'f field) Constraint.t -> (unit, 'f field) t

  val as_prover : (unit, 'f field) Types.As_prover.t -> (unit, 'f field) t

  val mk_lazy : (unit -> ('a, 'f) t) -> ('a Lazy.t, 'f) t

  val with_label : string -> (unit -> ('a, 'f field) t) -> ('a, 'f field) t

  val with_handler :
    Request.Handler.single -> (unit -> ('a, 'f field) t) -> ('a, 'f field) t

  val exists :
       ('var, 'value, 'f field) Types.Typ.t
    -> ('value, 'f field) Types.Provider.t
    -> (('var, 'value) Handle.t, 'f field) t

  val next_auxiliary : unit -> (int, 'f field) t

  val direct :
    ('f field Run_state.t -> 'f field Run_state.t * 'a) -> ('a, 'f field) t

  val constraint_count :
       ?weight:(('f field Cvar.t, 'f field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, 'f field) t)
    -> int
end

module type S = sig
  type ('a, 'f) t = ('a, 'f) Types.Checked.t

  type 'f field

  include Monad_let.S2 with type ('a, 'f) t := ('a, 'f) t

  val as_prover : (unit, 'f field) Types.As_prover.t -> (unit, 'f field) t

  val mk_lazy : (unit -> ('a, 'f) t) -> ('a Lazy.t, 'f) t

  val request_witness :
       ('var, 'value, 'f field) Types.Typ.t
    -> ('value Request.t, 'f field) Types.As_prover.t
    -> ('var, 'f field) t

  val request :
       ?such_that:('var -> (unit, 'f field) t)
    -> ('var, 'value, 'f field) Types.Typ.t
    -> 'value Request.t
    -> ('var, 'f field) t

  val exists_handle :
       ?request:('value Request.t, 'f field) Types.As_prover.t
    -> ?compute:('value, 'f field) Types.As_prover.t
    -> ('var, 'value, 'f field) Types.Typ.t
    -> (('var, 'value) Handle.t, 'f field) t

  val exists :
       ?request:('value Request.t, 'f field) Types.As_prover.t
    -> ?compute:('value, 'f field) Types.As_prover.t
    -> ('var, 'value, 'f field) Types.Typ.t
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
    -> (request -> response, 'f field) Types.As_prover.t
    -> ('a, 'f field) t

  val next_auxiliary : unit -> (int, 'f field) t

  val with_label : string -> (unit -> ('a, 'f field) t) -> ('a, 'f field) t

  val assert_ :
       ?label:Base.string
    -> ('f field Cvar.t, 'f field) Constraint.t
    -> (unit, 'f field) t

  val assert_r1cs :
       ?label:Base.string
    -> 'f field Cvar.t
    -> 'f field Cvar.t
    -> 'f field Cvar.t
    -> (unit, 'f field) t

  val assert_square :
       ?label:Base.string
    -> 'f field Cvar.t
    -> 'f field Cvar.t
    -> (unit, 'f field) t

  val assert_all :
       ?label:Base.string
    -> ('f field Cvar.t, 'f field) Constraint.t list
    -> (unit, 'f field) t

  val assert_equal :
       ?label:Base.string
    -> 'f field Cvar.t
    -> 'f field Cvar.t
    -> (unit, 'f field) t

  val direct :
    ('f field Run_state.t -> 'f field Run_state.t * 'a) -> ('a, 'f field) t

  val constraint_count :
       ?weight:(('f field Cvar.t, 'f field) Constraint.t -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> ('a, 'f field) t)
    -> int
end

module type Extended = sig
  type field

  type 'a t = ('a, field) Types.Checked.t

  include
    S
      with type 'f field := field
       and type ('a, 'f) t := ('a, 'f) Types.Checked.t

  val run : 'a t -> field Run_state.t -> field Run_state.t * 'a
end

module Unextend (Checked : Extended) : S with type 'f field = Checked.field =
struct
  include (
    Checked :
      S
        with type 'f field := Checked.field
         and type ('a, 'f) t := ('a, 'f) Types.Checked.t )

  type 'f field = Checked.field

  type ('a, 'f) t = ('a, 'f) Types.Checked.t
end
