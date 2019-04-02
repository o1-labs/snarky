module type Basic = sig
  type ('a, 's, 'f) t

  type 'f field

  include Monad_let.S3 with type ('a, 's, 'f) t := ('a, 's, 'f) t

  val add_constraint : 'f field Cvar.t Constraint.t -> (unit, 's, 'f field) t

  val as_prover : (unit, 'f field, 's) As_prover0.t -> (unit, 's, 'f field) t

  val with_label : string -> ('a, 's, 'f field) t -> ('a, 's, 'f field) t

  val with_state :
       ('s1, 'f field, 's) As_prover0.t
    -> ('s1 -> (unit, 'f field, 's) As_prover0.t)
    -> ('a, 's1, 'f field) t
    -> ('a, 's, 'f field) t

  val with_handler :
    Request.Handler.single -> ('a, 's, 'f field) t -> ('a, 's, 'f field) t

  val clear_handler : ('a, 's, 'f field) t -> ('a, 's, 'f field) t

  val exists :
       ('var, 'value, 'f field, (unit, unit, 'f field) t) Types.Typ.t
    -> ('value, 'f field, 's) Provider.t
    -> (('var, 'value) Handle.t, 's, 'f field) t

  val next_auxiliary : (int, 's, 'f field) t
end

module type S = sig
  type ('a, 's, 'f) t

  type 'f field

  include Monad_let.S3 with type ('a, 's, 'f) t := ('a, 's, 'f) t

  val as_prover : (unit, 'f field, 's) As_prover0.t -> (unit, 's, 'f field) t

  val request_witness :
       ('var, 'value, 'f field, (unit, unit, 'f field) t) Types.Typ.t
    -> ('value Request.t, 'f field, 's) As_prover0.t
    -> ('var, 's, 'f field) t

  val request :
       ?such_that:('var -> (unit, 's, 'f field) t)
    -> ('var, 'value, 'f field, (unit, unit, 'f field) t) Types.Typ.t
    -> 'value Request.t
    -> ('var, 's, 'f field) t

  val exists_handle :
       ?request:('value Request.t, 'f, 's) As_prover0.t
    -> ?compute:('value, 'f, 's) As_prover0.t
    -> ('var, 'value, 'f, (unit, unit, 'f) t) Types.Typ.t
    -> (('var, 'value) Handle.t, 's, 'f) t

  val exists :
       ?request:('value Request.t, 'f field, 's) As_prover0.t
    -> ?compute:('value, 'f field, 's) As_prover0.t
    -> ('var, 'value, 'f field, (unit, unit, 'f field) t) Types.Typ.t
    -> ('var, 's, 'f field) t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  val handle :
    ('a, 's, 'f field) t -> (request -> response) -> ('a, 's, 'f field) t

  val next_auxiliary : (int, 's, 'f field) t

  val with_label : string -> ('a, 's, 'f field) t -> ('a, 's, 'f field) t

  val with_state :
       ?and_then:('s1 -> (unit, 'f field, 's) As_prover0.t)
    -> ('s1, 'f field, 's) As_prover0.t
    -> ('a, 's1, 'f field) t
    -> ('a, 's, 'f field) t

  val assert_ :
       ?label:Base.string
    -> 'f field Cvar.t Constraint.t
    -> (unit, 's, 'f field) t

  val assert_r1cs :
       ?label:Base.string
    -> 'f field Cvar.t
    -> 'f field Cvar.t
    -> 'f field Cvar.t
    -> (unit, 's, 'f field) t

  val assert_square :
       ?label:Base.string
    -> 'f field Cvar.t
    -> 'f field Cvar.t
    -> (unit, 's, 'f field) t

  val assert_all :
       ?label:Base.string
    -> 'f field Cvar.t Constraint.t list
    -> (unit, 's, 'f field) t

  val assert_equal :
       ?label:Base.string
    -> 'f field Cvar.t
    -> 'f field Cvar.t
    -> (unit, 's, 'f field) t
end

module type Extended = sig
  type field

  include S with type 'f field := field

  val run :
       ('a, 's, field) t
    -> ('s, field) Types.Run_state.t
    -> ('s, field) Types.Run_state.t * 'a
end

module Unextend (Checked : Extended) :
  S
  with type 'f field = Checked.field
   and type ('a, 's, 'f) t = ('a, 's, 'f) Checked.t = struct
  include (
    Checked :
      S
      with type 'f field := Checked.field
       and type ('a, 's, 'f) t = ('a, 's, 'f) Checked.t )

  type 'f field = Checked.field
end
