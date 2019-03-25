module type Basic = sig
  type ('a, 's, 'f) t

  include Monad_let.S3 with type ('a, 's, 'f) t := ('a, 's, 'f) t

  val add_constraint : 'f Cvar.t Constraint.t -> (unit, 's, 'f) t

  val as_prover : (unit, 'f, 's) As_prover0.t -> (unit, 's, 'f) t

  val with_label : string -> ('a, 's, 'f) t -> ('a, 's, 'f) t

  val with_state :
       ('s1, 'f, 's) As_prover0.t
    -> ('s1 -> (unit, 'f, 's) As_prover0.t)
    -> ('a, 's1, 'f) t
    -> ('a, 's, 'f) t

  val with_handler : Request.Handler.single -> ('a, 's, 'f) t -> ('a, 's, 'f) t

  val clear_handler : ('a, 's, 'f) t -> ('a, 's, 'f) t

  val exists :
       ('var, 'value, 'f) Types.Typ.t
    -> ('value, 'f, 's) Provider.t
    -> (('var, 'value) Handle.t, 's, 'f) t

  val next_auxiliary : (int, 's, 'f) t
end

module type S = sig
  type ('a, 's, 'f) t

  include Monad_let.S3 with type ('a, 's, 'f) t := ('a, 's, 'f) t

  val as_prover : (unit, 'f, 's) As_prover0.t -> (unit, 's, 'f) t

  val request_witness :
       ('var, 'value, 'f) Types.Typ.t
    -> ('value Request.t, 'f, 's) As_prover0.t
    -> ('var, 's, 'f) t

  val request :
       ?such_that:('var -> (unit, 's, 'field) t)
    -> ('var, 'value, 'field) Types.Typ.t
    -> 'value Request.t
    -> ('var, 's, 'field) t

  val exists :
       ?request:('value Request.t, 'f, 's) As_prover0.t
    -> ?compute:('value, 'f, 's) As_prover0.t
    -> ('var, 'value, 'f) Types.Typ.t
    -> ('var, 's, 'f) t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  val handle : ('a, 's, 'f) t -> (request -> response) -> ('a, 's, 'f) t

  val next_auxiliary : (int, 's, 'f) t

  val with_label : string -> ('a, 's, 'f) t -> ('a, 's, 'f) t

  val with_state :
       ?and_then:('s1 -> (unit, 'f, 's) As_prover0.t)
    -> ('s1, 'f, 's) As_prover0.t
    -> ('a, 's1, 'f) t
    -> ('a, 's, 'f) t

  val assert_ :
    ?label:Base.string -> 'f Cvar.t Constraint.t -> (unit, 's, 'f) t

  val assert_r1cs :
       ?label:Base.string
    -> 'f Cvar.t
    -> 'f Cvar.t
    -> 'f Cvar.t
    -> (unit, 's, 'f) t

  val assert_square :
    ?label:Base.string -> 'f Cvar.t -> 'f Cvar.t -> (unit, 's, 'f) t

  val assert_all :
    ?label:Base.string -> 'f Cvar.t Constraint.t list -> (unit, 's, 'f) t

  val assert_equal :
    ?label:Base.string -> 'f Cvar.t -> 'f Cvar.t -> (unit, 's, 'f) t
end
