module type Backend_types = sig
  module Field : sig
    type t

    module Var : sig
      type t
    end
  end
end

module Runner_state (M : Backend_types) = struct
  module type S = sig
    type 's t

    type 's prover_state

    val to_prover_state : 's -> 's prover_state

    val get_value : 's t -> M.Field.Var.t -> M.Field.t

    val store_field_elt : 's t -> M.Field.t -> M.Field.Var.t

    val alloc_var : 's t -> unit -> M.Field.Var.t

    module Constraint : sig
      val add : stack:string list -> M.Field.Var.t Constraint.t -> 'a t -> unit

      val eval : M.Field.Var.t Constraint.t -> 's t -> bool
    end

    val eval_constraints : 's t -> bool

    val next_auxiliary : 's t -> int ref

    val prover_state : 's t -> 's prover_state option

    val set_prover_state : 's prover_state option -> 's1 t -> 's t

    val stack : 's t -> string list

    val set_stack : string list -> 's t -> 's t

    val handler : 's t -> Request.Handler.t

    val set_handler : Request.Handler.t -> 's t -> 's t
  end

  module type S_imperative = sig
    include S with type 'a prover_state = 'a

    val initial_state : unit t ref

    val run : ('a, 's, M.Field.t, M.Field.Var.t) Checked.t -> 's t -> 's t * 'a
  end
end

module type S = sig
  type ('a, 's, 'field, 'field_var) t

  type 's prover_state

  val assert_ :
       ?label:string
    -> 'field_var Constraint.t
    -> (unit, 's, 'field, 'field_var) t

  val assert_all :
       ?label:string
    -> 'field_var Constraint.t list
    -> (unit, 's, 'field, 'field_var) t

  val assert_r1cs :
       ?label:string
    -> 'field_var
    -> 'field_var
    -> 'field_var
    -> (unit, _, 'field, 'field_var) t

  val assert_square :
       ?label:string
    -> 'field_var
    -> 'field_var
    -> (unit, _, 'field, 'field_var) t

  val as_prover :
       (unit, 'field_var -> 'field, 's prover_state) As_prover0.t
    -> (unit, 's, 'field, 'field_var) t

  val with_state :
       ?and_then:(   's1
                  -> (unit, 'field_var -> 'field, 's prover_state) As_prover0.t)
    -> ('s1, 'field_var -> 'field, 's prover_state) As_prover0.t
    -> ('a, 's1, 'field, 'field_var) t
    -> ('a, 's, 'field, 'field_var) t

  val next_auxiliary : (int, 's, 'field, 'field_var) t

  val request_witness :
       ('var, 'value, 'field, 'field_var) Typ.t
    -> ('value Request.t, 'field_var -> 'field, 's prover_state) As_prover0.t
    -> ('var, 's, 'field, 'field_var) t

  val perform :
       (unit Request.t, 'field_var -> 'field, 's prover_state) As_prover0.t
    -> (unit, 's, 'field, 'field_var) t

  val request :
       ?such_that:('var -> (unit, 's, 'field, 'field_var) t)
    -> ('var, 'value, 'field, 'field_var) Typ.t
    -> 'value Request.t
    -> ('var, 's, 'field, 'field_var) t
  (** TODO: Come up with a better name for this in relation to the above *)

  val exists :
       ?request:( 'value Request.t
                , 'field_var -> 'field
                , 's prover_state )
                As_prover0.t
    -> ?compute:('value, 'field_var -> 'field, 's prover_state) As_prover0.t
    -> ('var, 'value, 'field, 'field_var) Typ.t
    -> ('var, 's, 'field, 'field_var) t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request: 'a Request.t
        ; respond: 'a Request.Response.t -> response }
        -> request

  module Handler : sig
    type t = request -> response
  end

  val handle :
       ('a, 's, 'field, 'field_var) t
    -> Handler.t
    -> ('a, 's, 'field, 'field_var) t

  val with_label :
    string -> ('a, 's, 'field, 'field_var) t -> ('a, 's, 'field, 'field_var) t
end
