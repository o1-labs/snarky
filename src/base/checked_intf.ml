module type Basic = sig
  module Types : Types.Types

  type constraint_

  type 'a t = 'a Types.Checked.t

  type run_state

  include Monad_lib.Monad_let.S with type 'a t := 'a t

  val add_constraint : constraint_ -> unit t

  val as_prover : unit Types.As_prover.t -> unit t

  val mk_lazy : (unit -> 'a t) -> 'a Lazy.t t

  val with_label : string -> (unit -> 'a t) -> 'a t

  val with_handler : Request.Handler.single -> (unit -> 'a t) -> 'a t

  val exists :
       ('var, 'value) Types.Typ.t
    -> 'value Types.Provider.t
    -> ('var, 'value) Handle.t t

  val next_auxiliary : unit -> int t

  val direct : (run_state -> run_state * 'a) -> 'a t

  val constraint_count :
       ?weight:(constraint_ -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> 'a t)
    -> int
end

module type S = sig
  module Types : Types.Types

  type constraint_

  type run_state

  type 'a t = 'a Types.Checked.t

  include Monad_lib.Monad_let.S with type 'a t := 'a t

  val as_prover : unit Types.As_prover.t -> unit t

  val mk_lazy : (unit -> 'a t) -> 'a Lazy.t t

  val request_witness :
    ('var, 'value) Types.Typ.t -> 'value Request.t Types.As_prover.t -> 'var t

  val request :
       ?such_that:('var -> unit t)
    -> ('var, 'value) Types.Typ.t
    -> 'value Request.t
    -> 'var t

  val exists_handle :
       ?request:'value Request.t Types.As_prover.t
    -> ?compute:'value Types.As_prover.t
    -> ('var, 'value) Types.Typ.t
    -> ('var, 'value) Handle.t t

  val exists :
       ?request:'value Request.t Types.As_prover.t
    -> ?compute:'value Types.As_prover.t
    -> ('var, 'value) Types.Typ.t
    -> 'var t

  type response = Request.response

  val unhandled : response

  type request = Request.request =
    | With :
        { request : 'a Request.t; respond : 'a Request.Response.t -> response }
        -> request

  val handle : (unit -> 'a t) -> (request -> response) -> 'a t

  val handle_as_prover :
    (unit -> 'a t) -> (request -> response) Types.As_prover.t -> 'a t

  val next_auxiliary : unit -> int t

  val with_label : string -> (unit -> 'a t) -> 'a t

  val assert_ : constraint_ -> unit t

  val assert_r1cs :
    Types.field_var -> Types.field_var -> Types.field_var -> unit t

  val assert_square : Types.field_var -> Types.field_var -> unit t

  val assert_all : constraint_ list -> unit t

  val assert_equal : Types.field_var -> Types.field_var -> unit t

  val direct : (run_state -> run_state * 'a) -> 'a t

  val constraint_count :
       ?weight:(constraint_ -> int)
    -> ?log:(?start:bool -> string -> int -> unit)
    -> (unit -> 'a t)
    -> int
end

module type Extended = sig
  include S

  val run : 'a t -> run_state -> run_state * 'a
end
