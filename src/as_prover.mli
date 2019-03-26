(** {!type:t} is the type of functions that the prover can run during the
    course of a checked computation.
    
    We form a {{: https://en.wikipedia.org/wiki/Monad_(functional_programming)}monad}
    over {!type:t} so that we have a simple way to interact with values inside the type
    (the value of type ['a] corresponding to our [('a, 'f, 's) t]).
    *)
include
  Monad_let.S3 with type ('a, 'f, 's) t = ('f Cvar.t -> 'f) -> 's -> 's * 'a

val run : ('a, 'f, 's) t -> ('f Cvar.t -> 'f) -> 's -> 's * 'a

module type S = sig
  type field

  include Monad_let.S2 with type ('a, 's) t = ('a, field, 's) t

  val run : ('a, 's) t -> (field Cvar.t -> field) -> 's -> 's * 'a

  val get_state : ('s, 's) t

  val set_state : 's -> (unit, 's) t

  val modify_state : ('s -> 's) -> (unit, 's) t

  val map2 : ('a, 's) t -> ('b, 's) t -> f:('a -> 'b -> 'c) -> ('c, 's) t

  val read_var : field Cvar.t -> (field, 's) t

  val read :
    ('var, 'value, field, 'r) Typ.t -> 'var -> ('value, 'prover_state) t

  module Ref : sig
    type 'a t

    val create :
         ('a, field, 'prover_state) As_prover0.t
      -> ('a t, 'prover_state, field, 'r) Checked.t

    val get : 'a t -> ('a, field, _) As_prover0.t

    val set : 'a t -> 'a -> (unit, field, _) As_prover0.t
  end
end

module Make (Env : sig
  type field
end) : S with type field := Env.field
