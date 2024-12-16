module type Basic = sig
  type ('a, 'f) t = ('a, 'f) Types.As_prover.t

  type field

  include Monad_let.S2 with type ('a, 'f) t := ('a, field) t

  val run : ('a, field) t -> (field Cvar.t -> field) -> 'a

  val map2 :
    ('a, field) t -> ('b, field) t -> f:('a -> 'b -> 'c) -> ('c, field) t

  val read_var : field Cvar.t -> (field, field) t

  val read : ('var, 'value, field, _) Types.Typ.t -> 'var -> ('value, field) t

  module Provider : sig
    type ('a, 'f) t

    val run :
      ('a, field) t -> (field Cvar.t -> field) -> Request.Handler.t -> 'a option
  end

  module Handle : sig
    val value : ('var, 'value) Handle.t -> ('value, field) Types.As_prover.t
  end
end
