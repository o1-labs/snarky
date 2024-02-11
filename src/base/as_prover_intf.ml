module type Basic = sig
  type ('a, 'f) t = ('a, 'f) Types.As_prover.t

  type 'f field

  include Monad_let.S2 with type ('a, 'f) t := ('a, 'f field) t

  val run : ('a, 'f field) t -> ('f field Cvar.t -> 'f field) -> 'a

  val map2 :
       ('a, 'f field) t
    -> ('b, 'f field) t
    -> f:('a -> 'b -> 'c)
    -> ('c, 'f field) t

  val read_var : 'f field Cvar.t -> ('f field, 'f field) t

  val read :
    ('var, 'value, 'f field, _) Types.Typ.t -> 'var -> ('value, 'f field) t

  module Provider : sig
    type ('a, 'f) t

    val run :
         ('a, 'f field) t
      -> ('f field Cvar.t -> 'f field)
      -> Request.Handler.t
      -> 'a option
  end

  module Handle : sig
    val value : ('var, 'value) Handle.t -> ('value, 'f field) Types.As_prover.t
  end
end
