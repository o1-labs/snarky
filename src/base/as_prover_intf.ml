module type Basic = sig
  type cvar

  type ('a, 'f, 'c) t = ('a, 'f, cvar) Types.As_prover.t

  type 'f field

  include Monad_let.S3 with type ('a, 'f, 'c) t := ('a, 'f field, cvar) t

  val run : ('a, 'f field, cvar) t -> (cvar -> 'f field) -> 'a

  val map2 :
       ('a, 'f field, cvar) t
    -> ('b, 'f field, cvar) t
    -> f:('a -> 'b -> 'c)
    -> ('c, 'f field, cvar) t

  val read_var : cvar -> ('f field, 'f field, cvar) t

  val read :
       ('var, 'value, 'f field, cvar, _) Types.Typ.t
    -> 'var
    -> ('value, 'f field, cvar) t

  module Provider : sig
    type ('a, 'f) t

    val run :
         ('a, 'f field) t
      -> string list
      -> (cvar -> 'f field)
      -> Request.Handler.t
      -> 'a
  end

  module Handle : sig
    val value :
      ('var, 'value) Handle.t -> ('value, 'f field, cvar) Types.As_prover.t
  end
end
