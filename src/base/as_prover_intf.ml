module type Basic = sig
  type ('a, 'f, 'field_var) t = ('a, 'f, 'field_var) Types.As_prover.t

  type 'f field

  type 'f field_var

  include
    Monad_let.S3
      with type ('a, 'f, 'field_var) t := ('a, 'f field, 'f field_var) t

  val run : ('a, 'f field, 'f field_var) t -> ('f field_var -> 'f field) -> 'a

  val map2 :
       ('a, 'f field, 'f field_var) t
    -> ('b, 'f field, 'f field_var) t
    -> f:('a -> 'b -> 'c)
    -> ('c, 'f field, 'f field_var) t

  val read_var : 'f field_var -> ('f field, 'f field, 'f field_var) t

  val read :
       ('var, 'value, 'f field, 'f field_var, _) Types.Typ.t
    -> 'var
    -> ('value, 'f field, 'f field_var) t

  module Provider : sig
    type ('a, 'f, 'field_var) t

    val run :
         ('a, 'f field, 'f field_var) t
      -> string list
      -> ('f field_var -> 'f field)
      -> Request.Handler.t
      -> 'a
  end

  module Handle : sig
    val value :
         ('var, 'value) Handle.t
      -> ('value, 'f field, 'f field_var) Types.As_prover.t
  end
end
