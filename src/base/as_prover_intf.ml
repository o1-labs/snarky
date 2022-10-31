module type Basic = sig
  type ('a, 'f) t

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
      -> string list
      -> ('f field Cvar.t -> 'f field)
      -> Request.Handler.t
      -> 'a
  end

  module Handle : sig
    val value : ('var, 'value) Handle.t -> ('value, 'f field) Types.As_prover.t
  end
end

module type S = sig
  module Types : Types.Types

  include
    Basic
      with type ('a, 'f) t = ('a, 'f) Types.As_prover.t
       and type ('a, 'f) Provider.t = ('a, 'f) Types.Provider.t

  module Ref : sig
    type 'a t

    val create :
      ('a, 'f field) Types.As_prover.t -> ('a t, 'f field) Types.Checked.t

    val get : 'a t -> ('a, 'f field) Types.As_prover.t

    val set : 'a t -> 'a -> (unit, 'f field) Types.As_prover.t

    val typ : ('a t, 'a, 'f field) Types.Typ.t
  end
end

module type Extended = sig
  type field

  module Types : Types.Types

  include
    S
      with module Types := Types
      with type 'f field := field
       and type ('a, 'f) t := ('a, 'f) Types.As_prover.t

  type 'a t = ('a, field) Types.As_prover.t
end
