module type Basic' = sig
  module Types : Types.Types

  open Types.As_prover

  type 'f field

  val return : 'a -> ('a, 'f, 's) t

  val map : ('a, 'f, 's) t -> f:('a -> 'b) -> ('b, 'f, 's) t

  val bind : ('a, 'f, 's) t -> f:('a -> ('b, 'f, 's) t) -> ('b, 'f, 's) t

  val run :
    ('a, 'f field, 's) t -> ('f field Cvar.t -> 'f field) -> 's -> 's * 'a

  val wrap : ('s -> 's * 'a) -> ('a, 'f field, 's) t

  val with_read : (('f field Cvar.t -> 'f field) -> 'a) -> ('a, 'f field, 's) t
end

module type Basic = sig
  include Basic'

  type ('a, 'f, 's) t = ('a, 'f, 's) Types.As_prover.t
end

module type S' = sig
  module Types : Types.Types

  open Types.As_prover

  type 'f field

  include
    Monad_let.S3 with type ('a, 'f, 's) t := ('a, 'f, 's) Types.As_prover.t

  val run :
    ('a, 'f field, 's) t -> ('f field Cvar.t -> 'f field) -> 's -> 's * 'a

  val get_state : ('s, 'f field, 's) t

  val set_state : 's -> (unit, 'f field, 's) t

  val modify_state : ('s -> 's) -> (unit, 'f field, 's) t

  val map2 :
       ('a, 'f field, 's) t
    -> ('b, 'f field, 's) t
    -> f:('a -> 'b -> 'c)
    -> ('c, 'f field, 's) t

  val read_var : 'f field Cvar.t -> ('f field, 'f field, 's) t

  val read :
       ('var, 'value, 'f field) Types.Typ.t
    -> 'var
    -> ('value, 'f field, 'prover_state) t

  module Ref : sig
    type 'a t

    val create :
         ('a, 'f field, 'prover_state) Types.As_prover.t
      -> ('a t, 'prover_state, 'f field) Types.Checked.t

    val get : 'a t -> ('a, 'f field, _) Types.As_prover.t

    val set : 'a t -> 'a -> (unit, 'f field, _) Types.As_prover.t
  end

  module Provider : sig
    include module type of Types.Provider

    val run :
         ('a, 'f field, 's) t
      -> string list
      -> ('f field Cvar.t -> 'f field)
      -> 's
      -> Request.Handler.t
      -> 's * 'a
  end
end

module type S = sig
  include S'

  type ('a, 'f, 's) t = ('a, 'f, 's) Types.As_prover.t
end

module type Extended = sig
  type field

  module Types : Types.Types

  type ('a, 's) t = ('a, field, 's) Types.As_prover.t

  type ('a, 's) as_prover = ('a, 's) t

  include S' with type 'f field := field with module Types := Types
end
