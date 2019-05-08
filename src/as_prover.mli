module type Basic = sig
  module Types : Types.Types

  type ('a, 'f, 's) t = ('a, 'f, 's) Types.As_prover.t

  type 'f field

  include Monad_let.S3 with type ('a, 'f, 's) t := ('a, 'f field, 's) t

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

  module Provider : sig
    type ('a, 'f, 's) t = ('a, 'f, 's) Types.Provider.t

    val run :
         ('a, 'f field, 's) t
      -> string list
      -> ('f field Cvar.t -> 'f field)
      -> 's
      -> Request.Handler.t
      -> 's * 'a
  end

  module Ref : sig
    type 'a t

    val create :
         ('a, 'f field, 'prover_state) Types.As_prover.t
      -> ('a t, 'prover_state, 'f field) Types.Checked.t

    val get : 'a t -> ('a, 'f field, _) Types.As_prover.t

    val set : 'a t -> 'a -> (unit, 'f field, _) Types.As_prover.t
  end
end

module type S = sig
  type field

  module Types : Types.Types

  include
    Basic
    with module Types := Types
    with type 'f field := field
     and type ('a, 'f, 's) t := ('a, 'f, 's) Types.As_prover.t

  type ('a, 's) t = ('a, field, 's) Types.As_prover.t
end

module Make_basic (Checked : Checked_intf.S) :
  Basic with module Types = Checked.Types with type 'f field = 'f Checked.field

include Basic with module Types = Checked.Types with type 'f field := 'f

module Make (Env : sig
  type field
end)
(Checked : Checked_intf.S with type 'f field := Env.field)
(Basic : Basic
         with module Types := Checked.Types
         with type 'f field := Env.field) :
  S
  with module Types = Checked.Types
  with type field := Env.field
   and type ('a, 's) t = ('a, Env.field, 's) Basic.t
