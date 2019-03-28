module type Basic = sig
  type ('a, 'f, 's) t

  type 'f field

  module Checked : Checked_intf.S

  include Monad_let.S3 with type ('a, 'f, 's) t := ('a, 'f field, 's) t

  type ('a, 'f, 's) as_prover = ('a, 'f, 's) t

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
       ('var, 'value, 'f field, (unit, unit, 'f field) Checked.t) Types.Typ.t
    -> 'var
    -> ('value, 'f field, 'prover_state) t

  module Ref : sig
    type 'a t

    val create :
         ('a, 'f field, 'prover_state) as_prover
      -> ('a t, 'prover_state, 'f field) Checked.t

    val get : 'a t -> ('a, 'f field, _) as_prover

    val set : 'a t -> 'a -> (unit, 'f field, _) as_prover
  end
end

module type S = sig
  type ('a, 's) t

  type field

  include
    Basic
    with type 'f field := field
     and type ('a, 'f, 's) t := ('a, 's) t
     and type ('a, 'f, 's) as_prover := ('a, 's) t
end

module Make_basic (Checked : Checked_intf.S) :
  Basic
  with type 'f field = 'f
   and type ('a, 'f, 's) t = ('a, 'f, 's) As_prover0.t
  with module Checked := Checked

include
  Basic
  with type 'f field := 'f
   and type ('a, 'f, 's) t = ('a, 'f, 's) As_prover0.t
  with module Checked := Checked

module Make (Env : sig
  type field
end)
(Checked : Checked_intf.S)
(Basic : Basic with type 'f field := Env.field with module Checked := Checked) :
  S
  with type field := Env.field
   and type ('a, 's) t = ('a, Env.field, 's) Basic.t
  with module Checked := Checked
