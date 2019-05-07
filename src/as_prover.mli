module type Basic = sig
  type ('a, 'f, 's) t

  type 'f field

  type ('a, 's, 'f) checked

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
       ('var, 'value, 'f field, (unit, unit, 'f field) checked) Types.Typ.t
    -> 'var
    -> ('value, 'f field, 'prover_state) t

  val with_lens :
    ('whole, 'view) Lens.t -> ('a, 'f, 'view) t -> ('a, 'f, 'whole) t

  module Ref : sig
    type 'a t

    val create :
         ('a, 'f field, 'prover_state) as_prover
      -> ('a t, 'prover_state, 'f field) checked

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
  with type 'f field = 'f Checked.field
   and type ('a, 'f, 's) t = ('a, 'f, 's) As_prover0.t
   and type ('a, 's, 'f) checked := ('a, 's, 'f) Checked.t

include
  Basic
  with type 'f field := 'f
   and type ('a, 'f, 's) t = ('a, 'f, 's) As_prover0.t
   and type ('a, 's, 'f) checked := ('a, 's, 'f) Checked.t

module Make (Env : sig
  type field
end)
(Checked : Checked_intf.S with type 'f field := Env.field)
(Basic : Basic
         with type 'f field := Env.field
          and type ('a, 's, 'f) checked := ('a, 's, 'f) Checked.t) :
  S
  with type field := Env.field
   and type ('a, 's) t = ('a, Env.field, 's) Basic.t
   and type ('a, 's, 'f) checked := ('a, 's, 'f) Checked.t
