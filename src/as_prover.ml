open Core_kernel

type ('a, 'f, 's) t = ('a, 'f, 's) As_prover0.t

type ('a, 'f, 's) as_prover = ('a, 'f, 's) t

module type Basic = sig
  type ('a, 'f, 's) t

  type 'f field

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
       ('var, 'value, 'f field) Typ.t
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

module T = struct
  include As_prover0.T

  let read ({read; _} : ('var, 'value, 'field) Typ.t) (var : 'var) :
      ('value, 'field, 'prover_state) t =
   fun tbl s -> (s, Typ_monads.Read.run (read var) tbl)

  module Ref = struct
    type 'a t = 'a option ref

    let create (x : ('a, 'field, 's) As_prover0.t) :
        ('a t, 's, 'field) Checked.t =
      let r = ref None in
      let open Checked in
      let%map () =
        Checked.as_prover (As_prover0.map x ~f:(fun x -> r := Some x))
      in
      r

    let get (r : 'a t) _tbl s = (s, Option.value_exn !r)

    let set (r : 'a t) x _tbl s = (s, (r := Some x))
  end
end

include T

module Make (Env : sig
  type field
end)
(Basic : Basic with type 'f field := Env.field) =
struct
  type ('a, 's) t = ('a, Env.field, 's) Basic.t

  include Env

  include (
    Basic :
      Basic
      with type 'f field := field
       and type ('a, 'f, 's) t := ('a, 's) t
       and type ('a, 'f, 's) as_prover := ('a, 's) t )
end
