open Core_kernel

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

module Make_basic
    (Checked : Checked_intf.S
               with type ('a, 'f, 's) Types.As_prover.t =
                           ('a, 'f, 's) As_prover0.t) =
struct
  module Types = Checked.Types

  type ('a, 'f, 's) t = ('a, 'f, 's) As_prover0.t

  type 'f field = 'f Checked.field

  include As_prover0.T

  let read ({read; _} : ('var, 'value, 'field) Types.Typ.t) (var : 'var) :
      ('value, 'field, 'prover_state) t =
   fun tbl s -> (s, Typ_monads.Read.run (read var) tbl)

  module Provider = struct
    open Types.Provider

    type ('a, 'f, 's) t = ('a, 'f, 's) Types.Provider.t

    let run t stack tbl s (handler : Request.Handler.t) =
      match t with
      | Request rc ->
          let s', r = As_prover0.run rc tbl s in
          (s', Request.Handler.run handler stack r)
      | Compute c ->
          As_prover0.run c tbl s
      | Both (rc, c) -> (
          let s', r = As_prover0.run rc tbl s in
          match Request.Handler.run handler stack r with
          | exception _ ->
              As_prover0.run c tbl s
          | x ->
              (s', x) )
  end

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

module T : Basic with module Types = Checked.Types with type 'f field := 'f =
  Make_basic (Checked)

include T

module Make (Env : sig
  type field
end)
(Checked : Checked_intf.S with type 'f field := Env.field)
(Basic : Basic
         with module Types := Checked.Types
         with type 'f field := Env.field) =
struct
  module Types = Checked.Types

  type ('a, 's) t = ('a, Env.field, 's) Types.As_prover.t

  include Env

  include (
    Basic :
      Basic
      with module Types := Types
      with type 'f field := field
       and type ('a, 'f, 's) t := ('a, 'f, 's) Types.As_prover.t )
end
