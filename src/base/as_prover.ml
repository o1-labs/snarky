open Core_kernel
open As_prover_intf

module Make
    (Checked : Checked_intf.S)
    (As_prover : Basic
                 with type ('a, 'f, 's) t :=
                             ('a, 'f, 's) Checked.Types.As_prover.t
                  and type 'f field := 'f Checked.field
                  and type ('a, 'f, 's) Provider.t =
                             ('a, 'f, 's) Checked.Types.Provider.t) =
struct
  module Types = Checked.Types

  type ('a, 'f, 's) t = ('a, 'f, 's) Types.As_prover.t

  type 'f field = 'f Checked.field

  include As_prover

  module Ref = struct
    type 'a t = 'a option ref

    let create (x : ('a, 'field, 's) Types.As_prover.t) :
        ('a t, 's, 'field) Checked.t =
      let r = ref None in
      let open Checked in
      let%map () =
        Checked.as_prover (As_prover.map x ~f:(fun x -> r := Some x))
      in
      r

    let get (r : 'a t) =
      let%map () = As_prover.return () in
      Option.value_exn !r

    let set (r : 'a t) x =
      let%map () = As_prover.return () in
      r := Some x

    let store x = Typ_monads.Store.return (ref (Some x))

    let read r = Typ_monads.Read.return (Option.value_exn !r)

    let alloc () = Typ_monads.Alloc.return (ref None)
  end
end

module T : S with module Types = Checked.Types with type 'f field := 'f =
  Make (Checked) (As_prover0)

include T

module Make_extended (Env : sig
  type field
end)
(Checked : Checked_intf.S with type 'f field := Env.field)
(As_prover : S
             with module Types := Checked.Types
             with type 'f field := Env.field) =
struct
  module Types = Checked.Types

  type ('a, 's) t = ('a, Env.field, 's) Types.As_prover.t

  include Env

  include (
    As_prover :
      S
      with module Types := Types
      with type 'f field := field
       and type ('a, 'f, 's) t := ('a, 'f, 's) Types.As_prover.t )
end
