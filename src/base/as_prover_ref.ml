open Core_kernel

type 'a t = 'a option ref

module type S = sig
  module Types : Types.Types

  type ('a, 'f) checked

  type 'f field

  type nonrec 'a t = 'a t

  val create : ('a, 'f field) As_prover0.t -> ('a t, 'f field) checked

  val get : 'a t -> ('a, 'f field) As_prover0.t

  val set : 'a t -> 'a -> (unit, 'f field) As_prover0.t
end

module Make
    (Checked : Checked_intf.S)
    (As_prover : As_prover_intf.Basic
                   with type 'f field := 'f Checked.field
                    and type ('a, 'f) Provider.t =
                     ('a, 'f) Checked.Types.Provider.t) :
  S
    with module Types = Checked.Types
     and type ('a, 'f) checked := ('a, 'f) Checked.t
     and type 'f field = 'f Checked.field = struct
  module Types = Checked.Types

  type 'f field = 'f Checked.field

  type nonrec 'a t = 'a t

  let create (x : ('a, 'f Checked.field) As_prover.t) :
      ('a t, 'f Checked.field) Checked.t =
    let r = ref None in
    let open Checked in
    let%map () =
      Checked.as_prover (As_prover.map x ~f:(fun x -> r := Some x))
    in
    r

  open As_prover.Let_syntax

  let get (r : 'a t) =
    let%map () = As_prover.return () in
    Option.value_exn !r

  let set (r : 'a t) x =
    let%map () = As_prover.return () in
    r := Some x
end
