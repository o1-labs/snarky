open Core_kernel

type 'a t = 'a option ref

module Make_ref_typ (Checked : Monad_let.S2) = struct
  let typ : ('a t, 'a, _, _, _) Types.Typ.t =
    Typ
      { var_to_fields = (fun x -> ([||], !x))
      ; var_of_fields = (fun (_, x) -> ref x)
      ; value_to_fields = (fun x -> ([||], Some x))
      ; value_of_fields = (fun (_, x) -> Option.value_exn x)
      ; size_in_field_elements = 0
      ; constraint_system_auxiliary = (fun () -> None)
      ; check = (fun _ -> Checked.return ())
      }
end

module type S = sig
  module Types : Types.Types

  type ('a, 'f) checked

  type 'f field

  type cvar

  type nonrec 'a t = 'a t

  val create : ('a, 'f field, cvar) As_prover0.t -> ('a t, 'f field) checked

  val get : 'a t -> ('a, 'f field, cvar) As_prover0.t

  val set : 'a t -> 'a -> (unit, 'f field, cvar) As_prover0.t
end

module Make
    (Cvar : T)
    (Checked : Checked_intf.S with type cvar := Cvar.t)
    (As_prover : As_prover_intf.Basic
                   with type 'f field := 'f Checked.field
                    and type cvar := Cvar.t
                    and type ('a, 'f) Provider.t =
                     ('a, 'f, Cvar.t) Checked.Types.Provider.t) :
  S
    with module Types = Checked.Types
     and type ('a, 'f) checked := ('a, 'f) Checked.t
     and type 'f field = 'f Checked.field
     and type cvar := Cvar.t = struct
  module Types = Checked.Types

  type 'f field = 'f Checked.field

  type nonrec 'a t = 'a t

  let create (x : ('a, 'f Checked.field, Cvar.t) As_prover.t) :
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
