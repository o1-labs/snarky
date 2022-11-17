open Core_kernel
open As_prover_intf

module Ref0 = struct
  type 'a t = 'a option ref
end

module type S = sig
  module Types : Types.Types

  include
    Basic
      with type ('a, 'f) t = ('a, 'f) Types.As_prover.t
       and type ('a, 'f) Provider.t = ('a, 'f) Types.Provider.t

  module Ref : sig
    type 'a t = 'a Ref0.t

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

module Make_ref_typ (Checked : Monad_let.S2) = struct
  let typ : ('a Ref0.t, 'a, _, _) Types.Typ.t =
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

module Make
    (Checked : Checked_intf.S)
    (As_prover : Basic
                   with type ('a, 'f) t := ('a, 'f) Checked.Types.As_prover.t
                    and type 'f field := 'f Checked.field
                    and type ('a, 'f) Provider.t =
                     ('a, 'f) Checked.Types.Provider.t) =
struct
  module Types = Checked.Types

  type ('a, 'f) t = ('a, 'f) Types.As_prover.t

  type 'f field = 'f Checked.field

  include As_prover

  module Ref = struct
    type 'a t = 'a Ref0.t

    let create (x : ('a, 'field) Types.As_prover.t) : ('a t, 'field) Checked.t =
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

    include Make_ref_typ (Checked)
  end
end

module T : S with module Types = Checked_ast.Types with type 'f field := 'f =
  Make (Checked_ast) (As_prover0)

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

  type 'a t = ('a, Env.field) Types.As_prover.t

  include Env

  include (
    As_prover :
      S
        with module Types := Types
        with type 'f field := field
         and type ('a, 'f) t := ('a, 'f) Types.As_prover.t )
end
