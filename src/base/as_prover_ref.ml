open Core_kernel

type 'a t = 'a option ref

module Make_ref_typ (Checked : Monad_let.S2) = struct
  let typ : ('a t, 'a, _, _) Types.Typ.t =
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
