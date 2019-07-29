open Core_kernel
open Type0

let rec equal_at_depth ~depth typ1 typ2 =
  if Int.equal typ1.type_id typ2.type_id then true
  else
    match (typ1.type_desc, typ2.type_desc) with
    | Tvar _, _ when typ1.type_depth > depth ->
        true
    | _, Tvar _ when typ2.type_depth > depth ->
        true
    | Ttuple typs1, Ttuple typs2 -> (
      match List.for_all2 typs1 typs2 ~f:(equal_at_depth ~depth) with
      | Ok b ->
          b
      | Unequal_lengths ->
          false )
    | ( Tarrow (typ1a, typ1b, explicitness1, label1)
      , Tarrow (typ2a, typ2b, explicitness2, label2) ) ->
        explicitness_equal explicitness1 explicitness2
        && arg_label_equal label1 label2
        && equal_at_depth ~depth typ1a typ2a
        && equal_at_depth ~depth typ1b typ2b
    | ( Tctor ({var_decl= decl1; _} as variant1)
      , Tctor ({var_decl= decl2; _} as variant2) )
      when Int.equal decl1.tdec_id decl2.tdec_id ->
        List.for_all2_exn ~f:(equal_at_depth ~depth) variant1.var_params
          variant2.var_params
        && List.for_all2_exn ~f:(equal_at_depth ~depth)
             variant1.var_implicit_params variant2.var_implicit_params
    | Tpoly (typs1, typ1), Tpoly (typs2, typ2) -> (
      match List.for_all2 typs1 typs2 ~f:(equal_at_depth ~depth) with
      | Ok true ->
          equal_at_depth ~depth typ1 typ2
      | _ ->
          false )
    | _, _ ->
        false

(* TODO: integrate with a backtrack mechanism for unification errors. *)
let set_depth depth typ = typ.type_depth <- depth

let update_depth depth typ = if typ.type_depth > depth then set_depth depth typ

let unify_depths typ1 typ2 =
  iter ~f:(update_depth typ1.type_depth) typ2 ;
  iter ~f:(update_depth typ2.type_depth) typ1
