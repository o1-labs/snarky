open Core_kernel
open Ast_types
open Type0

let type_id = ref 0

let mk depth type_desc =
  incr type_id ;
  {type_desc; type_id= !type_id; type_depth= depth}

let mkvar ?(explicitness = Explicit) depth name =
  mk depth (Tvar (name, explicitness))

let rec typ_debug_print fmt typ =
  let open Format in
  let print i = fprintf fmt i in
  let print_comma fmt () = pp_print_char fmt ',' in
  let print_list pp = pp_print_list ~pp_sep:print_comma pp in
  let print_label fmt = function
    | Asttypes.Nolabel ->
        ()
    | Asttypes.Labelled str ->
        fprintf fmt "~%s:" str
    | Asttypes.Optional str ->
        fprintf fmt "?%s:" str
  in
  print "(%i:" typ.type_id ;
  ( match typ.type_desc with
  | Tvar (None, Explicit) ->
      print "var _"
  | Tvar (Some name, Explicit) ->
      print "var %s@" name
  | Tvar (None, Implicit) ->
      print "implicit_var _"
  | Tvar (Some name, Implicit) ->
      print "implicit_var %s" name
  | Tpoly (typs, typ) ->
      print "poly [%a] %a"
        (print_list typ_debug_print)
        typs typ_debug_print typ
  | Tarrow (typ1, typ2, Explicit, label) ->
      print "%a%a -> %a" print_label label typ_debug_print typ1 typ_debug_print
        typ2
  | Tarrow (typ1, typ2, Implicit, label) ->
      print "%a{%a} -> %a" print_label label typ_debug_print typ1
        typ_debug_print typ2
  | Tctor {var_ident= name; var_params= params; _} ->
      print "%a (%a)" Path.pp name (print_list typ_debug_print) params
  | Ttuple typs ->
      print "(%a)" (print_list typ_debug_print) typs ) ;
  print " @%i)" typ.type_depth

let fold ~init ~f typ =
  match typ.type_desc with
  | Tvar _ ->
      init
  | Ttuple typs ->
      List.fold ~init ~f typs
  | Tarrow (typ1, typ2, _, _) ->
      let acc = f init typ1 in
      f acc typ2
  | Tctor variant ->
      let acc = List.fold ~init ~f variant.var_params in
      List.fold ~init:acc ~f variant.var_implicit_params
  | Tpoly (typs, typ) ->
      let acc = List.fold ~init ~f typs in
      f acc typ

let iter ~f = fold ~init:() ~f:(fun () -> f)

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
        equal_explicitness explicitness1 explicitness2
        && equal_arg_label label1 label2
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

let type_vars ?depth typ =
  let deep_enough =
    match depth with
    | Some depth ->
        fun typ -> depth <= typ.type_depth
    | None ->
        fun _ -> true
  in
  let empty = Typeset.empty in
  let rec type_vars set typ =
    match typ.type_desc with
    | Tvar _ when deep_enough typ ->
        Set.add set typ
    | Tpoly (vars, typ) ->
        let poly_vars = List.fold ~init:empty vars ~f:type_vars in
        Set.union set (Set.diff (type_vars empty typ) poly_vars)
    | _ ->
        fold ~init:set typ ~f:type_vars
  in
  type_vars empty typ

let mk_option : (Type0.type_expr -> Type0.type_expr) ref =
  ref (fun _ -> failwith "mk_option not initialised")

let rec bubble_label_aux label typ =
  let {type_depth; _} = typ in
  match typ.type_desc with
  | Tarrow (typ1, typ2, explicit, arr_label)
    when Int.equal (compare_arg_label label arr_label) 0 ->
      (Some (typ1, explicit, arr_label), typ2)
  | Tarrow (typ1, typ2, explicit, arr_label)
    when match (label, arr_label) with
         | Labelled lbl, Optional arr_lbl ->
             String.equal lbl arr_lbl
         | _ ->
             false ->
      (Some (!mk_option typ1, explicit, arr_label), typ2)
  | Tarrow (typ1, typ2, explicit, arr_label) -> (
    match bubble_label_aux label typ2 with
    | None, _ ->
        (None, typ)
    | res, typ2 ->
        (res, mk type_depth (Tarrow (typ1, typ2, explicit, arr_label))) )
  | _ ->
      (None, typ)

let bubble_label label typ =
  let {type_depth; _} = typ in
  match bubble_label_aux label typ with
  | Some (typ1, explicit, arr_label), typ2 ->
      mk type_depth (Tarrow (typ1, typ2, explicit, arr_label))
  | None, typ ->
      typ

let discard_optional_labels typ =
  let rec go typ' =
    match typ'.type_desc with
    | Tarrow (_, typ2, _, Optional _) ->
        go typ2
    | Tarrow (_, _, _, _) ->
        typ
    | _ ->
        typ'
  in
  go typ

let is_arrow typ =
  match typ.type_desc with
  | Tarrow _ | Tpoly (_, {type_desc= Tarrow _; _}) ->
      true
  | _ ->
      false
