open Core_kernel
open Ast_types
open Type0

let type_id = ref 0

let mk depth type_desc =
  incr type_id ;
  {type_desc; type_id= !type_id; type_depth= depth}

let mkvar ?(explicitness = Explicit) depth name =
  mk depth (Tvar (name, explicitness))

(** The representative of a type. This unfolds any [Tref] values that are
    present to get to the true underlying type.
*)
let rec repr typ = match typ.type_desc with Tref typ -> repr typ | _ -> typ

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
      print "(%a)" (print_list typ_debug_print) typs
  | Tref typ ->
      print "= " ;
      typ_debug_print fmt typ ) ;
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
  | Tref typ ->
      f init typ

let iter ~f = fold ~init:() ~f:(fun () -> f)

(** Make a copy of the [type_desc], using [f] as a recursor. Unfolds any
    references to make a copy of the representative's description.
*)
let rec copy_desc ~f = function
  | Tvar _ as typ ->
      typ
  | Ttuple typs ->
      Ttuple (List.map ~f typs)
  | Tarrow (typ1, typ2, explicitness, label) ->
      Tarrow (f typ1, f typ2, explicitness, label)
  | Tctor ({var_params; var_implicit_params; _} as variant) ->
      Tctor
        { variant with
          var_params= List.map ~f var_params
        ; var_implicit_params= List.map ~f var_implicit_params }
  | Tpoly (typs, typ) ->
      Tpoly (List.map ~f typs, f typ)
  | Tref typ ->
      copy_desc ~f typ.type_desc

let rec equal_at_depth ~depth typ1 typ2 =
  let typ1 = repr typ1 in
  let typ2 = repr typ2 in
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
  match (repr typ).type_desc with
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

let implicit_params typ =
  let rec implicit_params set typ =
    match typ.type_desc with
    | Tvar (_, Implicit) ->
        Set.add set typ
    | Tpoly (_, typ) ->
        implicit_params set typ
    | _ ->
        fold ~init:set typ ~f:implicit_params
  in
  implicit_params Typeset.empty typ

let rec constr_map ~f typ =
  let {type_depth; _} = typ in
  match (repr typ).type_desc with
  | Tvar _ ->
      repr typ
  | Tctor variant ->
      let var_params = List.map ~f:(constr_map ~f) variant.var_params in
      let var_implicit_params =
        List.map ~f:(constr_map ~f) variant.var_implicit_params
      in
      mk type_depth (f {variant with var_params; var_implicit_params})
  | _ ->
      mk type_depth (copy_desc ~f:(constr_map ~f) typ.type_desc)

let discard_optional_labels typ =
  let rec go typ' =
    let typ' = repr typ' in
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
  match (repr typ).type_desc with
  | Tarrow _ | Tpoly (_, {type_desc= Tarrow _; _}) ->
      true
  | _ ->
      false

(** Returns [true] if [typ] is a strict subtype of [in_]
    (i.e. excluding [typ == in_]), or [false] otherwise.
*)
let contains typ ~in_ =
  let typ = repr typ in
  let equal = phys_equal typ in
  let rec contains in_ =
    let in_ = repr in_ in
    match in_.type_desc with
    | Tvar _ ->
        false
    | Ttuple typs ->
        List.exists ~f:equal typs || List.exists ~f:contains typs
    | Tarrow (typ1, typ2, _explicit, _label) ->
        equal typ1 || equal typ2 || contains typ1 || contains typ2
    | Tctor variant ->
        List.exists ~f:equal variant.var_params
        || List.exists ~f:equal variant.var_implicit_params
        || List.exists ~f:contains variant.var_params
        || List.exists ~f:contains variant.var_implicit_params
    | Tpoly (typs, typ) ->
        List.exists ~f:equal typs || equal typ
        || List.exists ~f:contains typs
        || contains typ
    | Tref _ ->
        assert false
  in
  contains in_

module Decl = struct
  let decl_id = ref 0

  let typ_mk = mk

  let mk ~name ~params ?(implicit_params = []) desc =
    incr decl_id ;
    { tdec_ident= name
    ; tdec_params= params
    ; tdec_implicit_params= implicit_params
    ; tdec_desc= desc
    ; tdec_id= !decl_id }

  let mk_typ ~params ?ident depth decl =
    let ident = Option.value ident ~default:(Path.Pident decl.tdec_ident) in
    typ_mk depth
      (Tctor
         { var_ident= ident
         ; var_params= params
         ; var_implicit_params= []
         ; var_decl= decl })
end
