open Core_kernel
open Ast_types
open Type0
open Format
open Ast_print

let rec type_desc ?(bracket = false) fmt = function
  | Tvar (None, _) ->
      fprintf fmt "_"
  | Tvar (Some name, _) ->
      fprintf fmt "'%s" name.txt
  | Ttuple typs ->
      fprintf fmt "@[<1>%a@]" tuple typs
  | Tarrow (typ1, typ2, implicitness, label) ->
      if bracket then fprintf fmt "(" ;
      ( match implicitness with
      | Explicit ->
          fprintf fmt "%a%a" arg_label label type_expr_b typ1
      | Implicit ->
          fprintf fmt "%a{%a}" arg_label label type_expr typ1 ) ;
      arg_label_box_end fmt label ;
      fprintf fmt "@ -> %a" type_expr typ2 ;
      if bracket then fprintf fmt ")"
  | Tctor v ->
      variant fmt v
  | Tpoly (vars, typ) ->
      if bracket then fprintf fmt "(" ;
      fprintf fmt "/*@[%a.@]*/@ %a" (type_desc ~bracket:false) (Ttuple vars)
        type_expr typ ;
      if bracket then fprintf fmt ")"
  | Trow row ->
      row_expr fmt row

and tuple fmt typs =
  fprintf fmt "(@,%a@,)" (pp_print_list ~pp_sep:comma_sep type_expr) typs

and type_expr fmt typ = type_desc fmt typ.type_desc

and type_expr_b fmt typ = type_desc ~bracket:true fmt typ.type_desc

and variant fmt v =
  match v.var_params with
  | [] ->
      Longident.pp fmt v.var_ident.txt
  | _ ->
      fprintf fmt "@[<hv2>%a%a@]" Longident.pp v.var_ident.txt tuple
        v.var_params

and row_expr fmt row =
  let first = ref true in
  let pp_sep fmt () = if !first then first := false else fprintf fmt "@ | " in
  let base, diff =
    Map.partition_tf row.row_contents ~f:(function
      | Row_never ->
          false
      | _ ->
          true )
  in
  let has_maybe = ref false in
  let always =
    Map.filter base ~f:(function
      | Row_always ->
          true
      | _ ->
          has_maybe := true ;
          false )
  in
  fprintf fmt "[@[<hv2>" ;
  if
    row.row_closed = Closed
    && (!has_maybe || not (List.is_empty row.row_includes))
  then fprintf fmt "<@ "
  else fprintf fmt "@," ;
  pp_map ~pp_sep (fun fmt (lid, _) _ -> Longident.pp fmt lid) fmt base ;
  pp_sep fmt () ;
  pp_print_list ~pp_sep type_expr fmt row.row_includes ;
  if !has_maybe || not (List.is_empty row.row_includes) then (
    fprintf fmt ">@ " ;
    pp_map ~pp_sep (fun fmt (lid, _) _ -> Longident.pp fmt lid) fmt always ) ;
  if not (Map.is_empty diff) then (
    fprintf fmt "-@ " ;
    pp_map ~pp_sep (fun fmt (lid, _) _ -> Longident.pp fmt lid) fmt diff ) ;
  fprintf fmt "@]]"

let field_decl fmt decl =
  fprintf fmt "%s:@ @[<hv>%a@]" decl.fld_ident.txt type_expr decl.fld_type

let ctor_args fmt = function
  | Ctor_tuple [] ->
      ()
  | Ctor_tuple typs ->
      tuple fmt typs
  | Ctor_record {tdec_desc= TRecord fields; _} ->
      fprintf fmt "{@[<2>%a@]}"
        (pp_print_list ~pp_sep:comma_sep field_decl)
        fields
  | Ctor_record _ ->
      assert false

let ctor_decl fmt decl =
  fprintf fmt "%a%a" pp_name decl.ctor_ident.txt ctor_args decl.ctor_args ;
  match decl.ctor_ret with
  | Some typ ->
      fprintf fmt "@ :@ @[<hv>%a@]" type_expr typ
  | None ->
      ()

let type_decl_desc fmt = function
  | TAbstract ->
      ()
  | TAlias typ | TUnfold typ ->
      fprintf fmt "@ =@ @[<hv>%a@]" type_expr typ
  | TRecord fields ->
      fprintf fmt "@ =@ {@[<hv2>%a@]}"
        (pp_print_list ~pp_sep:comma_sep field_decl)
        fields
  | TVariant ctors ->
      fprintf fmt "@ =@ %a" (pp_print_list ~pp_sep:bar_sep ctor_decl) ctors
  | TOpen ->
      fprintf fmt "@ =@ .."
  | TExtend (name, _, ctors) ->
      fprintf fmt "@ /*@[%a +=@ %a@]*/" Longident.pp name.txt
        (pp_print_list ~pp_sep:bar_sep ctor_decl)
        ctors
  | TForward i ->
      let print_id fmt i =
        match i with
        | Some i ->
            pp_print_int fmt i
        | None ->
            pp_print_char fmt '?'
      in
      fprintf fmt "@ /* forward declaration %a */" print_id !i

let type_decl fmt decl =
  fprintf fmt "type %s" decl.tdec_ident.txt ;
  (match decl.tdec_params with [] -> () | _ -> tuple fmt decl.tdec_params) ;
  type_decl_desc fmt decl.tdec_desc
