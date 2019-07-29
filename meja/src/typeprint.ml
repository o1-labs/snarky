open Ast_types
open Type0
open Format
open Ast_print

let rec type_desc ?(bracket = false) fmt = function
  | Tvar (None, _) ->
      fprintf fmt "_"
  | Tvar (Some name, _) ->
      fprintf fmt "'%s" name
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

and tuple fmt typs =
  fprintf fmt "(@,%a@,)" (pp_print_list ~pp_sep:comma_sep type_expr) typs

and type_expr fmt typ = type_desc fmt typ.type_desc

and type_expr_b fmt typ = type_desc ~bracket:true fmt typ.type_desc

and variant fmt v =
  match v.var_params with
  | [] ->
      Longident.pp fmt v.var_ident
  | _ ->
      fprintf fmt "@[<hv2>%a%a@]" Longident.pp v.var_ident tuple v.var_params

let field_decl fmt decl =
  fprintf fmt "%a:@ @[<hv>%a@]" Ident.pprint decl.fld_ident type_expr
    decl.fld_type

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
  fprintf fmt "%a%a" Ident.pprint decl.ctor_ident ctor_args decl.ctor_args ;
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
      fprintf fmt "@ /*@[%a +=@ %a@]*/" Longident.pp name
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
  fprintf fmt "type %a" Ident.pprint decl.tdec_ident ;
  (match decl.tdec_params with [] -> () | _ -> tuple fmt decl.tdec_params) ;
  type_decl_desc fmt decl.tdec_desc
