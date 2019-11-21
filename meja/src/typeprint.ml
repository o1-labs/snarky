open Ast_types
open Type0
open Format
open Ast_print

let rec type_desc ~mode ?(bracket = false) fmt = function
  | Tvar None ->
      fprintf fmt "_"
  | Tvar (Some name) ->
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
      fprintf fmt "/*@[%a.@]*/@ %a"
        (type_desc ~mode ~bracket:false)
        (Ttuple vars) type_expr typ ;
      if bracket then fprintf fmt ")"
  | Tref typ ->
      let typ = Type1.repr typ in
      if bracket then type_expr_b fmt typ else type_expr fmt typ
  | Treplace _ ->
      assert false
  | Tconv typ ->
      let typ1 = Type1.get_mode Checked typ in
      let typ2 = Type1.get_mode Prover typ in
      if bracket then fprintf fmt "(" ;
      fprintf fmt "%a@ --> %a" type_expr_b typ1 type_expr typ2 ;
      if bracket then fprintf fmt ")"
  | Topaque typ -> (
    match mode with
    | Checked ->
        fprintf fmt "@[<hv2>opaque(@,%a@,)@]" type_expr typ
    | Prover ->
        type_expr fmt typ )
  | Tother_mode typ -> (
    match (mode, typ.type_mode) with
    | Checked, Prover ->
        fprintf fmt "@[<hv2>Prover{@,%a@,}@]" type_expr typ
    | _ ->
        type_expr fmt typ )

and tuple fmt typs =
  fprintf fmt "(@,%a@,)" (pp_print_list ~pp_sep:comma_sep type_expr) typs

and type_expr fmt typ = type_desc ~mode:typ.type_mode fmt typ.type_desc

and type_expr_b fmt typ =
  type_desc ~mode:typ.type_mode ~bracket:true fmt typ.type_desc

and variant fmt v =
  match v.var_params with
  | [] ->
      Path.pp fmt v.var_ident
  | _ ->
      fprintf fmt "@[<hv2>%a%a@]" Path.pp v.var_ident tuple v.var_params

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
  | TAlias typ ->
      fprintf fmt " =@ %a" type_expr typ
  | TRecord fields ->
      fprintf fmt " =@ {@[<hv2>%a@]}"
        (pp_print_list ~pp_sep:comma_sep field_decl)
        fields
  | TVariant ctors ->
      fprintf fmt " =@ %a"
        (pp_print_list ~pp_sep:bar_sep ctor_decl)
        ctors
  | TOpen ->
      fprintf fmt " =@ .."
  | TExtend (name, ctors) ->
      fprintf fmt "@ /*@[%a +=@ %a@]*/" Path.pp name
        (pp_print_list ~pp_sep:bar_sep ctor_decl)
        ctors

let type_decl type_keyword fmt (ident, decl) =
  fprintf fmt "@[<hov2>%s @[<hv>%a@," type_keyword Ident.pprint ident ;
  (match decl.tdec_params with [] -> () | _ -> tuple fmt decl.tdec_params) ;
  fprintf fmt "@]" ;
  type_decl_desc fmt decl.tdec_desc ;
  fprintf fmt "@]"

let conv_type fmt = function
  | Conv_with (ident, mode, decl) ->
      let str =
        match mode with Checked -> "with" | Prover -> "with prover"
      in
      type_decl str fmt (ident, decl)
  | Conv_to typ ->
      fprintf fmt "to @[<hv>%a@]" type_expr typ

let rec signature fmt sigs =
  fprintf fmt "@[<hv>" ;
  List.iter (signature_item fmt) sigs ;
  fprintf fmt "@]"

and signature_item fmt = function
  | Svalue (name, typ) ->
      fprintf fmt "@[<2>let@ %a@ :@ @[<hv>%a;@]@]@;@;" Ident.pprint name
        type_expr typ
  | Sinstance (name, typ) ->
      fprintf fmt "@[<2>instance@ %a@ :@ @[<hv>%a@];@]@;@;" Ident.pprint name
        type_expr typ
  | Stype (ident, decl) ->
      fprintf fmt "%a;@;@;" (type_decl "type") (ident, decl)
  | Sconvtype (name, decl, tconv, conv_name, _conv_type) ->
      fprintf fmt "@[<v>%a@;%a@ by %a@];@;@;"
        (type_decl "convertible type")
        (name, decl) conv_type tconv Ident.pprint conv_name
  | Srectype (decl :: decls) ->
      let print_and_decls =
        pp_print_list ~pp_sep:pp_print_cut (type_decl "and")
      in
      let space_if_not_empty fmt = function
        | [] ->
            ()
        | _ ->
            fprintf fmt "@;"
      in
      fprintf fmt "@[<v>%a%a%a;@]@;@;" (type_decl "type") decl
        space_if_not_empty decls print_and_decls decls
  | Srectype [] ->
      assert false
  | Smodule (name, msig) ->
      let prefix fmt = fprintf fmt ":@ " in
      fprintf fmt "@[<hov2>module@ %a@ %a;@]@;@;" Ident.pprint name
        (module_sig ~prefix) msig
  | Smodtype (name, msig) ->
      let prefix fmt = fprintf fmt "=@ " in
      fprintf fmt "@[<hov2>module type@ %a@ %a;@]@;@;" Ident.pprint name
        (module_sig ~prefix) msig
  | Stypeext (typ, ctors) ->
      fprintf fmt "@[<2>type %a +=@[<hv2>@ %a@]@]@;@;" variant typ
        (pp_print_list ~pp_sep:bar_sep ctor_decl)
        ctors
  | Srequest (typ, ctor) ->
      fprintf fmt "@[<2>request (%a)@[<hv2>@ %a@]@]@;@;" type_expr typ
        ctor_decl ctor
  | Sprover sigs ->
      fprintf fmt "@[<2>Prover {@,%a@,}@]@;@;" signature sigs

and module_sig ~prefix fmt = function
  | Msig msig ->
      prefix fmt ;
      fprintf fmt "{@[<1>@;%a@]}" signature msig
  | Mname name ->
      prefix fmt ; Path.pp fmt name
  | Malias name ->
      fprintf fmt "=@ " ; Path.pp fmt name
  | Mabstract ->
      ()
  | Mfunctor (name, f, m) ->
      let pp = module_sig ~prefix:(fun _ -> ()) in
      fprintf fmt "/* @[functor@ (%a :@ %a)@ =>@ %a@] */" Ident.pprint name pp
        f pp m
