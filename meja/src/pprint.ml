open Parsetypes
open Format

let comma_sep fmt () = fprintf fmt ",@ "

let bar_sep fmt () = fprintf fmt "@ | "

let arg_label fmt = function
  | Asttypes.Nolabel -> ()
  | Labelled label -> fprintf fmt "%s:@," label
  | Optional label -> fprintf fmt "?%s@," label

let rec type_desc ?(bracket = false) fmt = function
  | Tvar (None, _, _) ->
      fprintf fmt "_"
  | Tvar (Some name, _, _) ->
      fprintf fmt "'%s" name.txt
  | Ttuple typs ->
      tuple fmt typs
  | Tarrow (typ1, typ2, implicitness, label) ->
      if bracket then fprintf fmt "(" ;
      ( match implicitness with
      | Explicit ->
          fprintf fmt "@[%a%a" arg_label label type_expr_b typ1
      | Implicit ->
          fprintf fmt "@[%a{%a}" arg_label label type_expr typ1 ) ;
      fprintf fmt "@ ->@ %a@]" type_expr typ2 ;
      if bracket then fprintf fmt ")"
  | Tctor v ->
      variant fmt v
  | Tpoly (vars, typ) ->
      if bracket then fprintf fmt "(" ;
      fprintf fmt "/*@[%a.@]*/@ %a" (type_desc ~bracket:false) (Ttuple vars)
        type_expr typ ;
      if bracket then fprintf fmt ")"

and tuple fmt typs =
  fprintf fmt "(@[%a@])" (pp_print_list ~pp_sep:comma_sep type_expr) typs

and type_expr fmt typ = type_desc fmt typ.type_desc

and type_expr_b fmt typ = type_desc ~bracket:true fmt typ.type_desc

and variant fmt v =
  match v.var_params with
  | [] ->
      Longident.pp fmt v.var_ident.txt
  | _ ->
      fprintf fmt "%a%a" Longident.pp v.var_ident.txt tuple v.var_params

let field_decl fmt decl =
  fprintf fmt "%s:@ @[%a@]" decl.fld_ident.txt type_expr decl.fld_type

let ctor_args fmt = function
  | Ctor_tuple [] ->
      ()
  | Ctor_tuple typs ->
      tuple fmt typs
  | Ctor_record (_, fields) ->
      fprintf fmt "{%a}" (pp_print_list ~pp_sep:comma_sep field_decl) fields

let ctor_decl fmt decl =
  fprintf fmt "%a%a" pp_name decl.ctor_ident.txt ctor_args decl.ctor_args ;
  match decl.ctor_ret with
  | Some typ ->
      fprintf fmt "@ :@ @[%a@]" type_expr typ
  | None ->
      ()

let type_decl_desc fmt = function
  | TAbstract ->
      ()
  | TAlias typ | TUnfold typ ->
      fprintf fmt "@ =@ %a" type_expr typ
  | TRecord fields ->
      fprintf fmt "@ =@ {%a}"
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

let rec signature_desc fmt = function
  | SValue (name, typ) ->
      fprintf fmt "@[let@ %a@ :@ @[%a;@]@]@;@;" pp_name name.txt type_expr typ
  | SInstance (name, typ) ->
      fprintf fmt "@[instance@ %a@ :@ @[%a@];@]@;@;" pp_name name.txt type_expr
        typ
  | STypeDecl decl ->
      fprintf fmt "@[%a;@]@;@;" type_decl decl
  | SModule (name, msig) ->
      let prefix fmt = fprintf fmt ":@ " in
      fprintf fmt "@[module@ %s@ %a;@]@;@;" name.txt (module_sig ~prefix) msig
  | SModType (name, msig) ->
      let prefix fmt = fprintf fmt "=@ " in
      fprintf fmt "@[module type@ %s@ %a;@]@;@;" name.txt (module_sig ~prefix)
        msig

and signature_item fmt sigi = signature_desc fmt sigi.sig_desc

and signature fmt sigs = List.iter (signature_item fmt) sigs

and module_sig_desc ~prefix fmt = function
  | Signature msig ->
      prefix fmt ;
      fprintf fmt "{@[<v2>@;%a@]}" signature msig
  | SigName name ->
      prefix fmt ; Longident.pp fmt name.txt
  | SigAbstract ->
      ()
  | SigFunctor (name, f, m) ->
      let pp = module_sig ~prefix:(fun _ -> ()) in
      fprintf fmt "/* @[functor@ (%s :@ %a)@ =>@ %a@] */" name.txt pp f pp m

and module_sig ~prefix fmt msig = module_sig_desc ~prefix fmt msig.msig_desc
