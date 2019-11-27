open Core_kernel
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
  | Trow {row_tags; row_closed; row_proxy= _} ->
      let needs_lower_bound =
        match row_closed with
        | Open ->
            fprintf fmt "[>@[<hv1>@," ; false
        | Closed ->
            let is_fixed =
              Map.for_all row_tags ~f:(function
                | _, (Present | Absent), _ ->
                    true
                | _, Maybe, _ ->
                    false )
            in
            if is_fixed then fprintf fmt "[@[<hv1>@,"
            else fprintf fmt "[<@[<hv1>@," ;
            not is_fixed
      in
      let is_first = ref true in
      Map.iteri row_tags ~f:(fun ~key:_ ~data:(path, pres, args) ->
          match pres with
          | Present | Maybe ->
              if !is_first then is_first := false else bar_sep fmt () ;
              if List.is_empty args then Path.pp fmt path
              else fprintf fmt "%a@[<hv1>%a@]" Path.pp path tuple args
          | Absent ->
              () ) ;
      ( if needs_lower_bound then
        let is_first = ref true in
        Map.iteri row_tags ~f:(fun ~key:_ ~data:(path, pres, _args) ->
            match pres with
            | Present ->
                if !is_first then (
                  is_first := false ;
                  fprintf fmt "@ > " )
                else bar_sep fmt () ;
                Path.pp fmt path
            | Maybe | Absent ->
                () ) ) ;
      fprintf fmt "@,@]]"

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
      fprintf fmt "@ =@ @[<hv>%a@]" type_expr typ
  | TRecord fields ->
      fprintf fmt "@ =@ {@[<hv2>%a@]}"
        (pp_print_list ~pp_sep:comma_sep field_decl)
        fields
  | TVariant ctors ->
      fprintf fmt "@ =@ %a" (pp_print_list ~pp_sep:bar_sep ctor_decl) ctors
  | TOpen ->
      fprintf fmt "@ =@ .."
  | TExtend (name, ctors) ->
      fprintf fmt "@ /*@[%a +=@ %a@]*/" Path.pp name
        (pp_print_list ~pp_sep:bar_sep ctor_decl)
        ctors

let type_decl ident fmt decl =
  fprintf fmt "type %a" Ident.pprint ident ;
  (match decl.tdec_params with [] -> () | _ -> tuple fmt decl.tdec_params) ;
  type_decl_desc fmt decl.tdec_desc
