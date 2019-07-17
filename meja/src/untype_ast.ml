open Core_kernel
open Ast_types
open Type0
open Ast_build

let rec type_desc ?loc = function
  | Tvar (None, explicit) ->
      Type.none ?loc ~explicit ()
  | Tvar (Some name, explicit) ->
      Type.var ?loc ~explicit name
  | Ttuple typs ->
      Type.tuple ?loc (List.map ~f:(type_expr ?loc) typs)
  | Tarrow (typ1, typ2, explicit, label) ->
      Type.arrow ?loc ~explicit ~label (type_expr ?loc typ1)
        (type_expr ?loc typ2)
  | Tctor
      { var_ident= ident
      ; var_params= params
      ; var_implicit_params= implicits
      ; var_decl= _ } ->
      let params = List.map ~f:(type_expr ?loc) params in
      let implicits = List.map ~f:(type_expr ?loc) implicits in
      Type.constr ?loc ~params ~implicits ident
  | Tpoly (vars, var) ->
      Type.poly ?loc (List.map ~f:(type_expr ?loc) vars) (type_expr ?loc var)

and type_expr ?loc typ = type_desc ?loc typ.type_desc

let field_decl ?loc fld =
  Type_decl.Field.mk ?loc (Ident.name fld.fld_ident)
    (type_expr ?loc fld.fld_type)

let ctor_args ?loc ?ret name = function
  | Ctor_tuple typs ->
      Type_decl.Ctor.with_args ?loc ?ret name
        (List.map ~f:(type_expr ?loc) typs)
  | Ctor_record {tdec_desc= TRecord fields; _} ->
      Type_decl.Ctor.with_record ?loc ?ret name
        (List.map ~f:(field_decl ?loc) fields)
  | Ctor_record _ ->
      assert false

let ctor_decl ?loc ctor =
  ctor_args ?loc
    (Ident.name ctor.ctor_ident)
    ctor.ctor_args
    ?ret:(Option.map ~f:(type_expr ?loc) ctor.ctor_ret)

let rec type_decl_desc ?loc ?params ?implicits name = function
  | TAbstract ->
      Type_decl.abstract ?loc ?params ?implicits name
  | TAlias typ ->
      Type_decl.alias ?loc ?params ?implicits name (type_expr typ)
  | TUnfold typ ->
      Type_decl.unfold ?loc ?params ?implicits name (type_expr typ)
  | TRecord fields ->
      Type_decl.record ?loc ?params ?implicits name
        (List.map ~f:field_decl fields)
  | TVariant ctors ->
      Type_decl.variant ?loc ?params ?implicits name
        (List.map ~f:ctor_decl ctors)
  | TOpen ->
      Type_decl.open_ ?loc ?params ?implicits name
  | TExtend _ ->
      failwith "Cannot convert TExtend to a parsetree equivalent"
  | TForward _ ->
      Type_decl.forward ?loc ?params ?implicits name

and type_decl ?loc decl =
  type_decl_desc ?loc
    ~params:(List.map ~f:type_expr decl.tdec_params)
    ~implicits:(List.map ~f:type_expr decl.tdec_implicit_params)
    (Ident.name decl.tdec_ident)
    decl.tdec_desc

let rec pattern_desc = function
  | Typedast.Tpat_any ->
      Parsetypes.Ppat_any
  | Tpat_variable str ->
      Ppat_variable str
  | Tpat_constraint (p, typ) ->
      Ppat_constraint (pattern p, typ)
  | Tpat_tuple ps ->
      Ppat_tuple (List.map ~f:pattern ps)
  | Tpat_or (p1, p2) ->
      Ppat_or (pattern p1, pattern p2)
  | Tpat_int i ->
      Ppat_int i
  | Tpat_record fields ->
      Ppat_record (List.map fields ~f:(fun (label, p) -> (label, pattern p)))
  | Tpat_ctor (name, arg) ->
      Ppat_ctor (name, Option.map ~f:pattern arg)

and pattern p =
  {Parsetypes.pat_desc= pattern_desc p.Typedast.pat_desc; pat_loc= p.pat_loc}

let literal = function
  | Typedast.Int i ->
      Parsetypes.Int i
  | Bool b ->
      Bool b
  | Field f ->
      Field f
  | String s ->
      String s

let rec expression_desc = function
  | Typedast.Texp_apply (e, args) ->
      Parsetypes.Pexp_apply
        ( expression e
        , List.map args ~f:(fun (label, e) -> (label, expression e)) )
  | Texp_variable name ->
      Pexp_variable name
  | Texp_literal i ->
      Pexp_literal (literal i)
  | Texp_fun (label, p, e, explicit) ->
      Pexp_fun (label, pattern p, expression e, explicit)
  | Texp_newtype (name, e) ->
      Pexp_newtype (name, expression e)
  | Texp_seq (e1, e2) ->
      Pexp_seq (expression e1, expression e2)
  | Texp_let (p, e1, e2) ->
      Pexp_let (pattern p, expression e1, expression e2)
  | Texp_constraint (e, typ) ->
      Pexp_constraint (expression e, typ)
  | Texp_tuple es ->
      Pexp_tuple (List.map ~f:expression es)
  | Texp_match (e, cases) ->
      Pexp_match
        ( expression e
        , List.map cases ~f:(fun (p, e) -> (pattern p, expression e)) )
  | Texp_field (e, path) ->
      Pexp_field (expression e, path)
  | Texp_record (fields, default) ->
      Pexp_record
        ( List.map fields ~f:(fun (label, e) -> (label, expression e))
        , Option.map ~f:expression default )
  | Texp_ctor (path, arg) ->
      Pexp_ctor (path, Option.map ~f:expression arg)
  | Texp_unifiable {expression= e; name; id} ->
      Pexp_unifiable {expression= Option.map ~f:expression e; name; id}
  | Texp_if (e1, e2, e3) ->
      Pexp_if (expression e1, expression e2, Option.map ~f:expression e3)

and expression e =
  {Parsetypes.exp_desc= expression_desc e.Typedast.exp_desc; exp_loc= e.exp_loc}

let rec signature_desc = function
  | Typedast.Tsig_value (name, typ) ->
      Parsetypes.Psig_value (name, typ)
  | Tsig_instance (name, typ) ->
      Psig_instance (name, typ)
  | Tsig_type decl ->
      Psig_type decl
  | Tsig_module (name, msig) ->
      Psig_module (map_loc ~f:Ident.name name, module_sig msig)
  | Tsig_modtype (name, msig) ->
      Psig_modtype (map_loc ~f:Ident.name name, module_sig msig)
  | Tsig_open path ->
      Psig_open path
  | Tsig_typeext (typ, ctors) ->
      Psig_typeext (typ, ctors)
  | Tsig_request (arg, ctor) ->
      Psig_request (arg, ctor)
  | Tsig_multiple sigs ->
      Psig_multiple (List.map ~f:signature_item sigs)

and signature_item s =
  {Parsetypes.sig_desc= signature_desc s.Typedast.sig_desc; sig_loc= s.sig_loc}

and module_sig_desc = function
  | Typedast.Tmty_sig sigs ->
      Parsetypes.Pmty_sig (List.map ~f:signature_item sigs)
  | Tmty_name path ->
      Pmty_name path
  | Tmty_abstract ->
      Pmty_abstract
  | Tmty_functor (name, fsig, msig) ->
      Pmty_functor (name, module_sig fsig, module_sig msig)

and module_sig msig =
  { Parsetypes.msig_desc= module_sig_desc msig.Typedast.msig_desc
  ; msig_loc= msig.msig_loc }

let rec statement_desc = function
  | Typedast.Tstmt_value (p, e) ->
      Parsetypes.Pstmt_value (pattern p, expression e)
  | Tstmt_instance (name, e) ->
      Pstmt_instance (name, expression e)
  | Tstmt_type decl ->
      Pstmt_type decl
  | Tstmt_module (name, m) ->
      Pstmt_module (map_loc ~f:Ident.name name, module_expr m)
  | Tstmt_modtype (name, msig) ->
      Pstmt_modtype (map_loc ~f:Ident.name name, module_sig msig)
  | Tstmt_open path ->
      Pstmt_open path
  | Tstmt_typeext (typ, ctors) ->
      Pstmt_typeext (typ, ctors)
  | Tstmt_request (arg, ctor, handler) ->
      Pstmt_request
        ( arg
        , ctor
        , Option.map
            ~f:(fun (p, e) -> (Option.map ~f:pattern p, expression e))
            handler )
  | Tstmt_multiple stmts ->
      Pstmt_multiple (List.map ~f:statement stmts)

and statement s =
  { Parsetypes.stmt_desc= statement_desc s.Typedast.stmt_desc
  ; stmt_loc= s.stmt_loc }

and module_desc = function
  | Typedast.Tmod_struct stmts ->
      Parsetypes.Pmod_struct (List.map ~f:statement stmts)
  | Tmod_name path ->
      Pmod_name path
  | Tmod_functor (name, fsig, m) ->
      Pmod_functor (name, module_sig fsig, module_expr m)

and module_expr m =
  {Parsetypes.mod_desc= module_desc m.Typedast.mod_desc; mod_loc= m.mod_loc}
