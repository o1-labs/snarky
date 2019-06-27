open Core_kernel
open Type0
open Ast_build

let rec type_desc ?loc = function
  | Tvar (None, explicit) ->
      Type.none ?loc ~explicit ()
  | Tvar (Some name, explicit) ->
      Type.var ?loc ~explicit name.txt
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
      Type.constr ?loc ~params ~implicits ident.txt
  | Tpoly (vars, var) ->
      Type.poly ?loc (List.map ~f:(type_expr ?loc) vars) (type_expr ?loc var)

and type_expr ?loc typ = type_desc ?loc typ.type_desc

let field_decl ?loc fld =
  Type_decl.Field.mk ?loc fld.fld_ident.txt (type_expr ?loc fld.fld_type)

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
  ctor_args ?loc ctor.ctor_ident.txt ctor.ctor_args
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
    decl.tdec_ident.txt decl.tdec_desc

let rec pattern_desc = function
  | Typedast.Tpat_any ->
      Parsetypes.PAny
  | Tpat_variable str ->
      PVariable str
  | Tpat_constraint (p, typ) ->
      PConstraint (pattern p, typ)
  | Tpat_tuple ps ->
      PTuple (List.map ~f:pattern ps)
  | Tpat_or (p1, p2) ->
      POr (pattern p1, pattern p2)
  | Tpat_int i ->
      PInt i
  | Tpat_record fields ->
      PRecord (List.map fields ~f:(fun (label, p) -> (label, pattern p)))
  | Tpat_ctor (name, arg) ->
      PCtor (name, Option.map ~f:pattern arg)

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
      Parsetypes.Apply
        ( expression e
        , List.map args ~f:(fun (label, e) -> (label, expression e)) )
  | Texp_variable name ->
      Variable name
  | Texp_literal i ->
      Literal (literal i)
  | Texp_fun (label, p, e, explicit) ->
      Fun (label, pattern p, expression e, explicit)
  | Texp_newtype (name, e) ->
      Newtype (name, expression e)
  | Texp_seq (e1, e2) ->
      Seq (expression e1, expression e2)
  | Texp_let (p, e1, e2) ->
      Let (pattern p, expression e1, expression e2)
  | Texp_constraint (e, typ) ->
      Constraint (expression e, typ)
  | Texp_tuple es ->
      Tuple (List.map ~f:expression es)
  | Texp_match (e, cases) ->
      Match
        ( expression e
        , List.map cases ~f:(fun (p, e) -> (pattern p, expression e)) )
  | Texp_field (e, path) ->
      Field (expression e, path)
  | Texp_record (fields, default) ->
      Record
        ( List.map fields ~f:(fun (label, e) -> (label, expression e))
        , Option.map ~f:expression default )
  | Texp_ctor (path, arg) ->
      Ctor (path, Option.map ~f:expression arg)
  | Texp_unifiable {expression= e; name; id} ->
      Unifiable {expression= Option.map ~f:expression e; name; id}
  | Texp_if (e1, e2, e3) ->
      If (expression e1, expression e2, Option.map ~f:expression e3)

and expression e =
  {Parsetypes.exp_desc= expression_desc e.Typedast.exp_desc; exp_loc= e.exp_loc}

let rec signature_desc = function
  | Typedast.Tsig_value (name, typ) ->
      Parsetypes.SValue (name, typ)
  | Tsig_instance (name, typ) ->
      SInstance (name, typ)
  | Tsig_type decl ->
      STypeDecl decl
  | Tsig_module (name, msig) ->
      SModule (name, module_sig msig)
  | Tsig_modtype (name, msig) ->
      SModType (name, module_sig msig)
  | Tsig_open path ->
      SOpen path
  | Tsig_typeext (typ, ctors) ->
      STypeExtension (typ, ctors)
  | Tsig_request (arg, ctor) ->
      SRequest (arg, ctor)
  | Tsig_multiple sigs ->
      SMultiple (List.map ~f:signature_item sigs)

and signature_item s =
  {Parsetypes.sig_desc= signature_desc s.Typedast.sig_desc; sig_loc= s.sig_loc}

and module_sig_desc = function
  | Typedast.Tmty_sig sigs ->
      Parsetypes.Signature (List.map ~f:signature_item sigs)
  | Tmty_name path ->
      SigName path
  | Tmty_abstract ->
      SigAbstract
  | Tmty_functor (name, fsig, msig) ->
      Parsetypes.SigFunctor (name, module_sig fsig, module_sig msig)

and module_sig msig =
  { Parsetypes.msig_desc= module_sig_desc msig.Typedast.msig_desc
  ; msig_loc= msig.msig_loc }

let rec statement_desc = function
  | Typedast.Tstmt_value (p, e) ->
      Parsetypes.Value (pattern p, expression e)
  | Tstmt_instance (name, e) ->
      Instance (name, expression e)
  | Tstmt_type decl ->
      TypeDecl decl
  | Tstmt_module (name, m) ->
      Module (name, module_expr m)
  | Tstmt_modtype (name, msig) ->
      ModType (name, module_sig msig)
  | Tstmt_open path ->
      Open path
  | Tstmt_typeext (typ, ctors) ->
      TypeExtension (typ, ctors)
  | Tstmt_request (arg, ctor, handler) ->
      Request
        ( arg
        , ctor
        , Option.map
            ~f:(fun (p, e) -> (Option.map ~f:pattern p, expression e))
            handler )
  | Tstmt_multiple stmts ->
      Multiple (List.map ~f:statement stmts)

and statement s =
  { Parsetypes.stmt_desc= statement_desc s.Typedast.stmt_desc
  ; stmt_loc= s.stmt_loc }

and module_desc = function
  | Typedast.Structure stmts ->
      Parsetypes.Structure (List.map ~f:statement stmts)
  | ModName path ->
      ModName path
  | Functor (name, fsig, m) ->
      Functor (name, module_sig fsig, module_expr m)

and module_expr m =
  {Parsetypes.mod_desc= module_desc m.Typedast.mod_desc; mod_loc= m.mod_loc}
