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
  | Typedast.PAny ->
      Parsetypes.PAny
  | PVariable str ->
      PVariable str
  | PConstraint (p, typ) ->
      PConstraint (pattern p, typ)
  | PTuple ps ->
      PTuple (List.map ~f:pattern ps)
  | POr (p1, p2) ->
      POr (pattern p1, pattern p2)
  | PInt i ->
      PInt i
  | PRecord fields ->
      PRecord (List.map fields ~f:(fun (label, p) -> (label, pattern p)))
  | PCtor (name, arg) ->
      PCtor (name, Option.map ~f:pattern arg)

and pattern p =
  {Parsetypes.pat_desc= pattern_desc p.Typedast.pat_desc; pat_loc= p.pat_loc}

let rec expression_desc = function
  | Typedast.Apply (e, args) ->
      Parsetypes.Apply
        ( expression e
        , List.map args ~f:(fun (label, e) -> (label, expression e)) )
  | Variable name ->
      Variable name
  | Int i ->
      Int i
  | Fun (label, p, e, explicit) ->
      Fun (label, pattern p, expression e, explicit)
  | Newtype (name, e) ->
      Newtype (name, expression e)
  | Seq (e1, e2) ->
      Seq (expression e1, expression e2)
  | Let (p, e1, e2) ->
      Let (pattern p, expression e1, expression e2)
  | Constraint (e, typ) ->
      Constraint (expression e, typ)
  | Tuple es ->
      Tuple (List.map ~f:expression es)
  | Match (e, cases) ->
      Match
        ( expression e
        , List.map cases ~f:(fun (p, e) -> (pattern p, expression e)) )
  | Field (e, path) ->
      Field (expression e, path)
  | Record (fields, default) ->
      Record
        ( List.map fields ~f:(fun (label, e) -> (label, expression e))
        , Option.map ~f:expression default )
  | Ctor (path, arg) ->
      Ctor (path, Option.map ~f:expression arg)
  | Unifiable {expression= e; name; id} ->
      Unifiable {expression= Option.map ~f:expression e; name; id}

and expression e =
  {Parsetypes.exp_desc= expression_desc e.Typedast.exp_desc; exp_loc= e.exp_loc}

let rec signature_desc = function
  | Typedast.SValue (name, typ) ->
      Parsetypes.SValue (name, typ)
  | SInstance (name, typ) ->
      SInstance (name, typ)
  | STypeDecl decl ->
      STypeDecl decl
  | SModule (name, msig) ->
      SModule (name, module_sig msig)
  | SModType (name, msig) ->
      SModType (name, module_sig msig)
  | SOpen path ->
      SOpen path
  | STypeExtension (typ, ctors) ->
      STypeExtension (typ, ctors)
  | SRequest (arg, ctor) ->
      SRequest (arg, ctor)
  | SMultiple sigs ->
      SMultiple (List.map ~f:signature_item sigs)

and signature_item s =
  {Parsetypes.sig_desc= signature_desc s.Typedast.sig_desc; sig_loc= s.sig_loc}

and module_sig_desc = function
  | Typedast.Signature sigs ->
      Parsetypes.Signature (List.map ~f:signature_item sigs)
  | SigName path ->
      SigName path
  | SigAbstract ->
      SigAbstract
  | SigFunctor (name, fsig, msig) ->
      Parsetypes.SigFunctor (name, module_sig fsig, module_sig msig)

and module_sig msig =
  { Parsetypes.msig_desc= module_sig_desc msig.Typedast.msig_desc
  ; msig_loc= msig.msig_loc }

let rec statement_desc = function
  | Typedast.Value (p, e) ->
      Parsetypes.Value (pattern p, expression e)
  | Instance (name, e) ->
      Instance (name, expression e)
  | TypeDecl decl ->
      TypeDecl decl
  | Module (name, m) ->
      Module (name, module_expr m)
  | ModType (name, msig) ->
      ModType (name, module_sig msig)
  | Open path ->
      Open path
  | TypeExtension (typ, ctors) ->
      TypeExtension (typ, ctors)
  | Request (arg, ctor, handler) ->
      Request
        ( arg
        , ctor
        , Option.map
            ~f:(fun (p, e) -> (Option.map ~f:pattern p, expression e))
            handler )
  | Multiple stmts ->
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
