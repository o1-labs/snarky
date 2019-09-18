open Core_kernel
open Ast_types
open Ast_build

let rec longident_of_path = function
  | Path.Pident ident ->
      Longident.Lident (Ident.name ident)
  | Pdot (path, _mode, name) ->
      Ldot (longident_of_path path, name)
  | Papply (path1, path2) ->
      Lapply (longident_of_path path1, longident_of_path path2)

module Type0 = struct
  open Type0

  let rec type_desc ?loc = function
    | Tvar None ->
        Type.none ?loc ()
    | Tvar (Some name) ->
        Type.var ?loc name
    | Ttuple typs ->
        Type.tuple ?loc (List.map ~f:(type_expr ?loc) typs)
    | Tarrow (typ1, typ2, explicit, label) ->
        Type.arrow ?loc ~explicit ~label (type_expr ?loc typ1)
          (type_expr ?loc typ2)
    | Tctor {var_ident= ident; var_params= params; var_decl= _} ->
        let params = List.map ~f:(type_expr ?loc) params in
        Type.constr ?loc ~params (longident_of_path ident)
    | Tpoly (vars, var) ->
        Type.poly ?loc (List.map ~f:(type_expr ?loc) vars) (type_expr ?loc var)
    | Tref typ ->
        type_expr ?loc (Type1.repr typ)

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

  let rec type_decl_desc ?loc ?params name = function
    | TAbstract ->
        Type_decl.abstract ?loc ?params name
    | TAlias typ ->
        Type_decl.alias ?loc ?params name (type_expr typ)
    | TRecord fields ->
        Type_decl.record ?loc ?params name (List.map ~f:field_decl fields)
    | TVariant ctors ->
        Type_decl.variant ?loc ?params name (List.map ~f:ctor_decl ctors)
    | TOpen ->
        Type_decl.open_ ?loc ?params name
    | TExtend _ ->
        failwith "Cannot convert TExtend to a parsetree equivalent"
    | TForward _ ->
        failwith "Cannot convert TForward to a parsetree equivalent"

  and type_decl ?loc decl =
    type_decl_desc ?loc
      ~params:(List.map ~f:type_expr decl.tdec_params)
      (Ident.name decl.tdec_ident)
      decl.tdec_desc
end

let rec type_desc = function
  | Typedast.Ttyp_var name ->
      Parsetypes.Ptyp_var name
  | Ttyp_tuple typs ->
      Ptyp_tuple (List.map ~f:type_expr typs)
  | Ttyp_arrow (typ1, typ2, explicit, label) ->
      Ptyp_arrow (type_expr typ1, type_expr typ2, explicit, label)
  | Ttyp_ctor var ->
      Ptyp_ctor (variant var)
  | Ttyp_poly (vars, var) ->
      Ptyp_poly (List.map ~f:type_expr vars, type_expr var)

and type_expr {type_desc= typ; type_loc; type_type= _} =
  {type_desc= type_desc typ; type_loc}

and variant {Typedast.var_ident; var_params} =
  { Parsetypes.var_ident= map_loc ~f:longident_of_path var_ident
  ; var_params= List.map ~f:type_expr var_params }

let field_decl {Typedast.fld_ident; fld_type; fld_loc; fld_fld= _} =
  { Parsetypes.fld_ident= map_loc ~f:Ident.name fld_ident
  ; fld_type= type_expr fld_type
  ; fld_loc }

let ctor_args = function
  | Typedast.Tctor_tuple typs ->
      Parsetypes.Ctor_tuple (List.map ~f:type_expr typs)
  | Tctor_record fld ->
      Ctor_record (List.map ~f:field_decl fld)

let ctor_decl
    {Typedast.ctor_ident; ctor_args= args; ctor_ret; ctor_loc; ctor_ctor= _} =
  { Parsetypes.ctor_ident= map_loc ~f:Ident.name ctor_ident
  ; ctor_args= ctor_args args
  ; ctor_ret= Option.map ~f:type_expr ctor_ret
  ; ctor_loc }

let type_decl_desc = function
  | Typedast.Tdec_abstract ->
      Parsetypes.Pdec_abstract
  | Tdec_alias typ ->
      Pdec_alias (type_expr typ)
  | Tdec_record fields ->
      Pdec_record (List.map ~f:field_decl fields)
  | Tdec_variant ctors ->
      Pdec_variant (List.map ~f:ctor_decl ctors)
  | Tdec_open ->
      Pdec_open
  | Tdec_extend (lid, typ, ctor) ->
      Pdec_extend (lid, typ, List.map ~f:ctor_decl ctor)

let type_decl
    {Typedast.tdec_ident; tdec_params; tdec_desc; tdec_loc; tdec_tdec= _} =
  { Parsetypes.tdec_ident= map_loc ~f:Ident.name tdec_ident
  ; tdec_params= List.map ~f:type_expr tdec_params
  ; tdec_desc= type_decl_desc tdec_desc
  ; tdec_loc }

let rec pattern_desc = function
  | Typedast.Tpat_any ->
      Parsetypes.Ppat_any
  | Tpat_variable str ->
      Ppat_variable (map_loc ~f:Ident.name str)
  | Tpat_constraint (p, typ) ->
      Ppat_constraint (pattern p, type_expr typ)
  | Tpat_tuple ps ->
      Ppat_tuple (List.map ~f:pattern ps)
  | Tpat_or (p1, p2) ->
      Ppat_or (pattern p1, pattern p2)
  | Tpat_int i ->
      Ppat_int i
  | Tpat_record fields ->
      Ppat_record
        (List.map fields ~f:(fun (label, p) ->
             (map_loc ~f:longident_of_path label, pattern p) ))
  | Tpat_ctor (name, arg) ->
      Ppat_ctor (map_loc ~f:longident_of_path name, Option.map ~f:pattern arg)

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
      Pexp_variable (map_loc ~f:longident_of_path name)
  | Texp_literal i ->
      Pexp_literal (literal i)
  | Texp_fun (label, p, e, explicit) ->
      Pexp_fun (label, pattern p, expression e, explicit)
  | Texp_newtype (name, e) ->
      Pexp_newtype (map_loc ~f:Ident.name name, expression e)
  | Texp_seq (e1, e2) ->
      Pexp_seq (expression e1, expression e2)
  | Texp_let (p, e1, e2) ->
      Pexp_let (pattern p, expression e1, expression e2)
  | Texp_constraint (e, typ) ->
      Pexp_constraint (expression e, type_expr typ)
  | Texp_tuple es ->
      Pexp_tuple (List.map ~f:expression es)
  | Texp_match (e, cases) ->
      Pexp_match
        ( expression e
        , List.map cases ~f:(fun (p, e) -> (pattern p, expression e)) )
  | Texp_field (e, path) ->
      Pexp_field (expression e, map_loc ~f:longident_of_path path)
  | Texp_record (fields, default) ->
      Pexp_record
        ( List.map fields ~f:(fun (label, e) ->
              (map_loc ~f:longident_of_path label, expression e) )
        , Option.map ~f:expression default )
  | Texp_ctor (path, arg) ->
      Pexp_ctor
        (map_loc ~f:longident_of_path path, Option.map ~f:expression arg)
  | Texp_unifiable {expression= e; name; id} ->
      Pexp_unifiable
        { expression= Option.map ~f:expression e
        ; name= map_loc ~f:Ident.name name
        ; id }
  | Texp_if (e1, e2, e3) ->
      Pexp_if (expression e1, expression e2, Option.map ~f:expression e3)
  | Texp_prover e ->
      Pexp_prover (expression e)

and expression e =
  {Parsetypes.exp_desc= expression_desc e.Typedast.exp_desc; exp_loc= e.exp_loc}

let rec signature_desc = function
  | Typedast.Tsig_value (name, typ) ->
      Parsetypes.Psig_value (map_loc ~f:Ident.name name, type_expr typ)
  | Tsig_instance (name, typ) ->
      Psig_instance (map_loc ~f:Ident.name name, type_expr typ)
  | Tsig_type decl ->
      Psig_type (type_decl decl)
  | Tsig_module (name, msig) ->
      Psig_module (map_loc ~f:Ident.name name, module_sig msig)
  | Tsig_modtype (name, msig) ->
      Psig_modtype (map_loc ~f:Ident.name name, module_sig msig)
  | Tsig_open path ->
      Psig_open (map_loc ~f:longident_of_path path)
  | Tsig_typeext (typ, ctors) ->
      Psig_typeext (variant typ, List.map ~f:ctor_decl ctors)
  | Tsig_request (arg, ctor) ->
      Psig_request (type_expr arg, ctor_decl ctor)
  | Tsig_multiple sigs ->
      Psig_multiple (List.map ~f:signature_item sigs)
  | Tsig_prover sigs ->
      Psig_prover (List.map ~f:signature_item sigs)

and signature_item s =
  {Parsetypes.sig_desc= signature_desc s.Typedast.sig_desc; sig_loc= s.sig_loc}

and module_sig_desc = function
  | Typedast.Tmty_sig sigs ->
      Parsetypes.Pmty_sig (List.map ~f:signature_item sigs)
  | Tmty_name path ->
      Pmty_name (map_loc ~f:longident_of_path path)
  | Tmty_alias path ->
      Pmty_alias (map_loc ~f:longident_of_path path)
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
      Pstmt_instance (map_loc ~f:Ident.name name, expression e)
  | Tstmt_type decl ->
      Pstmt_type (type_decl decl)
  | Tstmt_module (name, m) ->
      Pstmt_module (map_loc ~f:Ident.name name, module_expr m)
  | Tstmt_modtype (name, msig) ->
      Pstmt_modtype (map_loc ~f:Ident.name name, module_sig msig)
  | Tstmt_open path ->
      Pstmt_open (map_loc ~f:longident_of_path path)
  | Tstmt_typeext (typ, ctors) ->
      Pstmt_typeext (variant typ, List.map ~f:ctor_decl ctors)
  | Tstmt_request (arg, ctor, handler) ->
      Pstmt_request
        ( type_expr arg
        , ctor_decl ctor
        , Option.map
            ~f:(fun (p, e) -> (Option.map ~f:pattern p, expression e))
            handler )
  | Tstmt_multiple stmts ->
      Pstmt_multiple (List.map ~f:statement stmts)
  | Tstmt_prover stmts ->
      Pstmt_prover (List.map ~f:statement stmts)

and statement s =
  { Parsetypes.stmt_desc= statement_desc s.Typedast.stmt_desc
  ; stmt_loc= s.stmt_loc }

and module_desc = function
  | Typedast.Tmod_struct stmts ->
      Parsetypes.Pmod_struct (List.map ~f:statement stmts)
  | Tmod_name path ->
      Pmod_name (map_loc ~f:longident_of_path path)
  | Tmod_functor (name, fsig, m) ->
      Pmod_functor (name, module_sig fsig, module_expr m)

and module_expr m =
  {Parsetypes.mod_desc= module_desc m.Typedast.mod_desc; mod_loc= m.mod_loc}
