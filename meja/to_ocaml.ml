open Core_kernel
open Asttypes
open Ast_helper
open Parsetypes

let rec of_type_desc ?loc typ =
  match typ with
  | Tvar (None, _, _) -> Typ.any ?loc ()
  | Tvar (Some name, _, _) -> Typ.var ?loc name.txt
  | Tpoly (_, typ) -> of_type_expr typ
  | Tarrow (typ1, typ2, _) ->
      Typ.arrow ?loc Nolabel (of_type_expr typ1) (of_type_expr typ2)
  | Tctor
      {var_ident= name; var_params= params; var_implicit_params= implicits; _}
    ->
      Typ.constr ?loc name (List.map ~f:of_type_expr (params @ implicits))
  | Ttuple typs -> Typ.tuple ?loc (List.map ~f:of_type_expr typs)

and of_type_expr typ = of_type_desc ~loc:typ.type_loc typ.type_desc

let of_field_decl {fld_ident= name; fld_type= typ; fld_loc= loc; _} =
  Type.field ~loc name (of_type_expr typ)

let of_ctor_args = function
  | Ctor_tuple args -> Parsetree.Pcstr_tuple (List.map ~f:of_type_expr args)
  | Ctor_record (_, fields) ->
      Parsetree.Pcstr_record (List.map ~f:of_field_decl fields)

let of_ctor_decl
    {ctor_ident= name; ctor_args= args; ctor_ret= ret; ctor_loc= loc} =
  Type.constructor name ~loc ~args:(of_ctor_args args)
    ?res:(Option.map ~f:of_type_expr ret)

let of_type_decl decl =
  let loc = decl.tdec_loc in
  let name = decl.tdec_ident in
  let params =
    List.map
      ~f:(fun t -> (of_type_expr t, Invariant))
      (decl.tdec_params @ decl.tdec_implicit_params)
  in
  match decl.tdec_desc with
  | TAbstract -> Type.mk name ~loc ~params
  | TAlias typ -> Type.mk name ~loc ~params ~manifest:(of_type_expr typ)
  | TRecord fields ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_record (List.map ~f:of_field_decl fields))
  | TVariant ctors ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_variant (List.map ~f:of_ctor_decl ctors))
  | TOpen -> Type.mk name ~loc ~params ~kind:Parsetree.Ptype_open
  | TExtend _ -> failwith "Cannot convert TExtend to OCaml"
  | TUnfold _ -> failwith "Cannot convert TUnfold to OCaml"
  | TForward _ -> failwith "Cannot convert TForward to OCaml"

let rec of_pattern_desc ?loc = function
  | PAny -> Pat.any ?loc ()
  | PVariable str -> Pat.var ?loc str
  | PConstraint (p, typ) ->
      Pat.constraint_ ?loc (of_pattern p) (of_type_expr typ)
  | PTuple ps -> Pat.tuple ?loc (List.map ~f:of_pattern ps)
  | POr (p1, p2) -> Pat.or_ ?loc (of_pattern p1) (of_pattern p2)
  | PInt i -> Pat.constant ?loc (Const.int i)
  | PRecord fields ->
      Pat.record ?loc
        (List.map fields ~f:(fun (f, p) -> (f, of_pattern p)))
        Open
  | PCtor (name, arg) -> Pat.construct ?loc name (Option.map ~f:of_pattern arg)

and of_pattern pat = of_pattern_desc ~loc:pat.pat_loc pat.pat_desc

let rec of_expression_desc ?loc = function
  | Apply (f, es) ->
      Exp.apply ?loc (of_expression f)
        (List.map ~f:(fun x -> (Nolabel, of_expression x)) es)
  | Variable name -> Exp.ident ?loc name
  | Int i -> Exp.constant ?loc (Const.int i)
  | Fun (p, body, _) ->
      Exp.fun_ ?loc Nolabel None (of_pattern p) (of_expression body)
  | Constraint (e, typ) ->
      Exp.constraint_ ?loc (of_expression e) (of_type_expr typ)
  | Seq (e1, e2) -> Exp.sequence ?loc (of_expression e1) (of_expression e2)
  | Let (p, e_rhs, e) ->
      Exp.let_ ?loc Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)
  | Tuple es -> Exp.tuple ?loc (List.map ~f:of_expression es)
  | Match (e, cases) ->
      Exp.match_ ?loc (of_expression e)
        (List.map cases ~f:(fun (p, e) ->
             Exp.case (of_pattern p) (of_expression e) ))
  | Field (e, field) -> Exp.field ?loc (of_expression e) field
  | Record (fields, ext) ->
      Exp.record ?loc
        (List.map fields ~f:(fun (f, e) -> (f, of_expression e)))
        (Option.map ~f:of_expression ext)
  | Ctor (name, arg) ->
      Exp.construct ?loc name (Option.map ~f:of_expression arg)
  | Unifiable {expression= Some e; _} -> of_expression e
  | Unifiable {name; _} -> Exp.ident ?loc (mk_lid name)

and of_expression exp = of_expression_desc ~loc:exp.exp_loc exp.exp_desc

let rec of_statement_desc ?loc = function
  | Value (p, e) ->
      Str.value ?loc Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]
  | Instance (name, e) ->
      Str.value ?loc Nonrecursive [Vb.mk (Pat.var ?loc name) (of_expression e)]
  | TypeDecl decl -> Str.type_ ?loc Recursive [of_type_decl decl]
  | Module (name, m) -> Str.module_ ?loc (Mb.mk ?loc name (of_module_expr m))
  | Open name -> Str.open_ ?loc (Opn.mk ?loc name)
  | TypeExtension (variant, ctors) ->
      let params =
        List.map variant.var_params ~f:(fun typ -> (of_type_expr typ, Invariant)
        )
      in
      let ctors =
        List.map ctors
          ~f:(fun { ctor_ident= name
                  ; ctor_args= args
                  ; ctor_ret= ret
                  ; ctor_loc= loc }
             ->
            Te.decl ~loc ~args:(of_ctor_args args) name
              ?res:(Option.map ~f:of_type_expr ret) )
      in
      Str.type_extension ?loc (Te.mk ~params variant.var_ident ctors)

and of_statement stmt = of_statement_desc ~loc:stmt.stmt_loc stmt.stmt_desc

and of_module_expr m = of_module_desc ~loc:m.mod_loc m.mod_desc

and of_module_desc ?loc = function
  | Structure stmts -> Mod.structure ?loc (List.map ~f:of_statement stmts)
  | ModName name -> Mod.ident ?loc name

let of_file = List.map ~f:of_statement
