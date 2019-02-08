open Core_kernel
open Asttypes
open Ast_helper
open Parsetypes

let mk_lid name = Location.mkloc (Longident.Lident name.txt) name.loc

let rec of_type_desc ?loc typ =
  match typ with
  | Tvar (None, _) -> Typ.any ?loc ()
  | Tvar (Some name, _) -> Typ.var ?loc name.txt
  | Tpoly (_, typ) -> of_type_expr typ
  | Tarrow (typ1, typ2) ->
      Typ.arrow ?loc Nolabel (of_type_expr typ1) (of_type_expr typ2)
  | Tctor {var_ident= name; var_params= params; _} ->
      Typ.constr ?loc (mk_lid name) (List.map ~f:of_type_expr params)
  | Ttuple typs -> Typ.tuple ?loc (List.map ~f:of_type_expr typs)

and of_type_expr typ = of_type_desc ~loc:typ.type_loc typ.type_desc

let of_field_decl {fld_ident= name; fld_type= typ; fld_loc= loc; _} =
  Type.field ~loc name (of_type_expr typ)

let of_type_decl decl =
  let loc = decl.tdec_loc in
  let name = decl.tdec_ident in
  let params =
    List.map ~f:(fun t -> (of_type_expr t, Invariant)) decl.tdec_params
  in
  match decl.tdec_desc with
  | TAbstract -> Type.mk name ~loc ~params
  | TAlias typ -> Type.mk name ~loc ~params ~manifest:(of_type_expr typ)
  | TRecord fields ->
      Type.mk name ~loc ~params
        ~kind:(Parsetree.Ptype_record (List.map ~f:of_field_decl fields))

let rec of_pattern_desc ?loc = function
  | PVariable str -> Pat.var ?loc str
  | PConstraint (p, typ) ->
      Pat.constraint_ ?loc (of_pattern p) (of_type_expr typ)
  | PTuple ps -> Pat.tuple ?loc (List.map ~f:of_pattern ps)

and of_pattern pat = of_pattern_desc ~loc:pat.pat_loc pat.pat_desc

let rec of_expression_desc ?loc = function
  | Apply (f, es) ->
      Exp.apply ?loc (of_expression f)
        (List.map ~f:(fun x -> (Nolabel, of_expression x)) es)
  | Variable name -> Exp.ident ?loc (mk_lid name)
  | Int i -> Exp.constant ?loc (Const.int i)
  | Fun (p, body) ->
      Exp.fun_ ?loc Nolabel None (of_pattern p) (of_expression body)
  | Constraint (e, typ) ->
      Exp.constraint_ ?loc (of_expression e) (of_type_expr typ)
  | Seq (e1, e2) -> Exp.sequence ?loc (of_expression e1) (of_expression e2)
  | Let (p, e_rhs, e) ->
      Exp.let_ ?loc Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)
  | Tuple es -> Exp.tuple ?loc (List.map ~f:of_expression es)

and of_expression exp = of_expression_desc ~loc:exp.exp_loc exp.exp_desc

let of_statement_desc ?loc = function
  | Value (p, e) ->
      Str.value ?loc Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]
  | TypeDecl decl -> Str.type_ ?loc Recursive [of_type_decl decl]

let of_statement stmt = of_statement_desc ~loc:stmt.stmt_loc stmt.stmt_desc

let of_file = List.map ~f:of_statement
