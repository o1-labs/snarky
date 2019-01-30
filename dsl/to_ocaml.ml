open Core_kernel
open Asttypes
open Ast_helper
open Parsetypes

let mk_lid name = Location.mkloc (Longident.Lident name.txt) name.loc

let rec of_typ typ =
  let loc = typ.type_loc in
  match typ.type_desc with
  | Tpoly (var, typ) ->
      var.in_recursion <- true ;
      let typ' = of_typ typ in
      var.in_recursion <- false ;
      typ'
  | Tvar {instance= Some typ; _} when not typ.in_recursion ->
      typ.in_recursion <- true ;
      let typ' = of_typ typ in
      typ.in_recursion <- false ;
      typ'
  | Tvar {name= None; _} ->
      (*Typ.any ~loc ()*) Typ.var ~loc (sprintf "_a%d" typ.id)
  | Tvar {name= Some name; _} -> Typ.var ~loc name.txt
  | Tarrow (typ1, typ2) -> Typ.arrow ~loc Nolabel (of_typ typ1) (of_typ typ2)
  | Tconstr {constr_ident; _} -> Typ.constr ~loc (mk_lid constr_ident) []
  | Ttuple typs -> Typ.tuple ~loc (List.map ~f:of_typ typs)
  | Tdefer typ -> of_typ typ

let of_type_decl name typ_dec =
  let open Ast_helper in
  let loc = typ_dec.type_decl_loc in
  match typ_dec.type_decl_desc with
  | Abstract -> Type.mk name ~loc
  | Alias typ -> Type.mk name ~loc ~manifest:(of_typ typ)
  | Record fields ->
    Type.mk name ~loc ~kind:(Parsetree.Ptype_record (
      List.map fields ~f:(fun decl ->
        Type.field ~loc:decl.field_loc decl.field_ident (of_typ decl.field_type))))

let rec of_pattern pat =
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | PVariable str -> Pat.var ~loc str
  | PConstraint {pcon_pat= p; pcon_typ= typ} ->
      Pat.constraint_ ~loc (of_pattern p) (of_typ typ)

let rec of_expression exp =
  let loc = exp.exp_loc in
  match exp.exp_desc with
  | Apply (f, xs) ->
      let xs = List.map xs ~f:(fun x -> (Nolabel, of_expression x)) in
      Exp.apply ~loc (of_expression f) xs
  | Variable name -> Exp.ident ~loc (mk_lid name)
  | Int i -> Exp.constant ~loc (Const.int i)
  | Fun (p, body) ->
      Exp.fun_ ~loc Nolabel None (of_pattern p) (of_expression body)
  | Constraint {econ_exp= e; econ_typ= typ} ->
      Exp.constraint_ ~loc (of_expression e) (of_typ typ)
  | Seq (e1, e2) -> Exp.sequence ~loc (of_expression e1) (of_expression e2)
  | Let (p, e_rhs, e) ->
      Exp.let_ ~loc Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)
  | Tuple es -> Exp.tuple ~loc (List.map ~f:of_expression es)

let of_statement stmt =
  let loc = stmt.stmt_loc in
  match stmt.stmt_desc with
  | Value (p, e) ->
    Str.value ~loc Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]
  | Type (name, typ_decl) ->
    Str.type_ ~loc Nonrecursive [of_type_decl name typ_decl]

let of_file = List.map ~f:of_statement
