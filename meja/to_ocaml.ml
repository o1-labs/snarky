open Core_kernel
open Asttypes
open Ast_helper
open Parsetypes

let mk_lid name = Location.mkloc (Longident.Lident name.txt) name.loc

let rec of_type_desc typ =
  match typ with
  | Tvar (None, _) -> Typ.any ()
  | Tvar (Some name, _) -> Typ.var name.txt
  | Tarrow (typ1, typ2) ->
      Typ.arrow Nolabel (of_type_expr typ1) (of_type_expr typ2)
  | Tconstr name -> Typ.constr (mk_lid name) []

and of_type_expr typ = of_type_desc typ.type_desc

let rec of_pattern_desc = function
  | PVariable str -> Pat.var str
  | PConstraint (p, typ) -> Pat.constraint_ (of_pattern p) (of_type_expr typ)

and of_pattern pat = of_pattern_desc pat.pat_desc

let rec of_expression_desc = function
  | Apply (f, es) ->
      Exp.apply (of_expression f)
        (List.map ~f:(fun x -> (Nolabel, of_expression x)) es)
  | Variable name -> Exp.ident (mk_lid name)
  | Int i -> Exp.constant (Const.int i)
  | Fun (p, body) -> Exp.fun_ Nolabel None (of_pattern p) (of_expression body)
  | Constraint (e, typ) -> Exp.constraint_ (of_expression e) (of_type_expr typ)
  | Seq (e1, e2) -> Exp.sequence (of_expression e1) (of_expression e2)
  | Let (p, e_rhs, e) ->
      Exp.let_ Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)

and of_expression exp = of_expression_desc exp.exp_desc

let of_statement_desc = function
  | Value (p, e) ->
      Str.value Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]

let of_statement stmt = of_statement_desc stmt.stmt_desc

let of_file = List.map ~f:of_statement
