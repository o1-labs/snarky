open Core_kernel
open Asttypes
open Ast_helper
open Parsetypes

let mk_lid name = Location.mkloc (Longident.Lident name.txt) name.loc

let rec of_typ typ =
  match typ.type_desc with
  | Tvar (None, _) -> Typ.any ()
  | Tvar (Some name, _) -> Typ.var name.txt
  | Tarrow (typ1, typ2) -> Typ.arrow Nolabel (of_typ typ1) (of_typ typ2)
  | Tconstr name -> Typ.constr (mk_lid name) []

let rec of_pattern = function
  | PVariable str -> Pat.var str
  | PConstraint (p, typ) -> Pat.constraint_ (of_pattern p) (of_typ typ)

let rec of_expression = function
  | Apply (f, es) ->
      Exp.apply (of_expression f)
        (List.map ~f:(fun x -> (Nolabel, of_expression x)) es)
  | Variable name -> Exp.ident (mk_lid name)
  | Int i -> Exp.constant (Const.int i)
  | Fun (p, body) -> Exp.fun_ Nolabel None (of_pattern p) (of_expression body)
  | Constraint (e, typ) -> Exp.constraint_ (of_expression e) (of_typ typ)
  | Seq (e1, e2) -> Exp.sequence (of_expression e1) (of_expression e2)
  | Let (p, e_rhs, e) ->
      Exp.let_ Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)

let of_statement = function
  | Value (p, e) ->
      Str.value Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]

let of_file = List.map ~f:of_statement
