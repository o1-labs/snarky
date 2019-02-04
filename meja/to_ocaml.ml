open Core_kernel
open Asttypes
open Ast_helper
open Parsetypes

let mk_lid name = Location.mkloc (Longident.Lident name.txt) name.loc

let of_pattern = function PVariable str -> Pat.var str

let of_typ = function
  | TAny -> Typ.any ()
  | TVariable name -> Typ.constr (mk_lid name) []

let rec of_expression = function
  | Apply (f, x) -> Exp.apply (of_expression f) [(Nolabel, of_expression x)]
  | Variable name -> Exp.ident (mk_lid name)
  | Int i -> Exp.constant (Const.int i)
  | Fun (args, typ, body) ->
      let rec wrap_args args body =
        match args with
        | [] -> body
        | (p, typ) :: args ->
            let pat =
              match typ with
              | Some typ -> Pat.constraint_ (of_pattern p) (of_typ typ)
              | None -> of_pattern p
            in
            Exp.fun_ Nolabel None pat (wrap_args args body)
      in
      let body =
        match typ with
        | Some typ -> Exp.constraint_ (of_expression body) (of_typ typ)
        | None -> of_expression body
      in
      wrap_args args body
  | Seq (e1, e2) -> Exp.sequence (of_expression e1) (of_expression e2)
  | Let (p, e_rhs, e) ->
      Exp.let_ Nonrecursive
        [Vb.mk (of_pattern p) (of_expression e_rhs)]
        (of_expression e)

let of_statement = function
  | Value (p, e) ->
      Str.value Nonrecursive [Vb.mk (of_pattern p) (of_expression e)]

let of_file = List.map ~f:of_statement
