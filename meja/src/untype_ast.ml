open Core_kernel
open Type0
open Ast_build

let rec type_desc ?loc = function
  | Tvar (None, _, explicit) ->
      Type.none ?loc ~explicit ()
  | Tvar (Some name, _, explicit) ->
      Type.var ?loc ~explicit name.txt
  | Ttuple typs ->
      Type.tuple ?loc (List.map ~f:type_expr typs)
  | Tarrow (typ1, typ2, explicit, label) ->
      Type.arrow ?loc ~explicit ~label (type_expr typ1) (type_expr typ2)
  | Tctor
      { var_ident= ident
      ; var_params= params
      ; var_implicit_params= implicits
      ; var_decl_id= _ } ->
      let params = List.map ~f:type_expr params in
      let implicits = List.map ~f:type_expr implicits in
      Type.constr ?loc ~params ~implicits ident.txt
  | Tpoly (vars, var) ->
      Type.poly ?loc (List.map ~f:type_expr vars) (type_expr var)

and type_expr typ = type_desc ~loc:typ.type_loc typ.type_desc
