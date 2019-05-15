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

let field_decl fld =
  Type_decl.Field.mk ~loc:fld.fld_loc fld.fld_ident.txt
    (type_expr fld.fld_type)

let ctor_args ?loc ?ret name = function
  | Ctor_tuple typs ->
      Type_decl.Ctor.with_args ?loc ?ret name (List.map ~f:type_expr typs)
  | Ctor_record (_, fields) ->
      Type_decl.Ctor.with_record ?loc ?ret name (List.map ~f:field_decl fields)

let ctor_decl ctor =
  ctor_args ctor.ctor_ident.txt ctor.ctor_args ~loc:ctor.ctor_loc
    ?ret:(Option.map ~f:type_expr ctor.ctor_ret)

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

and type_decl decl =
  type_decl_desc ~loc:decl.tdec_loc
    ~params:(List.map ~f:type_expr decl.tdec_params)
    ~implicits:(List.map ~f:type_expr decl.tdec_implicit_params)
    decl.tdec_ident.txt decl.tdec_desc
