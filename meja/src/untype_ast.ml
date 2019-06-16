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
      ; var_length= _
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
