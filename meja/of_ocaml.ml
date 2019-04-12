open Path
open Longident
open Core_kernel
open Parsetypes
open Types
open Location

let rec longident_of_path = function
  | Pident ident -> Lident (Ident.name ident)
  | Pdot (path, ident, _) -> Ldot (longident_of_path path, ident)
  | Papply (path1, path2) ->
      Lapply (longident_of_path path1, longident_of_path path2)

let rec to_type_desc ~loc desc =
  let to_type_expr = to_type_expr ~loc in
  match desc with
  | Tvar x | Tunivar x ->
      Parsetypes.Tvar (Option.map ~f:(fun x -> mkloc x loc) x, -1, Explicit)
  | Tarrow ((Nolabel | Labelled _), typ1, typ2, _) ->
      Parsetypes.Tarrow (to_type_expr typ1, to_type_expr typ2, Explicit)
  | Tarrow (Optional _lbl, _typ1, typ2, _) ->
      (* TODO: Don't ignore optional arguments. *)
      (to_type_expr typ2).type_desc
  | Ttuple typs -> Parsetypes.Ttuple (List.map ~f:to_type_expr typs)
  | Tconstr (path, params, _) ->
      let var_ident = mkloc (longident_of_path path) loc in
      Parsetypes.Tctor
        { var_ident
        ; var_params= List.map ~f:to_type_expr params
        ; var_implicit_params= []
        ; var_decl_id= 0 }
  | Tlink typ | Tsubst typ -> (to_type_expr typ).type_desc
  | Tpoly (typ, typs) ->
      Parsetypes.Tpoly (List.map ~f:to_type_expr typs, to_type_expr typ)
  | Tpackage (path, _bound_names, typs) ->
      (* We don't have packaged module types implemented here, but we can treat
         them as if they were [Tctor]s; there is no overlap between valid paths
         to packages and valid paths to type constructors. *)
      let var_ident = mkloc (longident_of_path path) loc in
      Parsetypes.Tctor
        { var_ident
        ; var_params= List.map ~f:to_type_expr typs
        ; var_implicit_params= []
        ; var_decl_id= 0 }
  | Tobject _ | Tfield _ | Tnil | Tvariant _ ->
      (* This type isn't supported here. For now, just replace it with a
         variable, so we can still manipulate values including it. *)
      Parsetypes.Tvar (None, -1, Explicit)

and to_type_expr ~loc typ =
  {type_desc= to_type_desc ~loc typ.desc; type_id= -1; type_loc= loc}

let to_field_decl {ld_id; ld_type; ld_loc; _} =
  { fld_ident= mkloc (Ident.name ld_id) ld_loc
  ; fld_type= to_type_expr ~loc:ld_loc ld_type
  ; fld_id= 0
  ; fld_loc= ld_loc }

let to_ctor_args ~loc = function
  | Cstr_tuple typs -> Ctor_tuple (List.map ~f:(to_type_expr ~loc) typs)
  | Cstr_record labels -> Ctor_record (0, List.map ~f:to_field_decl labels)

let to_ctor_decl {cd_id; cd_args; cd_res; cd_loc; _} =
  { ctor_ident= mkloc (Ident.name cd_id) cd_loc
  ; ctor_args= to_ctor_args ~loc:cd_loc cd_args
  ; ctor_ret= Option.map cd_res ~f:(to_type_expr ~loc:cd_loc)
  ; ctor_loc= cd_loc }

let to_type_decl_desc decl =
  match (decl.type_manifest, decl.type_kind) with
  | Some typ, _ -> TAlias (to_type_expr ~loc:decl.type_loc typ)
  | None, (Type_abstract | Type_open) -> TAbstract
  | None, Type_record (labels, _) -> TRecord (List.map labels ~f:to_field_decl)
  | None, Type_variant ctors -> TVariant (List.map ctors ~f:to_ctor_decl)

let can_create_signature_item item =
  match item with Sig_typext _ | Sig_class _ -> false | _ -> true

let rec to_signature_item item =
  match item with
  | Sig_value (ident, {val_type; val_loc; _}) ->
      { sig_desc=
          SValue
            ( mkloc (Ident.name ident) val_loc
            , to_type_expr ~loc:val_loc val_type )
      ; sig_loc= val_loc }
  | Sig_type (ident, decl, _rec_status) ->
      (* TODO: handle rec_status *)
      let tdec_desc = to_type_decl_desc decl in
      { sig_desc=
          STypeDecl
            { tdec_ident= mkloc (Ident.name ident) decl.type_loc
            ; tdec_params=
                List.map ~f:(to_type_expr ~loc:decl.type_loc) decl.type_params
            ; tdec_implicit_params= []
            ; tdec_desc
            ; tdec_id= 0
            ; tdec_loc= decl.type_loc }
      ; sig_loc= decl.type_loc }
  | Sig_module (ident, decl, _) ->
      { sig_desc=
          SModule
            ( mkloc (Ident.name ident) decl.md_loc
            , to_module_sig ~loc:decl.md_loc (Some decl.md_type) )
      ; sig_loc= decl.md_loc }
  | Sig_modtype (ident, decl) ->
      { sig_desc=
          SModType
            ( mkloc (Ident.name ident) decl.mtd_loc
            , to_module_sig ~loc:decl.mtd_loc decl.mtd_type )
      ; sig_loc= decl.mtd_loc }
  | _ ->
      failwith "Cannot create a signature item from this OCaml signature item."

and to_signature items =
  List.filter_map items ~f:(fun item ->
      if can_create_signature_item item then Some (to_signature_item item)
      else None )

and to_module_sig_desc ~loc decl =
  match decl with
  | None -> SigAbstract
  | Some (Mty_ident path | Mty_alias (_, path)) ->
      SigName (mkloc (longident_of_path path) loc)
  | Some (Mty_signature signature) -> Signature (to_signature signature)
  | Some (Mty_functor (name, f, mty)) ->
      SigFunctor
        ( mkloc (Ident.name name) loc
        , to_module_sig ~loc f
        , to_module_sig ~loc (Some mty) )

and to_module_sig ~loc decl =
  {msig_loc= loc; msig_desc= to_module_sig_desc ~loc decl}
