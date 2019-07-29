(** Convert from the OCaml typed tree to a Meja parsetree.

    This code is heavily dependent on OCaml's internals, and a new copy of this
    file should be added for each supported version.

    NOTE: When modifying this file, ensure that corresponding changes are made
          the other of_ocaml_*.ml files, and test compilation with all
          supported OCaml versions.
*)
open Path

open Longident
open Core_kernel
open Meja_lib.Parsetypes
open Types
open Location

let rec longident_of_path = function
  | Pident ident ->
      Lident (Ident.name ident)
  | Pdot (path, ident, _) ->
      Ldot (longident_of_path path, ident)
  | Papply (path1, path2) ->
      Lapply (longident_of_path path1, longident_of_path path2)

let rec to_type_desc ~loc desc =
  let to_type_expr = to_type_expr ~loc in
  match desc with
  | Tvar x | Tunivar x ->
      Ptyp_var (Option.map ~f:(fun x -> mkloc x loc) x, Explicit)
  | Tarrow (label, typ1, typ2, _) ->
      Ptyp_arrow (to_type_expr typ1, to_type_expr typ2, Explicit, label)
  | Ttuple typs ->
      Ptyp_tuple (List.map ~f:to_type_expr typs)
  | Tconstr (path, params, _) ->
      let var_ident = mkloc (longident_of_path path) loc in
      Ptyp_ctor
        { var_ident
        ; var_params= List.map ~f:to_type_expr params
        ; var_implicit_params= [] }
  | Tlink typ | Tsubst typ ->
      (to_type_expr typ).type_desc
  | Tpoly (typ, typs) ->
      Ptyp_poly (List.map ~f:to_type_expr typs, to_type_expr typ)
  | Tpackage (path, _bound_names, typs) ->
      (* We don't have packaged module types implemented here, but we can treat
         them as if they were [Tctor]s; there is no overlap between valid paths
         to packages and valid paths to type constructors. *)
      let var_ident = mkloc (longident_of_path path) loc in
      Ptyp_ctor
        { var_ident
        ; var_params= List.map ~f:to_type_expr typs
        ; var_implicit_params= [] }
  | Tobject _ | Tfield _ | Tnil | Tvariant _ ->
      (* This type isn't supported here. For now, just replace it with a
         variable, so we can still manipulate values including it. *)
      Ptyp_var (None, Explicit)

and to_type_expr ~loc typ =
  {type_desc= to_type_desc ~loc typ.desc; type_loc= loc}

let to_field_decl {ld_id; ld_type; ld_loc; _} =
  { fld_ident= mkloc (Ident.name ld_id) ld_loc
  ; fld_type= to_type_expr ~loc:ld_loc ld_type
  ; fld_loc= ld_loc }

let to_ctor_args ~loc = function
  | Cstr_tuple typs ->
      Ctor_tuple (List.map ~f:(to_type_expr ~loc) typs)
  | Cstr_record labels ->
      Ctor_record (List.map ~f:to_field_decl labels)

let to_ctor_decl {cd_id; cd_args; cd_res; cd_loc; _} =
  { ctor_ident= mkloc (Ident.name cd_id) cd_loc
  ; ctor_args= to_ctor_args ~loc:cd_loc cd_args
  ; ctor_ret= Option.map cd_res ~f:(to_type_expr ~loc:cd_loc)
  ; ctor_loc= cd_loc }

let to_type_decl_desc decl =
  match (decl.type_manifest, decl.type_kind) with
  | Some typ, _ ->
      TAlias (to_type_expr ~loc:decl.type_loc typ)
  | None, Type_abstract ->
      TAbstract
  | None, Type_open ->
      TOpen
  | None, Type_record (labels, _) ->
      TRecord (List.map labels ~f:to_field_decl)
  | None, Type_variant ctors ->
      TVariant (List.map ctors ~f:to_ctor_decl)

let can_create_signature_item item =
  match item with Sig_typext _ | Sig_class _ -> false | _ -> true

let rec to_signature_item item =
  match item with
  | Sig_value (ident, {val_type; val_loc; _}) ->
      { sig_desc=
          Psig_value
            ( mkloc (Ident.name ident) val_loc
            , to_type_expr ~loc:val_loc val_type )
      ; sig_loc= val_loc }
  | Sig_type (ident, decl, _rec_status) ->
      (* TODO: handle rec_status *)
      let tdec_desc = to_type_decl_desc decl in
      { sig_desc=
          Psig_type
            { tdec_ident= mkloc (Ident.name ident) decl.type_loc
            ; tdec_params=
                List.map ~f:(to_type_expr ~loc:decl.type_loc) decl.type_params
            ; tdec_implicit_params= []
            ; tdec_desc
            ; tdec_loc= decl.type_loc }
      ; sig_loc= decl.type_loc }
  | Sig_module (ident, decl, _) ->
      { sig_desc=
          Psig_module
            ( mkloc (Ident.name ident) decl.md_loc
            , to_module_sig ~loc:decl.md_loc (Some decl.md_type) )
      ; sig_loc= decl.md_loc }
  | Sig_modtype (ident, decl) ->
      { sig_desc=
          Psig_modtype
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
  | None ->
      Pmty_abstract
  | Some (Mty_ident path | Mty_alias (_, path)) ->
      Pmty_name (mkloc (longident_of_path path) loc)
  | Some (Mty_signature signature) ->
      Pmty_sig (to_signature signature)
  | Some (Mty_functor (name, f, mty)) ->
      Pmty_functor
        ( mkloc (Ident.name name) loc
        , to_module_sig ~loc f
        , to_module_sig ~loc (Some mty) )

and to_module_sig ~loc decl =
  {msig_loc= loc; msig_desc= to_module_sig_desc ~loc decl}

(** Versioned utility function for the To_ocaml module. *)
let open_of_name name = name
