open Core_kernel
open Type0

type mapper =
  { type_expr: mapper -> type_expr -> type_expr
  ; type_desc: mapper -> type_desc -> type_desc
  ; variant: mapper -> variant -> variant
  ; field_decl: mapper -> field_decl -> field_decl
  ; ctor_args: mapper -> ctor_args -> ctor_args
  ; ctor_decl: mapper -> ctor_decl -> ctor_decl
  ; type_decl: mapper -> type_decl -> type_decl
  ; type_decl_desc: mapper -> type_decl_desc -> type_decl_desc
  ; conv_type: mapper -> conv_type -> conv_type
  ; signature_item: mapper -> signature_item -> signature_item
  ; signature: mapper -> signature -> signature
  ; module_sig: mapper -> module_sig -> module_sig
  ; ident: mapper -> Ident.t -> Ident.t
  ; path: mapper -> Path.t -> Path.t }

let map_list ~same ~f =
  List.map ~f:(fun x ->
      let y = f x in
      if not (phys_equal x y) then same := false ;
      y )

(** The default mapper (and the functions that it contains) are careful to
    preserve equality unless some subvalue changes, to ensure that we don't
    perform unnecessary allocations/GCs during mapping.
    This also makes it much less likely that we might replace a variant which
    we planned to mutate later.

    CAUTION:
      In order to break recursion, the default [type_expr] mapper replaces the
      [type_desc] with [Treplace typ], where [typ] is its return value. Any
      partial-override of this function should ensure that this behaviour is
      kept using [Type1.set_replacement].

      To restore the previous values, a snapshot should be taken before with
      [Type1.Snapshot.create], and then a call to [Type1.backtrack_replace]
      with the snapshot will undo the [Treplace] changes.

      DO NOT recurse into [Treplace] values: they will often be self-recursive.
*)

let type_expr mapper typ =
  match typ.Type0.type_desc with
  | Treplace typ ->
      (* Recursion breaking. *)
      typ
  | desc ->
      let alt_desc = typ.type_alternate.type_desc in
      let alt_alt_desc = typ.type_alternate.type_alternate.type_desc in
      let typ' = Type1.mkvar ~mode:typ.type_mode typ.type_depth None in
      (* Initialise [typ'] as its own recursion-breaking value. *)
      typ'.type_desc <- Treplace typ' ;
      typ'.type_alternate.type_desc <- Treplace typ'.type_alternate ;
      let is_stitched = phys_equal typ typ.type_alternate.type_alternate in
      if is_stitched then
        (* Change from tri-stitching to plain stitching. *)
        typ'.type_alternate.type_alternate <- typ'
      else
        typ'.type_alternate.type_alternate.type_desc
        <- Treplace typ'.type_alternate.type_alternate ;
      Type1.set_replacement typ typ' ;
      let type_desc = mapper.type_desc mapper desc in
      let alt_type_desc = mapper.type_desc mapper alt_desc in
      let alt_alt_type_desc =
        if is_stitched then type_desc else mapper.type_desc mapper alt_alt_desc
      in
      if
        phys_equal type_desc desc
        && phys_equal alt_type_desc alt_desc
        && phys_equal alt_alt_type_desc alt_alt_desc
      then (
        typ'.type_desc <- Tref typ ;
        typ'.type_alternate.type_desc <- Tref typ.type_alternate ;
        typ'.type_alternate.type_alternate.type_desc
        <- Tref typ.type_alternate.type_alternate ;
        typ )
      else (
        typ'.type_desc <- type_desc ;
        typ'.type_alternate.type_desc <- alt_type_desc ;
        typ'.type_alternate.type_alternate.type_desc <- alt_alt_type_desc ;
        typ' )

let type_desc mapper desc =
  match desc with
  | Tvar _ ->
      desc
  | Ttuple typs ->
      let same = ref true in
      let typs = map_list typs ~same ~f:(mapper.type_expr mapper) in
      if !same then desc else Ttuple typs
  | Tarrow (typ1, typ2, explicit, label) ->
      let typ1' = mapper.type_expr mapper typ1 in
      let typ2' = mapper.type_expr mapper typ2 in
      if phys_equal typ1' typ1 && phys_equal typ2' typ2 then desc
      else Tarrow (typ1', typ2', explicit, label)
  | Tctor variant ->
      let variant' = mapper.variant mapper variant in
      if phys_equal variant' variant then desc else Tctor variant'
  | Tpoly (vars, typ) ->
      let same = ref true in
      let vars = map_list vars ~same ~f:(mapper.type_expr mapper) in
      let typ' = mapper.type_expr mapper typ in
      if !same && phys_equal typ' typ then desc else Tpoly (vars, typ')
  | Tref typ ->
      let typ' = mapper.type_expr mapper typ in
      if phys_equal typ' typ then desc else Tref typ'
  | Tconv typ ->
      let typ' = mapper.type_expr mapper typ in
      if phys_equal typ' typ then desc else Tconv typ'
  | Topaque typ ->
      let typ' = mapper.type_expr mapper typ in
      if phys_equal typ' typ then desc else Topaque typ'
  | Tother_mode typ ->
      let typ' = mapper.type_expr mapper typ in
      if phys_equal typ' typ then desc else Tother_mode typ'
  | Treplace typ ->
      (* Recursion breaking. *)
      typ.type_desc

let variant mapper ({var_ident= ident; var_params} as variant) =
  let var_ident = mapper.path mapper ident in
  let same = ref true in
  let var_params = map_list var_params ~same ~f:(mapper.type_expr mapper) in
  if !same && phys_equal var_ident ident then variant
  else {var_ident; var_params}

let field_decl mapper ({fld_ident= ident; fld_type= typ} as fld) =
  let fld_type = mapper.type_expr mapper typ in
  let fld_ident = mapper.ident mapper ident in
  if phys_equal fld_type typ && phys_equal fld_ident ident then fld
  else {fld_ident; fld_type}

let ctor_args mapper args =
  match args with
  | Ctor_tuple typs ->
      let same = ref true in
      let typs = map_list typs ~same ~f:(mapper.type_expr mapper) in
      if !same then args else Ctor_tuple typs
  | Ctor_record decl ->
      let decl' = mapper.type_decl mapper decl in
      if phys_equal decl' decl then args else Ctor_record decl

let ctor_decl mapper ({ctor_ident; ctor_args= args; ctor_ret} as decl) =
  let ctor_ident = mapper.ident mapper ctor_ident in
  let ctor_args = mapper.ctor_args mapper args in
  match ctor_ret with
  | None ->
      if phys_equal ctor_args args && phys_equal ctor_ident decl.ctor_ident
      then decl
      else {ctor_ident; ctor_args; ctor_ret}
  | Some ret ->
      let ctor_ret = mapper.type_expr mapper ret in
      if
        phys_equal ctor_args args && phys_equal ctor_ret ret
        && phys_equal ctor_ident decl.ctor_ident
      then decl
      else {ctor_ident; ctor_args; ctor_ret= Some ctor_ret}

let type_decl mapper
    ({tdec_params= params; tdec_desc= desc; tdec_id; tdec_ret= ret} as decl) =
  let same = ref true in
  let tdec_params = map_list params ~same ~f:(mapper.type_expr mapper) in
  let tdec_desc = mapper.type_decl_desc mapper desc in
  let tdec_ret = mapper.type_expr mapper ret in
  if !same && phys_equal tdec_desc desc && phys_equal tdec_ret ret then decl
  else {tdec_params; tdec_desc; tdec_id; tdec_ret}

let type_decl_desc mapper desc =
  match desc with
  | TAbstract | TOpen ->
      desc
  | TAlias typ ->
      let typ' = mapper.type_expr mapper typ in
      if phys_equal typ' typ then desc else TAlias typ'
  | TRecord flds ->
      let same = ref true in
      let flds = map_list flds ~same ~f:(mapper.field_decl mapper) in
      if !same then desc else TRecord flds
  | TVariant ctors ->
      let same = ref true in
      let ctors = map_list ctors ~same ~f:(mapper.ctor_decl mapper) in
      if !same then desc else TVariant ctors
  | TExtend (path, ctors) ->
      let path' = mapper.path mapper path in
      let same = ref true in
      let ctors = map_list ctors ~same ~f:(mapper.ctor_decl mapper) in
      if !same && phys_equal path' path then desc else TExtend (path', ctors)

let conv_type mapper conv_type =
  match conv_type with
  | Conv_with (ident, mode, decl) ->
      let ident' = mapper.ident mapper ident in
      let decl' = mapper.type_decl mapper decl in
      if phys_equal ident' ident && phys_equal decl' decl then conv_type
      else Conv_with (ident', mode, decl')
  | Conv_to typ ->
      let typ' = mapper.type_expr mapper typ in
      if phys_equal typ' typ then conv_type else Conv_to typ'

let signature_item mapper sigi =
  match sigi with
  | Svalue (name, typ) ->
      let name' = mapper.ident mapper name in
      let typ' = mapper.type_expr mapper typ in
      if phys_equal name' name && phys_equal typ' typ then sigi
      else Svalue (name', typ')
  | Sinstance (name, typ) ->
      let name' = mapper.ident mapper name in
      let typ' = mapper.type_expr mapper typ in
      if phys_equal name' name && phys_equal typ' typ then sigi
      else Sinstance (name', typ')
  | Stype (name, typ) ->
      let name' = mapper.ident mapper name in
      let typ' = mapper.type_decl mapper typ in
      if phys_equal name' name && phys_equal typ' typ then sigi
      else Stype (name', typ')
  | Sconvtype (name, decl, conv_decl, conv_name, conv_typ) ->
      let name' = mapper.ident mapper name in
      let decl' = mapper.type_decl mapper decl in
      let conv_decl' = mapper.conv_type mapper conv_decl in
      let conv_name' = mapper.ident mapper conv_name in
      let conv_typ' = mapper.type_expr mapper conv_typ in
      if
        phys_equal name' name && phys_equal decl' decl
        && phys_equal conv_decl' conv_decl
        && phys_equal conv_name' conv_name
        && phys_equal conv_typ' conv_typ
      then sigi
      else Sconvtype (name', decl', conv_decl', conv_name', conv_typ')
  | Srectype typs ->
      let same = ref true in
      let typs =
        map_list typs ~same ~f:(fun x ->
            let name, typ = x in
            let name' = mapper.ident mapper name in
            let typ' = mapper.type_decl mapper typ in
            if phys_equal name' name && phys_equal typ' typ then x
            else (name', typ') )
      in
      if !same then sigi else Srectype typs
  | Smodule (name, mty) ->
      let name' = mapper.ident mapper name in
      let mty' = mapper.module_sig mapper mty in
      if phys_equal name' name && phys_equal mty' mty then sigi
      else Smodule (name', mty')
  | Smodtype (name, mty) ->
      let name' = mapper.ident mapper name in
      let mty' = mapper.module_sig mapper mty in
      if phys_equal name' name && phys_equal mty' mty then sigi
      else Smodtype (name', mty')
  | Stypeext (variant, ctors) ->
      let variant' = mapper.variant mapper variant in
      let same = ref true in
      let ctors = map_list ctors ~same ~f:(mapper.ctor_decl mapper) in
      if !same && phys_equal variant' variant then sigi
      else Stypeext (variant', ctors)
  | Srequest (typ, ctor) ->
      let typ' = mapper.type_expr mapper typ in
      let ctor' = mapper.ctor_decl mapper ctor in
      if phys_equal typ' typ && phys_equal ctor' ctor then sigi
      else Srequest (typ', ctor')
  | Sprover sigs ->
      let sigs' = mapper.signature mapper sigs in
      if phys_equal sigs' sigs then sigi else Sprover sigs'

let signature mapper sigs =
  let same = ref true in
  let sigs' = map_list sigs ~same ~f:(mapper.signature_item mapper) in
  if !same then sigs else sigs'

let module_sig mapper mty =
  match mty with
  | Msig sigs ->
      let sigs' = mapper.signature mapper sigs in
      if phys_equal sigs' sigs then mty else Msig sigs'
  | Mname path ->
      let path' = mapper.path mapper path in
      if phys_equal path' path then mty else Mname path'
  | Malias path ->
      let path' = mapper.path mapper path in
      if phys_equal path' path then mty else Malias path'
  | Mabstract ->
      mty
  | Mfunctor (name, fty, mty) ->
      let fty' = mapper.module_sig mapper fty in
      let mty' = mapper.module_sig mapper mty in
      if phys_equal fty' fty && phys_equal mty' mty then mty
      else Mfunctor (name, fty', mty')

let path mapper path =
  match path with
  | Path.Pident ident ->
      let ident' = mapper.ident mapper ident in
      if phys_equal ident' ident then path else Path.Pident ident'
  | Path.Pdot (path1, mode, str) ->
      let path1' = mapper.path mapper path1 in
      if phys_equal path1' path1 then path else Path.Pdot (path1', mode, str)
  | Path.Papply (path1, path2) ->
      let path1' = mapper.path mapper path1 in
      let path2' = mapper.path mapper path2 in
      if phys_equal path1' path1 && phys_equal path2' path2 then path
      else Path.Papply (path1', path2')

let ident (_mapper : mapper) (ident : Ident.t) = ident

let default_mapper =
  { type_expr
  ; type_desc
  ; variant
  ; field_decl
  ; ctor_args
  ; ctor_decl
  ; type_decl
  ; type_decl_desc
  ; conv_type
  ; signature_item
  ; signature
  ; module_sig
  ; ident
  ; path }
