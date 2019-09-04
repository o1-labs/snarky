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
*)

let type_expr mapper ({type_desc= desc; type_id; type_depth} as typ) =
  let type_desc = mapper.type_desc mapper desc in
  if phys_equal type_desc desc then typ else {type_desc; type_id; type_depth}

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

let variant mapper ({var_ident= ident; var_params; var_decl= decl} as variant)
    =
  let var_ident = mapper.path mapper ident in
  let same = ref true in
  let var_params = map_list var_params ~same ~f:(mapper.type_expr mapper) in
  let var_decl = mapper.type_decl mapper decl in
  if !same && phys_equal var_ident ident && phys_equal var_decl decl then
    variant
  else {var_ident; var_params; var_decl}

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
    ({tdec_ident= ident; tdec_params= params; tdec_desc= desc; tdec_id} as decl)
    =
  let same = ref true in
  let tdec_params = map_list params ~same ~f:(mapper.type_expr mapper) in
  let tdec_desc = mapper.type_decl_desc mapper desc in
  let tdec_ident = mapper.ident mapper ident in
  if !same && phys_equal tdec_desc desc && phys_equal tdec_ident ident then
    decl
  else {tdec_ident; tdec_params; tdec_desc; tdec_id}

let type_decl_desc mapper desc =
  match desc with
  | TAbstract | TOpen | TForward _ ->
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
  | TExtend (path, decl, ctors) ->
      let path' = mapper.path mapper path in
      let decl' = mapper.type_decl mapper decl in
      let same = ref true in
      let ctors = map_list ctors ~same ~f:(mapper.ctor_decl mapper) in
      if !same && phys_equal path' path && phys_equal decl' decl then desc
      else TExtend (path', decl', ctors)

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
  ; ident
  ; path }
