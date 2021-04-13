open Core_kernel
open Ast_types
open Type0
open Format

let get_mode : (mode -> type_expr -> type_expr) ref =
  ref (fun _ _ -> assert false)

let explicitness fmt = function
  | Explicit ->
      pp_print_string fmt "Explicit"
  | Implicit ->
      pp_print_string fmt "Implicit"

let arg_label fmt = function
  | Nolabel ->
      ()
  | Labelled s ->
      pp_print_string fmt s
  | Optional s ->
      fprintf fmt "?%s" s

let closed_flag fmt = function
  | Closed ->
      pp_print_string fmt "Closed"
  | Open ->
      pp_print_string fmt "Open"

let rec row_presence fmt {rp_desc; rp_id} =
  fprintf fmt "@[<1>{rp_id=%d;rp_desc=@,%a}@]" rp_id row_presence_desc rp_desc

and row_presence_desc fmt = function
  | RpPresent ->
      pp_print_string fmt "RpPresent"
  | RpMaybe ->
      pp_print_string fmt "RpMaybe"
  | RpAbsent ->
      pp_print_string fmt "RpAbsent"
  | RpSubtract rp ->
      fprintf fmt "RpSubtract %a" row_presence rp
  | RpAny ->
      pp_print_string fmt "RpAny"
  | RpRef rp ->
      fprintf fmt "RpRef %a" row_presence rp
  | RpReplace rp ->
      fprintf fmt "RpReplace %a" row_presence rp

let map ~key ~data fmt m =
  let first = ref true in
  fprintf fmt "@[<1>(" ;
  Map.iteri m ~f:(fun ~key:k ~data:d ->
      if !first then first := false else fprintf fmt ";@," ;
      fprintf fmt "@[<hov1>%a:@,%a@]" key k data d ) ;
  fprintf fmt ")@]"

let list pp fmt l =
  fprintf fmt "@[<1>[%a]@]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@,") pp)
    l

let option pp fmt = function
  | None ->
      pp_print_string fmt "None"
  | Some x ->
      pp fmt x

(** Hash set to track types printed in [typ_debug_print], to ensure that we
    don't get stuck in a recursion loop.
*)
let type_print_hash_tbl = Hash_set.create (module Int) ()

let rec type_expr' fmt typ =
  if Hash_set.mem type_print_hash_tbl typ.type_id then
    fprintf fmt "{id=%d}" typ.type_id
  else (
    Hash_set.add type_print_hash_tbl typ.type_id ;
    fprintf fmt
      "@[<1>{id=%d;depth=%d;mode=%a;type_alternate={id=%d};desc=@,%a}@]"
      typ.type_id typ.type_depth mode_debug_print typ.type_mode
      typ.type_alternate.type_id type_desc typ.type_desc )

and type_desc fmt = function
  | Tvar name ->
      fprintf fmt "Tvar %a" (option pp_print_string) name
  | Tpoly (typs, typ) ->
      fprintf fmt "@[<hov1>Tpoly (@,%a,@,%a)@]" (list type_expr') typs
        type_expr' typ
  | Tarrow (typ1, typ2, explicit, label) ->
      fprintf fmt "@[<hov1>Tarrow(@,%a,@,%a,@,%a,@,\"%a\")@]" type_expr' typ1
        type_expr' typ2 explicitness explicit arg_label label
  | Tctor var ->
      fprintf fmt "@[<hov1>Tctor(%a)@]" variant var
  | Ttuple typs ->
      fprintf fmt "@[<hov1>Ttuple(%a)@]" (list type_expr') typs
  | Tref typ ->
      fprintf fmt "@[<hov1>Tref(%a)@]" type_expr' typ
  | Tconv typ ->
      fprintf fmt "@[<hov1>Tconv(%a,%a)@]" type_expr' typ type_expr'
        typ.type_alternate
  | Topaque typ ->
      fprintf fmt "@[<hov1>Topaque(%a)@]" type_expr' typ
  | Tother_mode typ ->
      fprintf fmt "@[<hov1>Tother_mode(%a)@]" type_expr' typ
  | Treplace typ ->
      fprintf fmt "@[<hov1>Treplace(%a)@]" type_expr' typ
  | Trow row_contents ->
      fprintf fmt "@[<hov1>Trow(%a)@]" row row_contents

and variant fmt {var_ident; var_params} =
  fprintf fmt "@[<1>{var_ident=%a;var_params=@,%a}@]" Path.debug_print
    var_ident (list type_expr') var_params

and row fmt {row_tags; row_closed; row_rest; row_presence_proxy} =
  fprintf fmt
    "{@[<hov>@,row_tags=@,%a@,;row_closed=@,%a@,row_rest=@,%a@,row_presence_proxy=@,%a@,@]}"
    (map ~key:Ident.debug_print ~data:(fun fmt (path, rp, typs) ->
         fprintf fmt "@[<hov1>(%a,@,%a,@,%a)@]" Path.debug_print path
           row_presence rp (list type_expr') typs ))
    row_tags closed_flag row_closed type_expr' row_rest row_presence
    row_presence_proxy

let field_decl fmt {fld_ident; fld_type} =
  fprintf fmt "@[<hov1>{fld_ident=%a;fld_type=@,%a}@]" Ident.debug_print
    fld_ident type_expr' fld_type

let rec ctor_args fmt = function
  | Ctor_tuple typs ->
      fprintf fmt "Ctor_tuple %a" (list type_expr') typs
  | Ctor_record decl ->
      fprintf fmt "Ctor_record %a" type_decl' decl

and ctor_decl fmt {ctor_ident; ctor_args= args; ctor_ret} =
  fprintf fmt "{@[<hov>@,ctor_ident=%a;@,ctor_ret=@,%a;@,ctor_args=@,%a@,@]}"
    Ident.debug_print ctor_ident ctor_args args (option type_expr') ctor_ret

and type_decl' fmt {tdec_params; tdec_desc; tdec_id; tdec_ret} =
  fprintf fmt
    "{@[<hov>@,tdec_id=%d;@,tdec_params=@,%a;@,tdec_desc=@,%a;@,tdec_ret=@,%a@,@]}"
    tdec_id (list type_expr') tdec_params type_decl_desc tdec_desc type_expr'
    tdec_ret

and type_decl_desc fmt = function
  | TAbstract ->
      pp_print_string fmt "TAbstract"
  | TAlias typ ->
      fprintf fmt "TAlias %a" type_expr' typ
  | TRecord fields ->
      fprintf fmt "TRecord %a" (list field_decl) fields
  | TVariant ctors ->
      fprintf fmt "TVariant %a" (list ctor_decl) ctors
  | TOpen ->
      pp_print_string fmt "TOpen"
  | TExtend (path, ctors) ->
      fprintf fmt "TExtend (%a,@,%a)" Path.debug_print path (list ctor_decl)
        ctors

let rec type_expr_alts' fmt typ =
  if Hash_set.mem type_print_hash_tbl typ.type_id then
    fprintf fmt "{id=%d}" typ.type_id
  else (
    Hash_set.add type_print_hash_tbl typ.type_id ;
    fprintf fmt
      "{@[id=%d;depth=%d;mode=@,%a@,;type_alternate=@,%a@,;desc=@,%a@,@]}"
      typ.type_id typ.type_depth mode_debug_print typ.type_mode type_expr_alts'
      typ.type_alternate type_desc typ.type_desc )

let with_clear pp fmt x =
  Hash_set.clear type_print_hash_tbl ;
  pp fmt x ;
  Hash_set.clear type_print_hash_tbl

let type_expr = with_clear type_expr'

let type_expr_alts = with_clear type_expr_alts'

let type_decl = with_clear type_decl'
