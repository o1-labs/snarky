open Core_kernel
open Ast_types

type type_expr =
  {mutable type_desc: type_desc; type_id: int; mutable type_depth: int}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of str option * explicitness
  | Ttuple of type_expr list
  | Tarrow of type_expr * type_expr * explicitness * Asttypes.arg_label
  (* A type name. *)
  | Tctor of variant
  | Tpoly of type_expr list * type_expr

and variant =
  { var_ident: lid
  ; var_params: type_expr list
  ; var_implicit_params: type_expr list
  ; var_decl: type_decl }

and field_decl = {fld_ident: str; fld_type: type_expr; fld_id: int}

and ctor_args = Ctor_tuple of type_expr list | Ctor_record of type_decl

and ctor_decl =
  {ctor_ident: str; ctor_args: ctor_args; ctor_ret: type_expr option}

and type_decl =
  { tdec_ident: str
  ; tdec_params: type_expr list
  ; tdec_implicit_params: type_expr list
  ; tdec_desc: type_decl_desc
  ; tdec_id: int }

and type_decl_desc =
  | TAbstract
  | TAlias of type_expr
  | TUnfold of type_expr
  | TRecord of field_decl list
  | TVariant of ctor_decl list
  | TOpen
  | TExtend of lid * type_decl * ctor_decl list
      (** Internal; this should never be present in the AST. *)
  | TForward of int option ref
      (** Forward declaration for types loaded from cmi files. *)

let none = {type_desc= Tvar (None, Explicit); type_id= -1; type_depth= -1}

let rec typ_debug_print fmt typ =
  let open Format in
  let print i = fprintf fmt i in
  let print_comma fmt () = pp_print_char fmt ',' in
  let print_list pp = pp_print_list ~pp_sep:print_comma pp in
  let print_label fmt = function
    | Asttypes.Nolabel ->
        ()
    | Asttypes.Labelled str ->
        fprintf fmt "~%s:" str
    | Asttypes.Optional str ->
        fprintf fmt "?%s:" str
  in
  print "(%i:" typ.type_id ;
  ( match typ.type_desc with
  | Tvar (None, Explicit) ->
      print "var _"
  | Tvar (Some name, Explicit) ->
      print "var %s@" name.txt
  | Tvar (None, Implicit) ->
      print "implicit_var _"
  | Tvar (Some name, Implicit) ->
      print "implicit_var %s" name.txt
  | Tpoly (typs, typ) ->
      print "poly [%a] %a"
        (print_list typ_debug_print)
        typs typ_debug_print typ
  | Tarrow (typ1, typ2, Explicit, label) ->
      print "%a%a -> %a" print_label label typ_debug_print typ1 typ_debug_print
        typ2
  | Tarrow (typ1, typ2, Implicit, label) ->
      print "%a{%a} -> %a" print_label label typ_debug_print typ1
        typ_debug_print typ2
  | Tctor {var_ident= name; var_params= params; _} ->
      print "%a (%a)" Longident.pp name.txt (print_list typ_debug_print) params
  | Ttuple typs ->
      print "(%a)" (print_list typ_debug_print) typs ) ;
  print " @%i)" typ.type_depth

let fold ~init ~f typ =
  match typ.type_desc with
  | Tvar _ ->
      init
  | Ttuple typs ->
      List.fold ~init ~f typs
  | Tarrow (typ1, typ2, _, _) ->
      let acc = f init typ1 in
      f acc typ2
  | Tctor variant ->
      let acc = List.fold ~init ~f variant.var_params in
      List.fold ~init:acc ~f variant.var_implicit_params
  | Tpoly (typs, typ) ->
      let acc = List.fold ~init ~f typs in
      f acc typ

let iter ~f = fold ~init:() ~f:(fun () -> f)

(* TODO: integrate with a backtrack mechanism for unification errors. *)
let set_depth depth typ = typ.type_depth <- depth

let update_depth depth typ = if typ.type_depth > depth then set_depth depth typ

let unify_depths typ1 typ2 =
  iter ~f:(update_depth typ1.type_depth) typ2 ;
  iter ~f:(update_depth typ2.type_depth) typ1
