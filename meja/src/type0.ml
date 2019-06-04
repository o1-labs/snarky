open Core_kernel
open Ast_types

module Row = struct
  type 'a t =
    | Row_spec of 'a
    | Row_union of 'a t * 'a t
    | Row_inter of 'a t * 'a t
    | Row_diff of 'a t * 'a t
  [@@deriving ord, sexp]

  type 'a spec =
    {row_ctors: (Longident.t * int) (* decl_id *) list; row_typs: 'a list}
  [@@deriving ord, sexp]

  let has_ctors {row_ctors; _} = List.is_empty row_ctors

  let has_typs {row_typs; _} = List.is_empty row_typs

  let is_spec = function Row_spec _ -> true | _ -> false

  let has_direct_ctors = function
    | Row_spec spec ->
        has_ctors spec
    | _ ->
        false

  let has_direct_typs = function Row_spec spec -> has_typs spec | _ -> false

  open Format

  let rec debug_print print_spec fmt = function
    | Row_spec spec ->
        fprintf fmt "[%a]" print_spec spec
    | Row_union (row1, row2) ->
        fprintf fmt "[%a] + [%a]" (debug_print print_spec) row1
          (debug_print print_spec) row2
    | Row_inter (row1, row2) ->
        fprintf fmt "[%a] & [%a]" (debug_print print_spec) row1
          (debug_print print_spec) row2
    | Row_diff (row1, row2) ->
        fprintf fmt "[%a] - [%a]" (debug_print print_spec) row1
          (debug_print print_spec) row2

  let spec_debug_print print_typ fmt spec =
    let is_first = ref true in
    let pp_sep fmt () =
      if !is_first then is_first := false else fprintf fmt " | "
    in
    List.iter spec.row_ctors ~f:(fun (lid, id) ->
        fprintf fmt "%a%a(%i)" pp_sep () Longident.pp lid id ) ;
    List.iter spec.row_typs ~f:(fprintf fmt "%a%a" pp_sep () print_typ)

  let fold ~init ~f row =
    match row with
    | Row_spec _ ->
        init
    | Row_union (row1, row2) | Row_inter (row1, row2) | Row_diff (row1, row2)
      ->
        let acc = f init row1 in
        f acc row2

  let iter ~f = fold ~init:() ~f:(fun () -> f)

  let rec fold_typ ~init ~f row =
    match row with
    | Row_spec spec ->
        List.fold ~init ~f spec.row_typs
    | _ ->
        fold ~init ~f:(fun acc row -> fold_typ ~init:acc ~f row) row

  let iter_typ ~f = fold_typ ~init:() ~f:(fun () -> f)
end

type type_expr = {mutable type_desc: type_desc; type_id: int; type_depth: int}
[@@deriving sexp]

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of str option * explicitness
  | Ttuple of type_expr list
  | Tarrow of type_expr * type_expr * explicitness * arg_label
  (* A type name. *)
  | Tctor of variant
  | Tpoly of type_expr list * type_expr
  | Trow of row
[@@deriving sexp]

and variant =
  { var_ident: lid
  ; var_params: type_expr list
  ; var_implicit_params: type_expr list
  ; var_decl: type_decl }
[@@deriving sexp]

and row_spec = type_expr Row.spec [@@deriving sexp]

and row = row_spec Row.t [@@deriving sexp]

and field_decl = {fld_ident: str; fld_type: type_expr; fld_id: int}
[@@deriving sexp]

and ctor_args = Ctor_tuple of type_expr list | Ctor_record of type_decl
[@@deriving sexp]

and ctor_decl =
  {ctor_ident: str; ctor_args: ctor_args; ctor_ret: type_expr option}
[@@deriving sexp]

and type_decl =
  { tdec_ident: str
  ; tdec_params: type_expr list
  ; tdec_implicit_params: type_expr list
  ; tdec_desc: type_decl_desc
  ; tdec_id: int }
[@@deriving sexp]

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
[@@deriving sexp]

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
      print "(%a)" (print_list typ_debug_print) typs
  | Trow row ->
      print "[%a]" row_debug_print row ) ;
  print " @%i)" typ.type_depth

and row_debug_print fmt row =
  Row.debug_print (Row.spec_debug_print typ_debug_print) fmt row

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
  | Trow row ->
      Row.fold_typ ~init ~f row

let iter ~f = fold ~init:() ~f:(fun () -> f)
