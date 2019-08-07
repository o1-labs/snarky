open Core_kernel
open Ast_types

type type_expr =
  {mutable type_desc: type_desc; type_id: int; mutable type_depth: int}
[@@deriving sexp]

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of string option * explicitness
  | Ttuple of type_expr list
  | Tarrow of type_expr * type_expr * explicitness * Ast_types.arg_label
  (* A type name. *)
  | Tctor of variant
  | Tpoly of type_expr list * type_expr
[@@deriving sexp]

and variant =
  { var_ident: Path.t
  ; var_params: type_expr list
  ; var_implicit_params: type_expr list
  ; var_decl: type_decl }
[@@deriving sexp]

and field_decl = {fld_ident: Ident.t; fld_type: type_expr} [@@deriving sexp]

and ctor_args = Ctor_tuple of type_expr list | Ctor_record of type_decl
[@@deriving sexp]

and ctor_decl =
  {ctor_ident: Ident.t; ctor_args: ctor_args; ctor_ret: type_expr option}
[@@deriving sexp]

and type_decl =
  { tdec_ident: Ident.t
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
  | TExtend of Path.t * type_decl * ctor_decl list
      (** Internal; this should never be present in the AST. *)
  | TForward of int option ref
      (** Forward declaration for types loaded from cmi files. *)
[@@deriving sexp]
