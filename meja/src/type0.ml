open Core_kernel
open Ast_types

type row_presence = Present | Maybe | Absent [@@deriving sexp, compare]

type type_expr =
  { mutable type_desc: type_desc
  ; type_id: int
  ; mutable type_depth: int
  ; type_mode: mode
  ; mutable type_alternate: type_expr }
[@@deriving sexp]

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of string option
  | Ttuple of type_expr list
  | Tarrow of type_expr * type_expr * explicitness * Ast_types.arg_label
  (* A type name. *)
  | Tctor of variant
  | Tpoly of type_expr list * type_expr
  | Tref of type_expr
  (* Defines a conversion between the two stitched types in the argument. *)
  | Tconv of type_expr
  (* Denotes that the checked part of the stitching is opaque. The prover part
     also uses the [Topaque] constructor, but only to keep the stitching in
     sync structurally.
  *)
  | Topaque of type_expr
  (* A type used to transparently expose types from one mode in another mode.
     This should only be used for implicit arguments.
  *)
  | Tother_mode of type_expr
  (* Cache the current value to break recursion. *)
  | Treplace of type_expr
  | Trow of row
[@@deriving sexp]

and variant = {var_ident: Path.t; var_params: type_expr list} [@@deriving sexp]

and row =
  { row_tags: (Path.t * row_presence * type_expr list) Ident.Map.t
  ; row_closed: closed_flag
        (* A type variable used to signal whether to copy the row or not. *)
  ; row_proxy: type_expr }
[@@deriving sexp]

type field_decl = {fld_ident: Ident.t; fld_type: type_expr} [@@deriving sexp]

type ctor_args = Ctor_tuple of type_expr list | Ctor_record of type_decl
[@@deriving sexp]

and ctor_decl =
  {ctor_ident: Ident.t; ctor_args: ctor_args; ctor_ret: type_expr option}
[@@deriving sexp]

and type_decl =
  { tdec_params: type_expr list
  ; tdec_desc: type_decl_desc
  ; tdec_id: int
  ; tdec_ret: type_expr }
[@@deriving sexp]

and type_decl_desc =
  | TAbstract
  | TAlias of type_expr
  | TRecord of field_decl list
  | TVariant of ctor_decl list
  | TOpen
  | TExtend of Path.t * ctor_decl list
      (** Internal; this should never be present in the AST. *)
[@@deriving sexp]
