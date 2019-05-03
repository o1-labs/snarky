open Ast_types

type type_expr = {type_desc: type_desc; type_id: int; type_loc: Location.t}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of str option * (* depth *) int * explicitness
  | Ttuple of type_expr list
  | Tarrow of type_expr * type_expr * explicitness * Asttypes.arg_label
  (* A type name. *)
  | Tctor of variant
  | Tpoly of type_expr list * type_expr

and variant =
  { var_ident: lid
  ; var_params: type_expr list
  ; var_implicit_params: type_expr list
  ; var_decl_id: int }

type field_decl =
  {fld_ident: str; fld_type: type_expr; fld_id: int; fld_loc: Location.t}

type ctor_args =
  | Ctor_tuple of type_expr list
  | Ctor_record of int * field_decl list

type ctor_decl =
  { ctor_ident: str
  ; ctor_args: ctor_args
  ; ctor_ret: type_expr option
  ; ctor_loc: Location.t }

type type_decl =
  { tdec_ident: str
  ; tdec_params: type_expr list
  ; tdec_implicit_params: type_expr list
  ; tdec_desc: type_decl_desc
  ; tdec_id: int
  ; tdec_loc: Location.t }

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

let none ?(loc = Location.none) () =
  {type_desc= Tvar (None, -1, Explicit); type_id= -1; type_loc= loc}
