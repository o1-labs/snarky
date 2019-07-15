open Ast_types

type type_expr = {type_desc: type_desc; type_loc: Location.t}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Ttyp_var of str option * explicitness
  | Ttyp_tuple of type_expr list
  | Ttyp_arrow of type_expr * type_expr * explicitness * Asttypes.arg_label
  (* A type name. *)
  | Ttyp_ctor of variant
  | Ttyp_poly of type_expr list * type_expr

and variant =
  { var_ident: lid
  ; var_params: type_expr list
  ; var_implicit_params: type_expr list }

type field_decl = {fld_ident: str; fld_type: type_expr; fld_loc: Location.t}

type ctor_args =
  | Ttor_tuple of type_expr list
  | Ttor_record of int * field_decl list

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
  ; tdec_loc: Location.t }

and type_decl_desc =
  | Tdec_abstract
  | Tdec_alias of type_expr
  | Tdec_unfold of type_expr
  | Tdec_record of field_decl list
  | Tdec_variant of ctor_decl list
  | Tdec_open
  | Tdec_extend of lid * Type0.type_decl * ctor_decl list
      (** Internal; this should never be present in the AST. *)
  | Tdec_forward of int option ref
      (** Forward declaration for types loaded from cmi files. *)

open Parsetypes

type ident = Ident.t Location.loc

type path = Path.t Location.loc

type literal = Int of int | Bool of bool | Field of string | String of string

type pattern =
  {pat_desc: pattern_desc; pat_loc: Location.t; pat_type: Type0.type_expr}

and pattern_desc =
  | Tpat_any
  | Tpat_variable of ident
  | Tpat_constraint of pattern * type_expr
  | Tpat_tuple of pattern list
  | Tpat_or of pattern * pattern
  | Tpat_int of int
  | Tpat_record of (path * pattern) list
  | Tpat_ctor of path * pattern option

type expression =
  {exp_desc: expression_desc; exp_loc: Location.t; exp_type: Type0.type_expr}

and expression_desc =
  | Texp_apply of expression * (Asttypes.arg_label * expression) list
  | Texp_variable of path
  | Texp_literal of literal
  | Texp_fun of Asttypes.arg_label * pattern * expression * explicitness
  | Texp_newtype of ident * expression
  | Texp_seq of expression * expression
  | Texp_let of pattern * expression * expression
  | Texp_constraint of expression * type_expr
  | Texp_tuple of expression list
  | Texp_match of expression * (pattern * expression) list
  | Texp_field of expression * path
  | Texp_record of (path * expression) list * expression option
  | Texp_ctor of path * expression option
  | Texp_unifiable of
      { mutable expression: expression option
      ; name: ident
      ; id: int }
  | Texp_if of expression * expression * expression option

type signature_item = {sig_desc: signature_desc; sig_loc: Location.t}

and signature = signature_item list

and signature_desc =
  | Tsig_value of ident * type_expr
  | Tsig_instance of ident * type_expr
  | Tsig_type of type_decl
  | Tsig_module of ident * module_sig
  | Tsig_modtype of ident * module_sig
  | Tsig_open of path
  | Tsig_typeext of variant * ctor_decl list
  | Tsig_request of type_expr * ctor_decl
  | Tsig_multiple of signature

and module_sig = {msig_desc: module_sig_desc; msig_loc: Location.t}

and module_sig_desc =
  | Tmty_sig of signature
  | Tmty_name of path
  | Tmty_abstract
  | Tmty_functor of str * module_sig * module_sig

type statement = {stmt_desc: statement_desc; stmt_loc: Location.t}

and statements = statement list

and statement_desc =
  | Tstmt_value of pattern * expression
  | Tstmt_instance of ident * expression
  | Tstmt_type of type_decl
  | Tstmt_module of ident * module_expr
  | Tstmt_modtype of ident * module_sig
  | Tstmt_open of path
  | Tstmt_typeext of variant * ctor_decl list
  | Tstmt_request of
      type_expr * ctor_decl * (pattern option * expression) option
  | Tstmt_multiple of statements

and module_expr = {mod_desc: module_desc; mod_loc: Location.t}

and module_desc =
  | Tmod_struct of statements
  | Tmod_name of path
  | Tmod_functor of str * module_sig * module_expr
