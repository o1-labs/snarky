open Ast_types

type type_expr = {type_desc: type_desc; type_loc: Location.t}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Ptyp_var of str option * explicitness
  | Ptyp_tuple of type_expr list
  | Ptyp_arrow of type_expr * type_expr * explicitness * Asttypes.arg_label
  (* A type name. *)
  | Ptyp_ctor of variant
  | Ptyp_poly of type_expr list * type_expr

and variant =
  { var_ident: lid
  ; var_params: type_expr list
  ; var_implicit_params: type_expr list }

type field_decl = {fld_ident: str; fld_type: type_expr; fld_loc: Location.t}

type ctor_args =
  | Ctor_tuple of type_expr list
  | Ctor_record of field_decl list

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
  | Pdec_abstract
  | Pdec_alias of type_expr
  | Pdec_record of field_decl list
  | Pdec_variant of ctor_decl list
  | Pdec_open
  | Pdec_extend of Path.t Location.loc * Type0.type_decl * ctor_decl list
      (** Internal; this should never be present in the AST. *)

type literal = Int of int | Bool of bool | Field of string | String of string

type pattern = {pat_desc: pattern_desc; pat_loc: Location.t}

and pattern_desc =
  | Ppat_any
  | Ppat_variable of str
  | Ppat_constraint of pattern * type_expr
  | Ppat_tuple of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_int of int
  | Ppat_record of (lid * pattern) list
  | Ppat_ctor of lid * pattern option

type expression = {exp_desc: expression_desc; exp_loc: Location.t}

and expression_desc =
  | Pexp_apply of expression * (Asttypes.arg_label * expression) list
  | Pexp_variable of lid
  | Pexp_literal of literal
  | Pexp_fun of Asttypes.arg_label * pattern * expression * explicitness
  | Pexp_newtype of str * expression
  | Pexp_seq of expression * expression
  | Pexp_let of pattern * expression * expression
  | Pexp_constraint of expression * type_expr
  | Pexp_tuple of expression list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_field of expression * lid
  | Pexp_record of (lid * expression) list * expression option
  | Pexp_ctor of lid * expression option
  | Pexp_unifiable of
      { mutable expression: expression option
      ; name: str
      ; id: int }
  | Pexp_if of expression * expression * expression option
  | Pexp_prover of expression

type signature_item = {sig_desc: signature_desc; sig_loc: Location.t}

and signature = signature_item list

and signature_desc =
  | Psig_value of str * type_expr
  | Psig_instance of str * type_expr
  | Psig_type of type_decl
  | Psig_module of str * module_sig
  | Psig_modtype of str * module_sig
  | Psig_open of lid
  | Psig_typeext of variant * ctor_decl list
  | Psig_request of type_expr * ctor_decl
  | Psig_multiple of signature
  | Psig_prover of signature

and module_sig = {msig_desc: module_sig_desc; msig_loc: Location.t}

and module_sig_desc =
  | Pmty_sig of signature
  | Pmty_name of lid
  | Pmty_alias of lid
  | Pmty_abstract
  | Pmty_functor of str * module_sig * module_sig

type statement = {stmt_desc: statement_desc; stmt_loc: Location.t}

and statements = statement list

and statement_desc =
  | Pstmt_value of pattern * expression
  | Pstmt_instance of str * expression
  | Pstmt_type of type_decl
  | Pstmt_module of str * module_expr
  | Pstmt_modtype of str * module_sig
  | Pstmt_open of lid
  | Pstmt_typeext of variant * ctor_decl list
  | Pstmt_request of
      type_expr * ctor_decl * (pattern option * expression) option
  | Pstmt_multiple of statements
  | Pstmt_prover of statements

and module_expr = {mod_desc: module_desc; mod_loc: Location.t}

and module_desc =
  | Pmod_struct of statements
  | Pmod_name of lid
  | Pmod_functor of str * module_sig * module_expr
