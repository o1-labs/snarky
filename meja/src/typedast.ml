open Ast_types
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
  | Tpat_record of (lid * pattern) list
  | Tpat_ctor of lid * pattern option

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
  | Texp_field of expression * lid
  | Texp_record of (lid * expression) list * expression option
  | Texp_ctor of lid * expression option
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
