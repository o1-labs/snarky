open Ast_types

type ident = Ident.t Location.loc

type path = Path.t Location.loc

type type_expr =
  {type_desc: type_desc; type_loc: Location.t; type_type: Type0.type_expr}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Ttyp_var of str option
  | Ttyp_tuple of type_expr list
  | Ttyp_arrow of type_expr * type_expr * explicitness * Asttypes.arg_label
  (* A type name. *)
  | Ttyp_ctor of variant
  | Ttyp_poly of type_expr list * type_expr
  | Ttyp_prover of type_expr
  | Ttyp_conv of type_expr * type_expr
  | Ttyp_opaque of type_expr

and variant = {var_ident: path; var_params: type_expr list}

type field_decl =
  { fld_ident: ident
  ; fld_type: type_expr
  ; fld_loc: Location.t
  ; fld_fld: Type0.field_decl }

type ctor_args =
  | Tctor_tuple of type_expr list
  | Tctor_record of field_decl list

type ctor_decl =
  { ctor_ident: ident
  ; ctor_args: ctor_args
  ; ctor_ret: type_expr option
  ; ctor_loc: Location.t
  ; ctor_ctor: Type0.ctor_decl }

type type_decl =
  { tdec_ident: ident
  ; tdec_params: type_expr list
  ; tdec_desc: type_decl_desc
  ; tdec_loc: Location.t
  ; tdec_tdec: Type0.type_decl }

and type_decl_desc =
  | Tdec_abstract
  | Tdec_alias of type_expr
  | Tdec_record of field_decl list
  | Tdec_variant of ctor_decl list
  | Tdec_open
  | Tdec_extend of path * ctor_decl list
      (** Internal; this should never be present in the AST. *)

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

type convert_body =
  { conv_body_desc: convert_body_desc
  ; conv_body_loc: Location.t
  ; conv_body_type: Type0.type_expr }

(** AST for generating [Typ.t] instances. *)
and convert_body_desc =
  | Tconv_record of (path * convert_body) list
  | Tconv_ctor of path * (Asttypes.arg_label * convert_body) list
  | Tconv_tuple of convert_body list
  | Tconv_arrow of convert_body * convert_body
  | Tconv_identity
  | Tconv_opaque

and convert =
  {conv_desc: convert_desc; conv_loc: Location.t; conv_type: Type0.type_expr}

(** AST for generating [Typ.t] instances from other [Typ.t] instances. *)
and convert_desc = Tconv_fun of ident * convert | Tconv_body of convert_body

type expression =
  {exp_desc: expression_desc; exp_loc: Location.t; exp_type: Type0.type_expr}

and expression_desc =
  | Texp_apply of
      expression * (explicitness * Asttypes.arg_label * expression) list
  | Texp_variable of path
  | Texp_literal of literal
  | Texp_fun of Asttypes.arg_label * pattern * expression * explicitness
  | Texp_newtype of ident * expression
  | Texp_seq of expression * expression
  | Texp_let of Asttypes.rec_flag * (pattern * expression) list * expression
  | Texp_instance of ident * expression * expression
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
  | Texp_read of
      convert
      * (Asttypes.arg_label * expression) list
      (* arguments to the conversion *)
      * expression
  | Texp_prover of
      convert
      * (Asttypes.arg_label * expression) list
      (* arguments to the conversion *)
      * expression
  | Texp_convert of convert

type conv_type =
  (* Other mode stitched declaration. *)
  | Ttconv_with of mode * type_decl
  (* Tri-stitching to existing declaration. *)
  | Ttconv_to of type_expr

type signature_item = {sig_desc: signature_desc; sig_loc: Location.t}

and signature = signature_item list

and signature_desc =
  | Tsig_value of ident * type_expr
  | Tsig_instance of ident * type_expr
  | Tsig_type of type_decl
  | Tsig_convtype of type_decl * conv_type * ident * type_expr
  | Tsig_rectype of type_decl list
  | Tsig_module of ident * module_sig
  | Tsig_modtype of ident * module_sig
  | Tsig_open of path
  | Tsig_typeext of variant * ctor_decl list
  | Tsig_request of type_expr * ctor_decl
  | Tsig_multiple of signature
  | Tsig_prover of signature
  | Tsig_convert of ident * type_expr

and module_sig = {msig_desc: module_sig_desc; msig_loc: Location.t}

and module_sig_desc =
  | Tmty_sig of signature
  | Tmty_name of path
  | Tmty_alias of path
  | Tmty_abstract
  | Tmty_functor of str * module_sig * module_sig

type statement = {stmt_desc: statement_desc; stmt_loc: Location.t}

and statements = statement list

and statement_desc =
  | Tstmt_value of Asttypes.rec_flag * (pattern * expression) list
  | Tstmt_instance of ident * expression
  | Tstmt_type of type_decl
  | Tstmt_convtype of type_decl * conv_type * ident * convert
  | Tstmt_rectype of type_decl list
  | Tstmt_module of ident * module_expr
  | Tstmt_modtype of ident * module_sig
  | Tstmt_open of path
  | Tstmt_open_instance of path
  | Tstmt_typeext of variant * ctor_decl list
  | Tstmt_request of
      type_expr * ctor_decl * (pattern option * expression) option
  | Tstmt_multiple of statements
  | Tstmt_prover of statements
  | Tstmt_convert of ident * type_expr * convert

and module_expr = {mod_desc: module_desc; mod_loc: Location.t}

and module_desc =
  | Tmod_struct of statements
  | Tmod_name of path
  | Tmod_functor of str * module_sig * module_expr
