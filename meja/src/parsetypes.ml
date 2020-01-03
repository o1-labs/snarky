open Ast_types

type type_expr = {type_desc: type_desc; type_loc: Location.t}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Ptyp_var of str option
  | Ptyp_tuple of type_expr list
  | Ptyp_arrow of type_expr * type_expr * explicitness * Asttypes.arg_label
  (* A type name. *)
  | Ptyp_ctor of variant
  | Ptyp_poly of type_expr list * type_expr
  | Ptyp_prover of type_expr
  | Ptyp_conv of type_expr * type_expr
  | Ptyp_opaque of type_expr
  | Ptyp_alias of type_expr * str
  | Ptyp_row of
      row_tag list
      * (* [Closed] if the row_field list is an upper bound,
           [Open] if the row_field list is a lower bound.
        *)
        closed_flag
      * (* The lower bound of the row, if it differs from the fields in the
           row_field list.
        *)
      str list option
  | Ptyp_row_subtract of type_expr * str list

and variant = {var_ident: lid; var_params: type_expr list}

and row_tag = {rtag_ident: str; rtag_arg: type_expr list; rtag_loc: Location.t}

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
  ; tdec_desc: type_decl_desc
  ; tdec_loc: Location.t }

and type_decl_desc =
  | Pdec_abstract
  | Pdec_alias of type_expr
  | Pdec_record of field_decl list
  | Pdec_variant of ctor_decl list
  | Pdec_open
  | Pdec_extend of Path.t Location.loc * ctor_decl list
      (** Internal; this should never be present in the AST. *)

type pattern = {pat_desc: pattern_desc; pat_loc: Location.t}

and pattern_desc =
  | Ppat_any
  | Ppat_variable of str
  | Ppat_constraint of pattern * type_expr
  | Ppat_tuple of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_integer of string * char option
      (** Delayed parsing of integers so that we can complain about the right
          bounds mismatch if they overflow.
      *)
  | Ppat_literal of literal
  | Ppat_record of (lid * pattern) list
  | Ppat_ctor of lid * pattern option
  | Ppat_row_ctor of str * pattern list

type expression = {exp_desc: expression_desc; exp_loc: Location.t}

and expression_desc =
  | Pexp_apply of expression * (Asttypes.arg_label * expression) list
  | Pexp_variable of lid
  | Pexp_integer of string * char option
      (** Delayed parsing of integers so that we can complain about the right
          bounds mismatch if they overflow.
      *)
  | Pexp_literal of literal
  | Pexp_fun of Asttypes.arg_label * pattern * expression * explicitness
  | Pexp_newtype of str * expression
  | Pexp_seq of expression * expression
  | Pexp_let of pattern * expression * expression
  | Pexp_instance of str * expression * expression
  | Pexp_constraint of expression * type_expr
  | Pexp_tuple of expression list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_field of expression * lid
  | Pexp_record of (lid * expression) list * expression option
  | Pexp_ctor of lid * expression option
  | Pexp_row_ctor of str * expression list
  | Pexp_unifiable of
      { mutable expression: expression option
      ; name: str
      ; id: int }
  | Pexp_if of expression * expression * expression option
  | Pexp_prover of expression

type conv_type =
  (* Other mode stitched declaration. *)
  | Ptconv_with of mode * type_decl
  (* Tri-stitching to existing declaration. *)
  | Ptconv_to of type_expr

type signature_item = {sig_desc: signature_desc; sig_loc: Location.t}

and signature = signature_item list

and signature_desc =
  | Psig_value of str * type_expr
  | Psig_instance of str * type_expr
  | Psig_type of type_decl
  | Psig_convtype of type_decl * conv_type * str option
  | Psig_rectype of type_decl list
  | Psig_module of str * module_sig
  | Psig_modtype of str * module_sig
  | Psig_open of lid
  | Psig_typeext of variant * ctor_decl list
  | Psig_request of type_expr * ctor_decl
  | Psig_multiple of signature
  | Psig_prover of signature
  | Psig_convert of str * type_expr

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
  | Pstmt_convtype of type_decl * conv_type * str option
  | Pstmt_rectype of type_decl list
  | Pstmt_module of str * module_expr
  | Pstmt_modtype of str * module_sig
  | Pstmt_open of lid
  | Pstmt_open_instance of lid
  | Pstmt_typeext of variant * ctor_decl list
  | Pstmt_request of
      type_expr * ctor_decl * (pattern option * expression) option
  | Pstmt_multiple of statements
  | Pstmt_prover of statements
  | Pstmt_convert of str * type_expr

and module_expr = {mod_desc: module_desc; mod_loc: Location.t}

and module_desc =
  | Pmod_struct of statements
  | Pmod_name of lid
  | Pmod_functor of str * module_sig * module_expr
