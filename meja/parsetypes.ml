type str = string Location.loc

type type_expr = {type_desc: type_desc; type_id: int; type_loc: Location.t}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of str option * (* depth *) int
  | Ttuple of type_expr list
  | Tarrow of type_expr * type_expr
  (* A type name. *)
  | Tctor of variant
  | Tpoly of type_expr list * type_expr

and variant = {var_ident: str; var_params: type_expr list; var_decl_id: int}

type field_decl =
  {fld_ident: str; fld_type: type_expr; fld_id: int; fld_loc: Location.t}

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
  ; tdec_id: int
  ; tdec_loc: Location.t }

and type_decl_desc =
  | TAbstract
  | TAlias of type_expr
  | TRecord of field_decl list
  | TVariant of ctor_decl list

type pattern = {pat_desc: pattern_desc; pat_loc: Location.t}

and pattern_desc =
  | PVariable of str
  | PConstraint of pattern * type_expr
  | PTuple of pattern list

type expression = {exp_desc: expression_desc; exp_loc: Location.t}

and expression_desc =
  | Apply of expression * expression list
  | Variable of str
  | Int of int
  | Fun of pattern * expression
  | Seq of expression * expression
  | Let of pattern * expression * expression
  | Constraint of expression * type_expr
  | Tuple of expression list

type statement = {stmt_desc: statement_desc; stmt_loc: Location.t}

and statement_desc = Value of pattern * expression | TypeDecl of type_decl
