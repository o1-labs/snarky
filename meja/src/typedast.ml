open Ast_types
open Parsetypes

type literal = Int of int | Bool of bool | Field of string | String of string

type pattern =
  {pat_desc: pattern_desc; pat_loc: Location.t; pat_type: Type0.type_expr}

and pattern_desc =
  | Tpat_any
  | Tpat_variable of str
  | Tpat_constraint of pattern * type_expr
  | Tpat_tuple of pattern list
  | Tpat_or of pattern * pattern
  | Tpat_int of int
  | Tpat_record of (lid * pattern) list
  | Tpat_ctor of lid * pattern option

type expression =
  {exp_desc: expression_desc; exp_loc: Location.t; exp_type: Type0.type_expr}

and expression_desc =
  | Apply of expression * (Asttypes.arg_label * expression) list
  | Variable of lid
  | Literal of literal
  | Fun of Asttypes.arg_label * pattern * expression * explicitness
  | Newtype of str * expression
  | Seq of expression * expression
  | Let of pattern * expression * expression
  | Constraint of expression * type_expr
  | Tuple of expression list
  | Match of expression * (pattern * expression) list
  | Field of expression * lid
  | Record of (lid * expression) list * expression option
  | Ctor of lid * expression option
  | Unifiable of {mutable expression: expression option; name: str; id: int}
  | If of expression * expression * expression option

type signature_item = {sig_desc: signature_desc; sig_loc: Location.t}

and signature_desc =
  | SValue of str * type_expr
  | SInstance of str * type_expr
  | STypeDecl of type_decl
  | SModule of str * module_sig
  | SModType of str * module_sig
  | SOpen of lid
  | STypeExtension of variant * ctor_decl list
  | SRequest of type_expr * ctor_decl
  | SMultiple of signature_item list

and module_sig = {msig_desc: module_sig_desc; msig_loc: Location.t}

and module_sig_desc =
  | Signature of signature_item list
  | SigName of lid
  | SigAbstract
  | SigFunctor of str * module_sig * module_sig

type statement = {stmt_desc: statement_desc; stmt_loc: Location.t}

and statement_desc =
  | Value of pattern * expression
  | Instance of str * expression
  | TypeDecl of type_decl
  | Module of str * module_expr
  | ModType of str * module_sig
  | Open of lid
  | TypeExtension of variant * ctor_decl list
  | Request of type_expr * ctor_decl * (pattern option * expression) option
  | Multiple of statement list

and module_expr = {mod_desc: module_desc; mod_loc: Location.t}

and module_desc =
  | Structure of statement list
  | ModName of lid
  | Functor of str * module_sig * module_expr
