open Core_kernel
open Ast_types

type type_expr = {type_desc: type_desc; type_id: int; type_loc: Location.t}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of str option * explicitness
  | Ttuple of type_expr list
  | Tarrow of type_expr * type_expr * explicitness * Asttypes.arg_label
  (* A type name. *)
  | Tctor of variant
  | Tpoly of type_expr list * type_expr

and variant =
  { var_ident: lid
  ; var_params: type_expr list
  ; var_implicit_params: type_expr list }

type field_decl =
  {fld_ident: str; fld_type: type_expr; fld_loc: Location.t}

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
  | TExtend of lid * Type0.type_decl * ctor_decl list
      (** Internal; this should never be present in the AST. *)
  | TForward of int option ref
      (** Forward declaration for types loaded from cmi files. *)

type pattern =
  {pat_desc: pattern_desc; pat_loc: Location.t; pat_type: Type0.type_expr}

and pattern_desc =
  | PAny
  | PVariable of str
  | PConstraint of pattern * type_expr
  | PTuple of pattern list
  | POr of pattern * pattern
  | PInt of int
  | PRecord of (lid * pattern) list
  | PCtor of lid * pattern option

type expression =
  {exp_desc: expression_desc; exp_loc: Location.t; exp_type: Type0.type_expr}

and expression_desc =
  | Apply of expression * (Asttypes.arg_label * expression) list
  | Variable of lid
  | Int of int
  | Fun of Asttypes.arg_label * pattern * expression * explicitness
  | Seq of expression * expression
  | Let of pattern * expression * expression
  | Constraint of expression * type_expr
  | Tuple of expression list
  | Match of expression * (pattern * expression) list
  | Field of expression * lid
  | Record of (lid * expression) list * expression option
  | Ctor of lid * expression option
  | Unifiable of {mutable expression: expression option; name: str; id: int}

type signature_item = {sig_desc: signature_desc; sig_loc: Location.t}

and signature_desc =
  | SValue of str * type_expr
  | SInstance of str * type_expr
  | STypeDecl of type_decl
  | SModule of str * module_sig
  | SModType of str * module_sig

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
  | Open of lid
  | TypeExtension of variant * ctor_decl list
  | Request of type_expr * ctor_decl * (pattern option * expression) option
  | Multiple of statement list

and module_expr = {mod_desc: module_desc; mod_loc: Location.t}

and module_desc =
  | Structure of statement list
  | ModName of lid
  | Functor of str * module_sig * module_expr

let rec typ_debug_print fmt typ =
  let open Format in
  let print i = fprintf fmt i in
  let print_comma fmt () = pp_print_char fmt ',' in
  let print_list pp = pp_print_list ~pp_sep:print_comma pp in
  let print_label fmt = function
    | Asttypes.Nolabel ->
        ()
    | Asttypes.Labelled str ->
        fprintf fmt "~%s:" str
    | Asttypes.Optional str ->
        fprintf fmt "?%s:" str
  in
  print "(%i:" typ.type_id ;
  ( match typ.type_desc with
  | Tvar (None, Explicit) ->
      print "var _"
  | Tvar (Some name, Explicit) ->
      print "var %s" name.txt
  | Tvar (None, Implicit) ->
      print "implicit_var _"
  | Tvar (Some name, Implicit) ->
      print "implicit_var %s" name.txt
  | Tpoly (typs, typ) ->
      print "poly [%a] %a"
        (print_list typ_debug_print)
        typs typ_debug_print typ
  | Tarrow (typ1, typ2, Explicit, label) ->
      print "%a%a -> %a" print_label label typ_debug_print typ1 typ_debug_print
        typ2
  | Tarrow (typ1, typ2, Implicit, label) ->
      print "%a{%a} -> %a" print_label label typ_debug_print typ1
        typ_debug_print typ2
  | Tctor {var_ident= name; var_params= params; _} ->
      print "%a (%a)" Longident.pp name.txt (print_list typ_debug_print) params
  | Ttuple typs ->
      print "(%a)" (print_list typ_debug_print) typs ) ;
  print ")"
