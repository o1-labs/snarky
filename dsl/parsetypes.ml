open Core_kernel

type position = Lexing.position =
  {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}

type loc = Location.t =
  {loc_start: position; loc_end: position; loc_ghost: bool}

let show_loc x = Format.asprintf "%a" Location.print_loc x

let pp_loc f x = Format.pp_print_string f (show_loc x)

type 'a loc' = 'a Location.loc = {txt: 'a; loc: loc} [@@deriving show]

type str = string loc' [@@deriving show]

type type_decl =
  { type_decl_desc: type_decl_desc
  ; type_decl_id: int
  ; type_decl_loc: loc
  ; mutable type_decl_in_recursion: bool }
[@@deriving show]

and type_decl_desc =
  | Abstract
  | Alias of type_expr
  | Record of field_decl list
  | Variant of constr_decl list
  | VariantRecord of field_decl list
[@@deriving show]

and field_decl =
  { field_ident: str
  ; field_type: type_expr
         [@printer
           fun fmt expr ->
             if expr.in_recursion then
               Format.pp_print_string fmt "Some <self-recursive>"
             else (
               expr.in_recursion <- true ;
               Format.pp_print_string fmt "Some" ;
               pp_type_expr fmt expr ;
               expr.in_recursion <- false )]
  ; field_loc: loc }
[@@deriving show]

and constr_decl =
  { constr_decl_ident: str
  ; constr_decl_args: constr_args
  ; constr_decl_return: type_expr option
  ; constr_decl_loc: loc }
[@@deriving show]

and constr_args =
  | Constr_tuple of type_expr list
  | Constr_record of field_decl list
[@@deriving show]

and type_expr =
  { mutable type_desc: type_desc
  ; id: int
  ; type_loc: loc
  ; mutable in_recursion: bool }
[@@deriving show]

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of type_var
  | Tpoly of type_expr (* A [Tvar] *) * type_expr
  | Tarrow of type_expr * type_expr
  (* A type name. *)
  | Tconstr of type_constr
  | Ttuple of type_expr list
  (* Internal, used to wrap a reference to a type. *)
  | Tdefer of type_expr
[@@deriving show]

and type_var =
  { name: str option
  ; depth: int
  ; mutable instance: type_expr option
         [@printer
           fun fmt expr ->
             match expr with
             | Some expr ->
                 if expr.in_recursion then
                   Format.pp_print_string fmt "Some <self-recursive>"
                 else (
                   expr.in_recursion <- true ;
                   Format.pp_print_string fmt "Some" ;
                   pp_type_expr fmt expr ;
                   expr.in_recursion <- false )
             | None -> Format.pp_print_string fmt "None"] }
[@@deriving show]

and type_constr =
  { constr_ident: str
  ; mutable constr_type_decl: type_decl
         [@printer
           fun fmt decl ->
             if decl.type_decl_in_recursion then
               Format.pp_print_string fmt "Some <self-recursive>"
             else (
               decl.type_decl_in_recursion <- true ;
               Format.pp_print_string fmt "Some" ;
               pp_type_decl fmt decl ;
               decl.type_decl_in_recursion <- false )] }
[@@deriving show]

module TypeDecl = struct
  let id = ref 0

  let mk ?(loc = Location.none) type_decl_desc =
    incr id ;
    { type_decl_desc
    ; type_decl_id= !id
    ; type_decl_loc= loc
    ; type_decl_in_recursion= false }

  let mk_constructor ?(loc = Location.none) ?return name args =
    { constr_decl_ident= Location.mkloc name loc
    ; constr_decl_args= args
    ; constr_decl_return= return
    ; constr_decl_loc= loc }
end

module Type = struct
  let id = ref 0

  let mk ?(loc = Location.none) type_desc =
    incr id ;
    {type_desc; id= !id; type_loc= loc; in_recursion= false}

  let mk_var ?loc ~depth name = mk ?loc (Tvar {name; depth; instance= None})

  let mk_constr' ?loc ~decl constr_ident =
    mk ?loc (Tconstr {constr_ident; constr_type_decl= decl})

  let mk_constr ?loc ?(decl = Abstract) constr_ident =
    mk_constr' ?loc ~decl:(TypeDecl.mk ?loc decl) constr_ident

  module T = struct
    type t = type_expr

    let compare {id= id1; _} {id= id2; _} = -Int.compare id1 id2

    let sexp_of_t _ = Sexp.List []
  end

  include T
  include Comparator.Make (T)
end

type constant = Parsetree.constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * string option
  | Pconst_float of string * char option
[@@deriving show]

type closed_flag = Asttypes.closed_flag = Closed | Open [@@deriving show]

type pattern = {pat_desc: pat_desc; pat_loc: loc} [@@deriving show]

and pat_desc =
  | PAny
  | PConstant of constant
  | PVariable of str
  | PConstraint of {pcon_pat: pattern; mutable pcon_typ: type_expr}
  | PRecord of (str * pattern) list * closed_flag
  | POr of pattern * pattern
  | PTuple of pattern list
[@@deriving show]

module Pattern = struct
  let mk ?(loc = Location.none) pat_desc = {pat_desc; pat_loc= loc}
end

type expression = {exp_desc: exp_desc; exp_loc: loc} [@@deriving show]

and exp_desc =
  | Apply of expression * expression list
  | Variable of str
  | Constant of constant
  | Fun of pattern * expression
  | Seq of expression * expression
  | Let of pattern * expression * expression
  | Constraint of {econ_exp: expression; mutable econ_typ: type_expr}
  | Tuple of expression list
  | Record_literal of record_contents
  | Field of expression * str
  | Match of expression * (pattern * expression) list
[@@deriving show]

and record_contents =
  {record_values: expression list; record_fields: field_decl list}

module Expression = struct
  let mk ?(loc = Location.none) exp_desc = {exp_desc; exp_loc= loc}
end

type statement = {stmt_desc: stmt_desc; stmt_loc: loc} [@@deriving show]

and stmt_desc = Value of pattern * expression | Type of str * type_decl
[@@deriving show]

module Statement = struct
  let mk ?(loc = Location.none) stmt_desc = {stmt_desc; stmt_loc= loc}
end
