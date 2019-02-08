open Core_kernel

module Longident = struct
  include Longident

  let rec compare lid1 lid2 =
    let nonzero_or x f = if Int.equal x 0 then f () else x in
    match (lid1, lid2) with
    | Lident name1, Lident name2 -> String.compare name1 name2
    | Ldot (lid1, name1), Ldot (lid2, name2) ->
        nonzero_or (String.compare name1 name2) (fun () -> compare lid1 lid2)
    | Lapply (lid1a, lid1b), Lapply (lid2a, lid2b) ->
        nonzero_or (compare lid1a lid2a) (fun () -> compare lid1b lid2b)
    | Lident _, _ -> -1
    | _, Lident _ -> 1
    | Ldot _, _ -> -1
    | _, Ldot _ -> 1

  let rec sexp_of_t lid =
    match lid with
    | Lident name -> Sexp.Atom name
    | Ldot (lid, name) -> Sexp.List [sexp_of_t lid; Atom name]
    | Lapply (lid1, lid2) -> Sexp.List [sexp_of_t lid1; sexp_of_t lid2]

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare

    let sexp_of_t = sexp_of_t
  end)

  let rec show () lid =
    match lid with
    | Lident name -> name
    | Ldot (lid, name) -> sprintf "%a.%s" show lid name
    | Lapply (lid1, lid2) -> sprintf "%a(%a)" show lid1 show lid2
end

type str = string Location.loc

type lid = Longident.t Location.loc

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

type pattern = {pat_desc: pattern_desc; pat_loc: Location.t}

and pattern_desc =
  | PVariable of str
  | PConstraint of pattern * type_expr
  | PTuple of pattern list

type expression = {exp_desc: expression_desc; exp_loc: Location.t}

and expression_desc =
  | Apply of expression * expression list
  | Variable of lid
  | Int of int
  | Fun of pattern * expression
  | Seq of expression * expression
  | Let of pattern * expression * expression
  | Constraint of expression * type_expr
  | Tuple of expression list

type statement = {stmt_desc: statement_desc; stmt_loc: Location.t}

and statement_desc =
  | Value of pattern * expression
  | TypeDecl of type_decl
  | Module of str * module_expr

and module_expr = {mod_desc: module_desc; mod_loc: Location.t}

and module_desc = Structure of statement list | ModName of lid
