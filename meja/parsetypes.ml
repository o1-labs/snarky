type str = string Location.loc

type type_expr = {type_desc: type_desc; type_id: int; type_loc: Location.t}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of str option * (* depth *) int
  | Ttuple of type_expr list
  | Tarrow of type_expr * type_expr
  (* A type name. *)
  | Tctor of str
  | Tpoly of type_expr list * type_expr

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

and statement_desc = Value of pattern * expression
