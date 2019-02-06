type str = string Location.loc

type type_expr = {type_desc: type_desc; type_id: int}

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of str option
  | Tarrow of type_expr * type_expr
  (* A type name. *)
  | Tconstr of str

type pattern = PVariable of str | PConstraint of pattern * type_expr

type expression =
  | Apply of expression * expression list
  | Variable of str
  | Int of int
  | Fun of pattern * expression
  | Seq of expression * expression
  | Let of pattern * expression * expression
  | Constraint of expression * type_expr

type statement = Value of pattern * expression
