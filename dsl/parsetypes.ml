type str = string Location.loc

type type_expr = {mutable desc: type_desc; id: int}

and type_desc =
  | Tvar of str option
  (* A type variable. Name is None when not yet chosen. *)
  | Tarrow of type_expr * type_expr
  | Tconstr of str
  (* A type name. *)
  | Tdefer of type_expr

(* Internal, used to wrap a reference to a type. *)

module Type = struct
  let id = ref 0

  let mk desc = incr id ; {desc; id= !id}
end

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
