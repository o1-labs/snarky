type str = string Location.loc

type typ = TAny | TVariable of str | TArrow of typ * typ

type pattern =
| PVariable of str
| PConstraint of pattern * typ

type expression =
  | Apply of expression * expression list
  | Variable of str
  | Int of int
  | Fun of pattern list * typ option * expression
  | Seq of expression * expression
  | Let of pattern * expression * expression

type statement = Value of pattern * expression
