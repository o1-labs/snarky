type str = string Location.loc

type pattern = PVariable of str

type typ = TAny | TVariable of str

type expression =
  | Apply of expression * expression list
  | Variable of str
  | Int of int
  | Fun of (pattern * typ option) list * typ option * expression
  | Seq of expression * expression
  | Let of pattern * expression * expression

type statement = Value of pattern * expression
