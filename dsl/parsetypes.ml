open Sexplib.Std

type pattern = PVariable of string [@@deriving sexp]

type typ = TAny | TVariable of string [@@deriving sexp]

type expression =
| Apply of expression * expression
| Variable of string
| Int of int
| Fun of (pattern * typ option) list * typ option * expression
| Seq of expression * expression
| Let of pattern * expression * expression [@@deriving sexp]

type statement = Value of pattern * expression [@@deriving sexp]
