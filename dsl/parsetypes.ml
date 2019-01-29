open Core_kernel

type position = Lexing.position =
  {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}
[@@deriving show]

type loc = Location.t =
  {loc_start: position; loc_end: position; loc_ghost: bool}
[@@deriving show]

type 'a loc' = 'a Location.loc = {txt: 'a; loc: loc} [@@deriving show]

type str = string loc' [@@deriving show]

type type_expr = {mutable type_desc: type_desc; id: int; type_loc: loc}
[@@deriving show]

and type_desc =
  (* A type variable. Name is None when not yet chosen. *)
  | Tvar of str option
  | Tarrow of type_expr * type_expr
  (* A type name. *)
  | Tconstr of str
  (* Internal, used to wrap a reference to a type. *)
  | Tdefer of type_expr
  (* Internal, used as a hint to copy before modifying. *)
  | Tcopy of type_expr * int
  (* Internal, used as a hint to stop copying. *)
  | Tnocopy of type_expr * int
[@@deriving show]

module Type = struct
  let id = ref 0

  let mk ?(loc = Location.none) type_desc =
    incr id ;
    {type_desc; id= !id; type_loc= loc}
end

type pattern = {pat_desc: pat_desc; pat_loc: loc} [@@deriving show]

and pat_desc = PVariable of str | PConstraint of pattern * type_expr
[@@deriving show]

module Pattern = struct
  let mk ?(loc = Location.none) pat_desc = {pat_desc; pat_loc= loc}
end

type expression = {exp_desc: exp_desc; exp_loc: loc} [@@deriving show]

and exp_desc =
  | Apply of expression * expression list
  | Variable of str
  | Int of int
  | Fun of pattern * expression
  | Seq of expression * expression
  | Let of pattern * expression * expression
  | Constraint of expression * type_expr
[@@deriving show]

module Expression = struct
  let mk ?(loc = Location.none) exp_desc = {exp_desc; exp_loc= loc}
end

type statement = {stmt_desc: stmt_desc; stmt_loc: loc} [@@deriving show]

and stmt_desc = Value of pattern * expression [@@deriving show]

module Statement = struct
  let mk ?(loc = Location.none) stmt_desc = {stmt_desc; stmt_loc= loc}
end
