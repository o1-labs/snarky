open Core_kernel

type position = Lexing.position =
  {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}

type loc = Location.t =
  {loc_start: position; loc_end: position; loc_ghost: bool}

let show_loc x = Format.asprintf "%a" Location.print_loc x

let pp_loc f x = Format.pp_print_string f (show_loc x)

type 'a loc' = 'a Location.loc = {txt: 'a; loc: loc} [@@deriving show]

type str = string loc' [@@deriving show]

type type_expr =
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
  | Tconstr of str
  | Ttuple of type_expr list
  (* Internal, used to wrap a reference to a type. *)
  | Tdefer of type_expr
[@@deriving show]

and type_var =
  {name: str option; depth: int;
    mutable instance: type_expr option
    [@printer fun fmt expr ->
      match expr with
      | Some expr ->
        if expr.in_recursion then
          Format.pp_print_string fmt "Some <self-recursive>"
        else
          (expr.in_recursion <- true;
          Format.pp_print_string fmt "Some";
          pp_type_expr fmt expr;
          expr.in_recursion <- false)
      | None -> Format.pp_print_string fmt "None"]
  }
[@@deriving show]

module Type = struct
  let id = ref 0

  let mk ?(loc = Location.none) type_desc =
    incr id ;
    {type_desc; id= !id; type_loc= loc; in_recursion= false}

  let mk_var ?loc ?(depth = -1) name =
    mk ?loc (Tvar {name; depth; instance= None})

  module T = struct
    type t = type_expr

    let compare {id= id1; _} {id= id2; _} = -Int.compare id1 id2

    let sexp_of_t _ = Sexp.List []
  end

  include T
  include Comparator.Make (T)
end

type pattern = {pat_desc: pat_desc; pat_loc: loc} [@@deriving show]

and pat_desc =
  | PVariable of str
  | PConstraint of {pcon_pat: pattern; mutable pcon_typ: type_expr}
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
  | Constraint of {econ_exp: expression; mutable econ_typ: type_expr}
  | Tuple of expression list
[@@deriving show]

module Expression = struct
  let mk ?(loc = Location.none) exp_desc = {exp_desc; exp_loc= loc}
end

type statement = {stmt_desc: stmt_desc; stmt_loc: loc} [@@deriving show]

and stmt_desc = Value of pattern * expression [@@deriving show]

module Statement = struct
  let mk ?(loc = Location.none) stmt_desc = {stmt_desc; stmt_loc= loc}
end
