%{
open Location
open Parsetypes
open Longident

let mklocation (loc_start, loc_end) = {loc_start; loc_end; loc_ghost= false}

let mktyp ~pos d = {type_desc= d; type_id= -1; type_loc= mklocation pos}
let mkpat ~pos d = {pat_desc= d; pat_loc= mklocation pos}
let mkexp ~pos d = {exp_desc= d; exp_loc= mklocation pos}
let mkstmt ~pos d = {stmt_desc= d; stmt_loc= mklocation pos}
let mkmod ~pos d = {mod_desc= d; mod_loc= mklocation pos}
%}
%token <int> INT
%token <string> LIDENT
%token <string> UIDENT
%token FUN
%token LET
%token MODULE
%token SEMI
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token DASHGT
%token EQUALGT
%token EQUAL
%token COLON
%token COMMA
%token UNDERSCORE
%token DOT
%token EOF

%token EOL

%start file
%type <Parsetypes.statement list> file

%%

file:
  | s = structure EOF
    { s }

structure:
  | (* Empty *)
    { [] }
  | s = structure_item maybe(SEMI)
    { [s] }
  | s = structure_item SEMI rest = structure
    { s :: rest }

structure_item:
  | LET x = pat EQUAL e = expr
    { mkstmt ~pos:$loc (Value (x, e)) }
  | MODULE x = as_loc(UIDENT) EQUAL m = module_expr
    { mkstmt ~pos:$loc (Module (x, m)) }

module_expr:
  | LBRACE s = structure RBRACE
    { mkmod ~pos:$loc (Structure s) }
  | x = as_loc(longident(UIDENT, UIDENT))
    { mkmod ~pos:$loc (ModName x) }

expr:
  | x = as_loc(longident(LIDENT, UIDENT))
    { mkexp ~pos:$loc (Variable x) }
  | x = INT
    { mkexp ~pos:$loc (Int x) }
  | FUN LBRACKET f = function_from_args
    { f }
  | LBRACKET es = exprs RBRACKET
    { es }
  | LBRACKET es = tuple(expr) RBRACKET
    { mkexp ~pos:$loc (Tuple (List.rev es)) }
  | LBRACE es = block RBRACE
    { es }
  | LET x = pat EQUAL lhs = expr SEMI rhs = expr
    { mkexp ~pos:$loc (Let (x, lhs, rhs)) }
  | f = expr LBRACKET es = expr_list RBRACKET
    { mkexp ~pos:$loc (Apply (f, List.rev es)) }

expr_list:
  | e = expr
    { [e] }
  | es = expr_list COMMA e = expr
    { e :: es }

function_from_args:
  | p = pat RBRACKET EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, body)) }
  | p = pat RBRACKET typ = type_expr EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, mkexp ~pos:$loc(typ) (Constraint (body, typ)))) }
  | p = pat COMMA f = function_from_args
    { mkexp ~pos:$loc (Fun (p, f)) }

exprs:
  | e = expr
    { e }
  | e1 = expr SEMI rest = exprs
    { mkexp ~pos:$loc (Seq (e1, rest)) }

block:
  | e = expr SEMI
    { e }
  | e1 = expr SEMI rest = block
    { mkexp ~pos:$loc (Seq (e1, rest)) }

pat:
  | LBRACKET p = pat RBRACKET
    { p }
  | LBRACKET ps = tuple(pat) RBRACKET
    { mkpat ~pos:$loc (PTuple (List.rev ps)) }
  | p = pat COLON typ = type_expr
    { mkpat ~pos:$loc (PConstraint (p, typ)) }
  | x = as_loc(LIDENT)
    { mkpat ~pos:$loc (PVariable x) }

simple_type_expr:
  | UNDERSCORE
    { mktyp ~pos:$loc (Tvar (None, 0)) }
  | x = as_loc(LIDENT)
    { mktyp ~pos:$loc (Tctor x) }
  | LBRACKET x = type_expr RBRACKET
    { x }
  | LBRACKET xs = tuple(type_expr) RBRACKET
    { mktyp ~pos:$loc (Ttuple (List.rev xs)) }

type_expr:
  | x = simple_type_expr
    { x }
  | x = simple_type_expr DASHGT y = type_expr
    { mktyp ~pos:$loc (Tarrow (x, y)) }

tuple(X):
  | xs = tuple(X) COMMA x = X
    { x :: xs }
  | x1 = X COMMA x2 = X
    { [x2; x1] }

%inline as_loc(X): x = X
  { mkloc x (mklocation ($symbolstartpos, $endpos)) }

%inline maybe(X):
  | (* empty *)
    { None }
  | x = X
    { Some x }

longident(X, M):
  | x = X
    { Lident x }
  | path = longident(M, M) DOT x = X
    { Ldot (path, x) }
