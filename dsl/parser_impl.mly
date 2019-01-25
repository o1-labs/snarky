%{
open Location
open Parsetypes

let mkrhs rhs pos = mkloc rhs (rhs_loc pos)
%}
%token <int> INT
%token <string> LIDENT
%token <string> UIDENT
%token FUN
%token LET
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
%token EOF

%token EOL

%start file
%type <Parsetypes.statement list> file

%%

file:
  | EOF (* Empty *)
    { [] }
  | s = structure_item EOF
    { [s] }
  | s = structure_item SEMI rest = file
    { s :: rest }

structure_item:
  | LET x = pat EQUAL e = expr
    { Value (x, e) }

simple_expr:
  | x = LIDENT
    { Variable (mkrhs x 1) }
  | x = INT
    { Int x }
  | FUN LBRACKET f = function_from_args
    { f }
  | LBRACKET es = exprs RBRACKET
    { es }
  | LBRACE es = block RBRACE
    { es }
  | LET x = pat EQUAL lhs = expr SEMI rhs = expr
    { Let (x, lhs, rhs) }

expr:
  | x = simple_expr
    { x }
  | f = simple_expr xs = simple_expr_list
    { Apply (f, List.rev xs) }

simple_expr_list:
  | x = simple_expr
    { [x] }
  | l = simple_expr_list x = simple_expr
    { x :: l }

function_from_args:
  | p = pat RBRACKET typ = opt_type_constraint EQUALGT LBRACE body = block RBRACE
    { Fun (p, typ, body) }
  | p = pat COMMA f = function_from_args
    { Fun (p, None, f) }

exprs:
  | e = expr
    { e }
  | e1 = expr SEMI rest = exprs
    { Seq (e1, rest) }

block:
  | e = expr SEMI
    { e }
  | e1 = expr SEMI rest = block
    { Seq (e1, rest) }

opt_type_constraint:
  /* empty */
  { None }
  | COLON typ = type_expr
  { Some typ }

pat:
  | LBRACKET p = pat RBRACKET
    { p }
  | p = pat COLON typ = type_expr
    { PConstraint (p, typ) }
  | x = LIDENT
    { PVariable (mkrhs x 1) }

simple_type_expr:
  | UNDERSCORE
    { TAny }
  | x = LIDENT
    { TVariable (mkrhs x 1) }
  | LBRACKET x = type_expr RBRACKET
    { x }

type_expr:
  | x = simple_type_expr
    { x }
  | x = simple_type_expr DASHGT y = type_expr
    { TArrow (x, y) }
