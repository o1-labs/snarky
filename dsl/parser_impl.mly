%{
open Location
open Parsetypes

let mkrhs rhs pos = mkloc rhs (rhs_loc pos)
%}
%token <int> INT
%token <string> VAR
%token FUN
%token LET
%token SEMI
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
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

expr:
  | x = VAR
    { Variable (mkrhs x 1) }
  | x = INT
    { Int x }
  | FUN LBRACKET x = args RBRACKET typ = opt_type_constraint EQUALGT LBRACE body = block RBRACE
    { Fun (x, typ, body) }
  | LBRACKET es = exprs RBRACKET
    { es }
  | LBRACE es = block RBRACE
    { es }
  | LET x = pat EQUAL lhs = expr SEMI rhs = expr
    { Let (x, lhs, rhs) }
  | f = expr LBRACKET es = expr_list RBRACKET
    { Apply (f, List.rev es) }

expr_list:
  | e = expr
    { [e] }
  | es = expr_list_multiple
    { es }

expr_list_multiple:
  | es = expr_list_multiple COMMA e = expr
    { e :: es }
  | e1 = expr COMMA e2 = expr
    { [e2; e1] }

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

args: 
  | x = arg
    { [x] }
  | x = arg COMMA rest = args
    { (x :: rest) }

arg:
  | x = pat typ = opt_type_constraint
    { (x, typ) }

opt_type_constraint:
  /* empty */
  { None }
  | COLON typ = type_expr
  { Some typ }

pat:
  | x = VAR
    { PVariable (mkrhs x 1) }

type_expr:
  | UNDERSCORE
    { TAny }
  | x = VAR
    { TVariable (mkrhs x 1) }
