%{
open Location
open Parsetypes

let mkrhs rhs pos = mkloc rhs (rhs_loc pos)

let mktyp d = {type_desc= d; type_id= -1; type_loc= none}
let mkpat d = {pat_desc= d; pat_loc= none}
let mkexp d = {exp_desc= d; exp_loc= none}
let mkstmt d = {stmt_desc= d; stmt_loc= none}
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
    { mkstmt (Value (x, e)) }

expr:
  | x = LIDENT
    { mkexp (Variable (mkrhs x 1)) }
  | x = INT
    { mkexp (Int x) }
  | FUN LBRACKET f = function_from_args
    { f }
  | LBRACKET es = exprs RBRACKET
    { es }
  | LBRACE es = block RBRACE
    { es }
  | LET x = pat EQUAL lhs = expr SEMI rhs = expr
    { mkexp (Let (x, lhs, rhs)) }
  | f = expr LBRACKET es = expr_list RBRACKET
    { mkexp (Apply (f, List.rev es)) }

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

function_from_args:
  | p = pat RBRACKET EQUALGT LBRACE body = block RBRACE
    { mkexp (Fun (p, body)) }
  | p = pat RBRACKET typ = type_expr EQUALGT LBRACE body = block RBRACE
    { mkexp (Fun (p, mkexp (Constraint (body, typ)))) }
  | p = pat COMMA f = function_from_args
    { mkexp (Fun (p, f)) }

exprs:
  | e = expr
    { e }
  | e1 = expr SEMI rest = exprs
    { mkexp (Seq (e1, rest)) }

block:
  | e = expr SEMI
    { e }
  | e1 = expr SEMI rest = block
    { mkexp (Seq (e1, rest)) }

pat:
  | LBRACKET p = pat RBRACKET
    { p }
  | p = pat COLON typ = type_expr
    { mkpat (PConstraint (p, typ)) }
  | x = LIDENT
    { mkpat (PVariable (mkrhs x 1)) }

simple_type_expr:
  | UNDERSCORE
    { mktyp (Tvar (None, 0)) }
  | x = LIDENT
    { mktyp (Tconstr (mkrhs x 1)) }
  | LBRACKET x = type_expr RBRACKET
    { x }

type_expr:
  | x = simple_type_expr
    { x }
  | x = simple_type_expr DASHGT y = type_expr
    { mktyp (Tarrow (x, y)) }
