%{
open Location
open Parsetypes

let mklocation (loc_start, loc_end) = {loc_start; loc_end; loc_ghost= false}

let mkrhs rhs pos = mkloc rhs (mklocation pos)

let mktyp ~pos d = Type.mk ~loc:(mklocation pos) d
let mktypvar ~pos d = Type.mk_var ~loc:(mklocation pos) d
let mkpat ~pos d = Pattern.mk ~loc:(mklocation pos) d
let mkexp ~pos d = Expression.mk ~loc:(mklocation pos) d
let mkstr ~pos d = Statement.mk ~loc:(mklocation pos) d
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
%token QUOT
%token UNDERSCORE
%token EOF

%token EOL

%left SEMI
%nonassoc below_COMMA
%left COMMA
%nonassoc below_EXP
%nonassoc LIDENT LET LBRACKET LBRACE INT FUN

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
  | bind = let_binding
    { let (x, e) = bind in
      mkstr ~pos:$loc (Value (x, e)) }

simple_expr:
  | x = LIDENT
    { mkexp ~pos:$loc (Variable (mkrhs x $loc(x))) }
  | x = INT
    { mkexp ~pos:$loc (Int x) }
  | FUN LBRACKET f = function_from_args
    { f }
  | LBRACKET es = exprs RBRACKET
    { es }
  | LBRACKET RBRACKET
    { mkexp ~pos:$loc (Tuple []) }
  | LBRACE es = block RBRACE
    { es }
  | bind = let_binding SEMI rhs = expr
    { let (x, lhs) = bind in
      mkexp ~pos:$loc (Let (x, lhs, rhs)) }

expr:
  | x = simple_expr %prec below_EXP
    { x }
  | f = simple_expr xs = simple_expr_list
    { mkexp ~pos:$loc (Apply (f, List.rev xs)) }
  | rev = expr_comma_list %prec below_COMMA
    { mkexp ~pos:$loc (Tuple (List.rev rev)) }

let_binding:
  | LET x = pat EQUAL e = expr
    { (x, e) }

simple_expr_list:
  | x = simple_expr
    { [x] }
  | l = simple_expr_list x = simple_expr
    { x :: l }

expr_comma_list:
  | l = expr_comma_list COMMA e = expr
    { e :: l }
  | e1 = expr COMMA e2 = expr
    { [e2; e1] }

function_from_args:
  | p = pat RBRACKET EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, body)) }
  | p = pat RBRACKET typ = type_expr EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, mkexp ~pos:$loc (Constraint {econ_exp= body; econ_typ= typ}))) }
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
  | p = pat COLON typ = type_expr
    { mkpat ~pos:$loc (PConstraint {pcon_pat= p; pcon_typ= typ}) }
  | x = LIDENT
    { mkpat ~pos:$loc (PVariable (mkrhs x $loc(x))) }

simple_type_expr:
  | UNDERSCORE
    { mktypvar ~pos:$loc None }
  | QUOT x = LIDENT
    { mktypvar ~pos:$loc (Some (mkrhs x $loc(x))) }
  | x = LIDENT
    { mktyp ~pos:$loc (Tconstr (mkrhs x $loc(x))) }
  | LBRACKET x = type_expr RBRACKET
    { x }

type_expr:
  | x = simple_type_expr
    { x }
  | x = simple_type_expr DASHGT y = type_expr
    { mktyp ~pos:$loc (Tarrow (x, y)) }
