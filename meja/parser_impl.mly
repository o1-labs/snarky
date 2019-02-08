%{
open Location
open Parsetypes

let mklocation (loc_start, loc_end) = {loc_start; loc_end; loc_ghost= false}

let mktyp ~pos d = {type_desc= d; type_id= -1; type_loc= mklocation pos}
let mkpat ~pos d = {pat_desc= d; pat_loc= mklocation pos}
let mkexp ~pos d = {exp_desc= d; exp_loc= mklocation pos}
let mkstmt ~pos d = {stmt_desc= d; stmt_loc= mklocation pos}
%}
%token <int> INT
%token <string> LIDENT
%token <string> UIDENT
%token FUN
%token LET
%token TYPE
%token TRUE
%token FALSE
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
%token BAR
%token QUOT
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
    { mkstmt ~pos:$loc (Value (x, e)) }
  | TYPE x = decl_type k = type_kind
    { let (x, args) = x in
      mkstmt ~pos:$loc (TypeDecl
        { tdec_ident= x
        ; tdec_params= args
        ; tdec_desc= k
        ; tdec_id= -1
        ; tdec_loc= mklocation $loc }) }

decl_type:
  | x = as_loc(LIDENT)
    { (x, []) }
  | x = as_loc(LIDENT) LBRACKET args = list(type_expr, COMMA) RBRACKET
    { (x, List.rev args) }

decl_type_expr:
  | x = decl_type
    { let (x, params) = x in
      mktyp ~pos:$loc
        (Tctor {var_ident= x; var_params= params; var_decl_id= 0}) }

record_field:
  | id = as_loc(LIDENT) COLON t = type_expr
    { { fld_ident= id ; fld_type= t ; fld_id= 0 ; fld_loc= mklocation $loc } }

type_kind:
  | (* empty *)
    { TAbstract }
  | EQUAL t = type_expr
    { TAlias t }
  | EQUAL LBRACE fields = list(record_field, COMMA) RBRACE
    { TRecord (List.rev fields) }
  | EQUAL maybe(BAR) ctors = list(ctor_decl, BAR)
    { TVariant (List.rev ctors) }

ctor_decl_args:
  | (* empty *)
    { Ctor_tuple [] }
  | LBRACKET rev_args = list(type_expr, COMMA) RBRACKET
    { Ctor_tuple (List.rev rev_args) }
  | LBRACE fields = list(record_field, COMMA) RBRACE
    { Ctor_record (List.rev fields) }

ctor_ident:
  | id = UIDENT
    { id }
  | LBRACKET RBRACKET
    { "()" }
  | TRUE
    { "true" }
  | FALSE
    { "false" }

ctor_decl:
  | id = as_loc(ctor_ident) args = ctor_decl_args
    return_typ = maybe(COLON t = decl_type_expr { t })
    { { ctor_ident= id
      ; ctor_args= args
      ; ctor_ret= return_typ
      ; ctor_loc= mklocation $loc } }

expr:
  | x = as_loc(LIDENT)
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
  | QUOT x = as_loc(LIDENT)
    { mktyp ~pos:$loc (Tvar (Some x, 0)) }
  | t = decl_type_expr
    { t }
  | LBRACKET x = type_expr RBRACKET
    { x }
  | LBRACKET xs = tuple(type_expr) RBRACKET
    { mktyp ~pos:$loc (Ttuple (List.rev xs)) }

type_expr:
  | x = simple_type_expr
    { x }
  | x = simple_type_expr DASHGT y = type_expr
    { mktyp ~pos:$loc (Tarrow (x, y)) }

list(X, SEP):
  | xs = list(X, SEP) SEP x = X
    { x :: xs }
  | x = X
    { [ x ] }

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
