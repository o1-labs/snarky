%{
open Location
open Asttypes
open Parsetypes
open Longident
open Parser_errors

let mklocation (loc_start, loc_end) = {loc_start; loc_end; loc_ghost= false}

let lid_last x = mkloc (last x.txt) x.loc

let mkloc ~pos x = mkloc x (mklocation pos)

let mktyp ~pos d = {type_desc= d; type_id= -1; type_loc= mklocation pos}
let mkpat ~pos d = {pat_desc= d; pat_loc= mklocation pos; pat_type= mktyp ~pos (Tvar (None, -1, Explicit))}
let mkexp ~pos d = {exp_desc= d; exp_loc= mklocation pos; exp_type= mktyp ~pos (Tvar (None, -1, Explicit))}
let mkstmt ~pos d = {stmt_desc= d; stmt_loc= mklocation pos}
let mkmod ~pos d = {mod_desc= d; mod_loc= mklocation pos}
%}
%token <int> INT
%token <string> LIDENT
%token <string> UIDENT
%token FUN
%token LET
%token INSTANCE
%token TRUE
%token FALSE
%token SWITCH
%token TYPE
%token MODULE
%token OPEN
%token SEMI
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token DASHGT
%token EQUALGT
%token PLUSEQUAL
%token EQUAL
%token COLON
%token COMMA
%token UNDERSCORE
%token BAR
%token QUOT
%token DOTDOTDOT
%token DOTDOT
%token DOT
%token <string> PREFIXOP
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token EOF

%token EOL

%left     INFIXOP0 EQUAL
%right    INFIXOP1
%left     INFIXOP2 PLUSEQUAL
%left     INFIXOP3
%right    INFIXOP4
%nonassoc above_infix
%nonassoc LBRACKET

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
  | structure_item err = err
    { raise (Error (err, Missing_semi)) }

structure_item:
  | LET x = pat EQUAL e = expr
    { mkstmt ~pos:$loc (Value (x, e)) }
  | INSTANCE x = as_loc(val_ident) EQUAL e = expr
    { mkstmt ~pos:$loc (Instance (x, e)) }
  | TYPE x = decl_type(LIDENT) k = type_kind
    { let (x, args) = x in
      mkstmt ~pos:$loc (TypeDecl
        { tdec_ident= x
        ; tdec_params= args
        ; tdec_implicit_params= []
        ; tdec_desc= k
        ; tdec_id= -1
        ; tdec_loc= mklocation $loc }) }
  | MODULE x = as_loc(UIDENT) EQUAL m = module_expr
    { mkstmt ~pos:$loc (Module (x, m)) }
  | OPEN x = as_loc(longident(UIDENT, UIDENT))
    { mkstmt ~pos:$loc (Open x) }
  | TYPE x = decl_type(type_lident) PLUSEQUAL
    maybe(BAR) ctors = list(ctor_decl, BAR)
    { let (x, params) = x in
      mkstmt ~pos:$loc (TypeExtension
        ( {var_ident= x; var_params= params; var_implicit_params= []; var_decl_id= 0}
        , ctors)) }

module_expr:
  | LBRACE s = structure RBRACE
    { mkmod ~pos:$loc (Structure s) }
  | x = as_loc(longident(UIDENT, UIDENT))
    { mkmod ~pos:$loc (ModName x) }

%inline decl_type(X):
  | x = as_loc(X)
    { (x, []) }
  | x = as_loc(X) LBRACKET args = list(type_expr, COMMA) RBRACKET
    { (x, List.rev args) }

decl_type_expr:
  | x = decl_type(longident(LIDENT, UIDENT))
    { let (x, params) = x in
      mktyp ~pos:$loc
        (Tctor {var_ident= x; var_params= params; var_implicit_params= []; var_decl_id= 0}) }

record_field(ID, EXP):
  | id = as_loc(ID) COLON t = EXP
    { (id, t) }

field_decl:
  | x = record_field(LIDENT, type_expr)
    { let (fld_ident, fld_type) = x in
      { fld_ident; fld_type; fld_id= 0; fld_loc= mklocation $loc } }

type_kind:
  | (* empty *)
    { TAbstract }
  | EQUAL k = type_kind_body
    { k }

type_kind_body:
  | t = type_expr
    { TAlias t }
  | LBRACE fields = list(field_decl, COMMA) RBRACE
    { TRecord (List.rev fields) }
  | maybe(BAR) ctors = list(ctor_decl, BAR)
    { TVariant (List.rev ctors) }
  | DOTDOT
    { TOpen }

ctor_decl_args:
  | (* empty *)
    { Ctor_tuple [] }
  | LBRACKET rev_args = list(type_expr, COMMA) RBRACKET
    { Ctor_tuple (List.rev rev_args) }
  | LBRACE fields = list(field_decl, COMMA) RBRACE
    { Ctor_record (0, List.rev fields) }

%inline type_lident:
  | id = LIDENT
    { Lident id }
  | m = longident(UIDENT, UIDENT) DOT id = LIDENT
    { Ldot (m, id) }

ctor_ident:
  | id = UIDENT
    { id }
  | LBRACKET RBRACKET
    { "()" }
  | TRUE
    { "true" }
  | FALSE
    { "false" }

infix_operator:
  | op = INFIXOP0 { op }
  | EQUAL         { "=" }
  | op = INFIXOP1 { op }
  | op = INFIXOP2 { op }
  | PLUSEQUAL     { "+=" }
  | op = INFIXOP3 { op }
  | op = INFIXOP4 { op }

operator:
    op = PREFIXOP
    { op }
  | op = infix_operator
    { op }

val_ident:
  | id = LIDENT
    { id }
  | LBRACKET op = operator RBRACKET
    { op }
  | LBRACKET operator err = err
    { raise (Error (err, Expecting "operator")) }
  | LBRACKET err = err
    { raise (Error (err, Expecting ")")) }

val_longident:
  | x = longident (val_ident, UIDENT)
    { x }

ctor_decl:
  | id = as_loc(ctor_ident) args = ctor_decl_args
    return_typ = maybe(COLON t = decl_type_expr { t })
    { { ctor_ident= id
      ; ctor_args= args
      ; ctor_ret= return_typ
      ; ctor_loc= mklocation $loc } }

expr_field:
  | x = record_field(longident(LIDENT, UIDENT), expr)
    { x }
  | x = as_loc(longident(LIDENT, UIDENT))
    { (x, mkexp ~pos:$loc (Variable (mk_lid (lid_last x)))) }

simpl_expr:
  | x = as_loc(val_longident)
    { mkexp ~pos:$loc (Variable x) }
  | x = INT
    { mkexp ~pos:$loc (Int x) }
  | LBRACKET e = expr_or_bare_tuple RBRACKET
    { e }
  | LBRACE es = block RBRACE
    { es }
  | e = expr_record
    { e }
  | e = simpl_expr DOT field = as_loc(longident(LIDENT, UIDENT))
    { mkexp ~pos:$loc (Field (e, field)) }

expr:
  | x = simpl_expr
    { x }
  | FUN LBRACKET RBRACKET EQUALGT LBRACE body = block RBRACE
    { let unit_pat =
        mkpat ~pos:$loc (PCtor (mkloc (Lident "()") ~pos:$loc, None))
      in
      mkexp ~pos:$loc (Fun (unit_pat, body, Explicit)) }
  | FUN LBRACKET f = function_from_args
    { f }
  | FUN LBRACE f = function_from_implicit_args
    { f }
  | f = expr LBRACKET es = expr_list RBRACKET
    { mkexp ~pos:$loc (Apply (f, List.rev es)) }
  | e1 = expr op = infix_operator e2 = expr %prec above_infix
    { let op = mkloc ~pos:$loc(op) (Lident op) in
      mkexp ~pos:$loc (Apply (mkexp ~pos:$loc (Variable op), [e1; e2])) }
  | op = PREFIXOP e = expr
    { let op = mkloc ~pos:$loc(op) (Lident op) in
      mkexp ~pos:$loc (Apply (mkexp ~pos:$loc (Variable op), [e])) }
  | SWITCH LBRACKET e = expr_or_bare_tuple RBRACKET LBRACE rev_cases = list(match_case, {}) RBRACE
    { mkexp ~pos:$loc (Match (e, List.rev rev_cases)) }
  | id = as_loc(longident(ctor_ident, UIDENT)) args = expr_ctor_args
    { mkexp ~pos:$loc (Ctor (id, args)) }

expr_record:
  | LBRACE fields = list(expr_field, COMMA) RBRACE
    { mkexp ~pos:$loc (Record(List.rev fields, None)) }
  | LBRACE DOTDOTDOT e = expr COMMA fields = list(expr_field, COMMA) RBRACE
    { mkexp ~pos:$loc (Record(List.rev fields, Some e)) }

expr_or_bare_tuple:
  | x = expr
    { x }
  | es = tuple(expr)
    { mkexp ~pos:$loc (Tuple (List.rev es)) }

expr_ctor_args:
  | (* empty *)
    { None }
  | LBRACKET e = expr_or_bare_tuple RBRACKET
    { Some e }
  | e = expr_record
    { Some e }

match_case:
  | BAR p = pat EQUALGT e = expr
    { (p, e) }

expr_list:
  | (* empty *)
    { [mkexp ~pos:$loc (Ctor (mkloc ~pos:$loc (Lident "()"), None))] }
  | e = expr
    { [e] }
  | es = expr_list COMMA e = expr
    { e :: es }

function_from_args:
  | p = pat RBRACKET EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, body, Explicit)) }
  | pat RBRACKET err = err
    { raise (Error (err, Fun_no_fat_arrow)) }
  | p = pat RBRACKET COLON typ = type_expr EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, mkexp ~pos:$loc(typ) (Constraint (body, typ)), Explicit)) }
  | p = pat COMMA f = function_from_args
    { mkexp ~pos:$loc (Fun (p, f, Explicit)) }

function_from_implicit_args:
  | p = pat RBRACE LBRACKET f = function_from_args
    { mkexp ~pos:$loc (Fun (p, f, Implicit)) }
  | p = pat RBRACE EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, body, Implicit)) }
  | p = pat RBRACE COLON typ = type_expr EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, mkexp ~pos:$loc(typ) (Constraint (body, typ)), Implicit)) }
  | p = pat COMMA f = function_from_implicit_args
    { mkexp ~pos:$loc (Fun (p, f, Implicit)) }
  | pat RBRACE err = err
    { raise (Error (err, Fun_no_fat_arrow)) }

block:
  | e = expr SEMI
    { e }
  | e1 = expr SEMI rest = block
    { mkexp ~pos:$loc (Seq (e1, rest)) }
  | LET x = pat EQUAL lhs = expr SEMI rhs = block
    { mkexp ~pos:$loc (Let (x, lhs, rhs)) }
  | LET pat EQUAL expr err = err
    { raise (Error (err, Missing_semi)) }
  | expr err = err
    { raise (Error (err, Missing_semi)) }

pat_field:
  | x = record_field(longident(LIDENT, UIDENT), pat)
    { x }
  | x = as_loc(longident(LIDENT, UIDENT))
    { (x, mkpat ~pos:$loc (PVariable (lid_last x))) }

pat_record:
  | LBRACE fields = list(pat_field, COMMA) RBRACE
    { mkpat ~pos:$loc (PRecord (List.rev fields)) }

pat_ctor_args:
  | (* empty *)
    { None }
  | LBRACKET p = pat_or_bare_tuple RBRACKET
    { Some p }
  | p = pat_record
    { Some p }

pat_no_bar:
  | UNDERSCORE
    { mkpat ~pos:$loc PAny }
  | LBRACKET p = pat_or_bare_tuple RBRACKET
    { p }
  | p = pat_no_bar COLON typ = type_expr
    { mkpat ~pos:$loc (PConstraint (p, typ)) }
  | x = as_loc(val_ident)
    { mkpat ~pos:$loc (PVariable x) }
  | i = INT
    { mkpat ~pos:$loc (PInt i) }
  | p = pat_record
    { p }
  | id = as_loc(longident(ctor_ident, UIDENT)) args = pat_ctor_args
    { mkpat ~pos:$loc (PCtor (id, args)) }

pat:
  | p = pat_no_bar
    { p }
  | p1 = pat_no_bar BAR p2 = pat
    { mkpat ~pos:$loc (POr (p1, p2)) }

pat_or_bare_tuple:
  | x = pat
    { x }
  | ps = tuple(pat)
    { mkpat ~pos:$loc (PTuple (List.rev ps)) }

simple_type_expr:
  | UNDERSCORE
    { mktyp ~pos:$loc (Tvar (None, 0, Explicit)) }
  | QUOT x = as_loc(LIDENT)
    { mktyp ~pos:$loc (Tvar (Some x, 0, Explicit)) }
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
    { mktyp ~pos:$loc (Tarrow (x, y, Explicit)) }
  | LBRACE x = simple_type_expr RBRACE DASHGT y = type_expr
    { mktyp ~pos:$loc (Tarrow (x, y, Implicit)) }

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
  { mkloc x ~pos:($symbolstartpos, $endpos) }

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

%inline err : _x = error
  { mklocation ($symbolstartpos, $endpos) }
