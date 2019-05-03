%{
module List = Core_kernel.List
open Location
open Asttypes
open Ast_types
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
let mksig ~pos d = {sig_desc= d; sig_loc= mklocation pos}
let mkmod ~pos d = {mod_desc= d; mod_loc= mklocation pos}
let mkmty ~pos d = {msig_desc= d; msig_loc= mklocation pos}

let conspat ~pos hd tl =
  mkpat ~pos (PCtor
    ( mkloc ~pos (Lident "::"), Some (mkpat ~pos (PTuple [hd; tl]))))

let consexp ~pos hd tl =
  mkexp ~pos (Ctor
    ( mkloc ~pos (Lident "::"), Some (mkexp ~pos (Tuple [hd; tl]))))
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
%token REQUEST
%token WITH
%token HANDLER
%token SEMI
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token TILDE
%token QUESTION
%token DASHGT
%token EQUALGT
%token PLUSEQUAL
%token EQUAL
%token COLON
%token COLONCOLON
%token COMMA
%token UNDERSCORE
%token BAR
%token QUOT
%token DOTDOTDOT
%token DOTDOT
%token DOT
%token <string> COMMENT
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
%right    COLONCOLON
%left     INFIXOP2 PLUSEQUAL
%left     INFIXOP3
%right    INFIXOP4
%nonassoc above_infix
%nonassoc LPAREN

%start implementation
%type <Parsetypes.statement list> implementation

%start interface
%type <Parsetypes.signature_item list> interface

%%

%inline lident:
  | l = LIDENT
    { l }
  | REQUEST
    { "request" }
  | HANDLER
    { "handler" }

implementation:
  | s = structure EOF
    { s }

interface:
  | s = signature EOF
    { s }

file(item):
  | (* Empty *)
    { [] }
  | s = item maybe(SEMI)
    { [s] }
  | s = item SEMI rest = file(item)
    { s :: rest }
  | item err = err
    { raise (Error (err, Missing_semi)) }

structure:
  | s = file(structure_item)
    { s }

signature:
  | s = file(signature_item)
    { s }

structure_item:
  | LET x = pat EQUAL e = expr
    { mkstmt ~pos:$loc (Value (x, e)) }
  | INSTANCE x = as_loc(val_ident) EQUAL e = expr
    { mkstmt ~pos:$loc (Instance (x, e)) }
  | TYPE x = decl_type(lident) k = type_kind
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
  | REQUEST LPAREN arg = type_expr RPAREN x = ctor_decl handler = maybe(default_request_handler)
    { mkstmt ~pos:$loc (Request (arg, x, handler)) }

signature_item:
  | LET x = as_loc(val_ident) COLON typ = type_expr
    { mksig ~pos:$loc (SValue (x, typ)) }
  | INSTANCE x = as_loc(val_ident) COLON typ = type_expr
    { mksig ~pos:$loc (SInstance (x, typ)) }
  | TYPE x = decl_type(lident) k = type_kind
    { let (x, args) = x in
      mksig ~pos:$loc (STypeDecl
        { tdec_ident= x
        ; tdec_params= args
        ; tdec_implicit_params= []
        ; tdec_desc= k
        ; tdec_id= -1
        ; tdec_loc= mklocation $loc }) }
  | MODULE x = as_loc(UIDENT) COLON m = module_sig
    { mksig ~pos:$loc (SModule (x, m)) }
  | MODULE x = as_loc(UIDENT)
    { mksig ~pos:$loc (SModule (x, mkmty ~pos:$loc SigAbstract)) }
  | MODULE TYPE x = as_loc(UIDENT) EQUAL m = module_sig
    { mksig ~pos:$loc (SModType (x, m)) }

default_request_handler:
  | WITH HANDLER p = pat_ctor_args EQUALGT LBRACE body = block RBRACE
    { (p, body) }

module_expr:
  | LBRACE s = structure RBRACE
    { mkmod ~pos:$loc (Structure s) }
  | x = as_loc(longident(UIDENT, UIDENT))
    { mkmod ~pos:$loc (ModName x) }

module_sig:
  | LBRACE s = signature RBRACE
    { mkmty ~pos:$loc (Signature s) }
  | x = as_loc(longident(UIDENT, UIDENT))
    { mkmty ~pos:$loc (SigName x) }

%inline decl_type(X):
  | x = as_loc(X)
    { (x, []) }
  | x = as_loc(X) LPAREN args = list(type_expr, COMMA) RPAREN
    { (x, List.rev args) }

decl_type_expr:
  | x = decl_type(longident(lident, UIDENT))
    { let (x, params) = x in
      mktyp ~pos:$loc
        (Tctor {var_ident= x; var_params= params; var_implicit_params= []; var_decl_id= 0}) }

record_field(ID, EXP):
  | id = as_loc(ID) COLON t = EXP
    { (id, t) }

field_decl:
  | x = record_field(lident, type_expr)
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
  | LPAREN rev_args = list(type_expr, COMMA) RPAREN
    { Ctor_tuple (List.rev rev_args) }
  | LBRACE fields = list(field_decl, COMMA) RBRACE
    { Ctor_record (0, List.rev fields) }

%inline type_lident:
  | id = lident
    { Lident id }
  | m = longident(UIDENT, UIDENT) DOT id = lident
    { Ldot (m, id) }

%inline ctor_ident:
  | id = UIDENT
    { id }
  | LPAREN RPAREN
    { "()" }
  | TRUE
    { "true" }
  | FALSE
    { "false" }
  | LBRACKET RBRACKET
    { "[]" }
  | LPAREN COLONCOLON RPAREN
    { "::" }

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
  | id = lident
    { id }
  | LPAREN op = operator RPAREN
    { op }
  | LPAREN operator err = err
    { raise (Error (err, Expecting "operator")) }
  | LPAREN err = err
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
  | x = record_field(longident(lident, UIDENT), expr)
    { x }
  | x = as_loc(longident(lident, UIDENT))
    { (x, mkexp ~pos:$loc (Variable (mk_lid (lid_last x)))) }

simpl_expr:
  | x = as_loc(val_longident)
    { mkexp ~pos:$loc (Variable x) }
  | x = INT
    { mkexp ~pos:$loc (Int x) }
  | LPAREN e = expr_or_bare_tuple RPAREN
    { e }
  | LBRACKET es = list(expr, COMMA) RBRACKET
    { List.fold
        ~init:(mkexp ~pos:$loc (Ctor (mkloc ~pos:$loc (Lident "[]"), None)))
        es ~f:(fun acc e -> consexp ~pos:$loc e acc) }
  | LBRACE es = block RBRACE
    { es }
  | e = expr_record
    { e }
  | e = simpl_expr DOT field = as_loc(longident(lident, UIDENT))
    { mkexp ~pos:$loc (Field (e, field)) }

expr:
  | x = simpl_expr
    { x }
  | FUN LPAREN RPAREN EQUALGT LBRACE body = block RBRACE
    { let unit_pat =
        mkpat ~pos:$loc (PCtor (mkloc (Lident "()") ~pos:$loc, None))
      in
      mkexp ~pos:$loc (Fun (Nolabel, unit_pat, body, Explicit)) }
  | FUN LPAREN f = function_from_args
    { f }
  | FUN LBRACE f = function_from_implicit_args
    { f }
  | f = expr LPAREN es = expr_arg_list RPAREN
    { mkexp ~pos:$loc (Apply (f, List.rev es)) }
  | hd = expr COLONCOLON tl = expr
    { consexp ~pos:$loc hd tl }
  | e1 = expr op = infix_operator e2 = expr %prec above_infix
    { let op = mkloc (Lident op) ~pos:$loc(op) in
      mkexp ~pos:$loc
        (Apply (mkexp ~pos:$loc (Variable op), [Nolabel, e1; Nolabel, e2])) }
  | op = PREFIXOP e = expr
    { let op = mkloc (Lident op) ~pos:$loc(op) in
      mkexp ~pos:$loc (Apply (mkexp ~pos:$loc (Variable op), [Nolabel, e])) }
  | SWITCH LPAREN e = expr_or_bare_tuple RPAREN LBRACE rev_cases = list(match_case, {}) RBRACE
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
  | LPAREN e = expr_or_bare_tuple RPAREN
    { Some e }
  | e = expr_record
    { Some e }

match_case:
  | BAR p = pat EQUALGT e = expr
    { (p, e) }

expr_arg:
  | e = expr
    { (Asttypes.Nolabel, e) }
  | TILDE name = LIDENT
    { (Asttypes.Labelled name, mkexp ~pos:$loc
        (Variable (mkloc ~pos:$loc (Lident name)))) }
  | TILDE name = LIDENT EQUAL e = expr
    { (Asttypes.Labelled name, e) }
  | QUESTION name = LIDENT
    { (Asttypes.Optional name, mkexp ~pos:$loc
        (Variable (mkloc ~pos:$loc (Lident name)))) }
  | QUESTION name = LIDENT EQUAL e = expr
    { (Asttypes.Optional name, e) }

expr_arg_list:
  | (* empty *)
    { [Nolabel, mkexp ~pos:$loc (Ctor (mkloc ~pos:$loc (Lident "()"), None))] }
  | e = expr_arg
    { [e] }
  | es = expr_arg_list COMMA e = expr_arg
    { e :: es }

pat_arg:
  | p = pat
    { (Asttypes.Nolabel, p) }
  | TILDE name = as_loc(LIDENT)
    { (Asttypes.Labelled name.txt, mkpat ~pos:$loc (PVariable name)) }
  | TILDE name = as_loc(LIDENT) COLON typ = type_expr
    { ( Asttypes.Labelled name.txt
      , mkpat ~pos:$loc
          (PConstraint (mkpat ~pos:$loc(name) (PVariable name), typ)) ) }

pat_arg_opt:
  | p = pat_arg
    { p }
  | QUESTION name = as_loc(LIDENT)
    { (Asttypes.Optional name.txt, mkpat ~pos:$loc (PVariable name)) }
  | QUESTION name = as_loc(LIDENT) COLON typ = type_expr
    { ( Asttypes.Optional name.txt
      , mkpat ~pos:$loc
          (PConstraint (mkpat ~pos:$loc(name) (PVariable name), typ)) ) }

function_from_args:
  | p = pat_arg_opt RPAREN EQUALGT LBRACE body = block RBRACE
    { let (label, p) = p in
      mkexp ~pos:$loc (Fun (label, p, body, Explicit)) }
  | pat_arg_opt RPAREN err = err
    { raise (Error (err, Fun_no_fat_arrow)) }
  | p = pat_arg_opt RPAREN COLON typ = type_expr EQUALGT LBRACE body = block RBRACE
    { let (label, p) = p in
      mkexp ~pos:$loc (Fun (label, p, mkexp ~pos:$loc(typ)
        (Constraint (body, typ)), Explicit)) }
  | p = pat_arg_opt COMMA f = function_from_args
    { let (label, p) = p in
      mkexp ~pos:$loc (Fun (label, p, f, Explicit)) }

function_from_implicit_args:
  | p = pat_arg RBRACE LPAREN f = function_from_args
    { let (label, p) = p in
      mkexp ~pos:$loc (Fun (label, p, f, Implicit)) }
  | p = pat_arg RBRACE EQUALGT LBRACE body = block RBRACE
    { let (label, p) = p in
      mkexp ~pos:$loc (Fun (label, p, body, Implicit)) }
  | p = pat_arg RBRACE COLON typ = type_expr EQUALGT LBRACE body = block RBRACE
    { let (label, p) = p in
      mkexp ~pos:$loc (Fun (label, p, mkexp ~pos:$loc(typ)
        (Constraint (body, typ)), Implicit)) }
  | p = pat_arg COMMA f = function_from_implicit_args
    { let (label, p) = p in
      mkexp ~pos:$loc (Fun (label, p, f, Implicit)) }
  | pat_arg RBRACE err = err
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
  | x = record_field(longident(lident, UIDENT), pat)
    { x }
  | x = as_loc(longident(lident, UIDENT))
    { (x, mkpat ~pos:$loc (PVariable (lid_last x))) }

pat_record:
  | LBRACE fields = list(pat_field, COMMA) RBRACE
    { mkpat ~pos:$loc (PRecord (List.rev fields)) }

pat_ctor_args:
  | (* empty *)
    { None }
  | LPAREN p = pat_or_bare_tuple RPAREN
    { Some p }
  | p = pat_record
    { Some p }

pat_no_bar:
  | UNDERSCORE
    { mkpat ~pos:$loc PAny }
  | LPAREN p = pat_or_bare_tuple RPAREN
    { p }
  | LBRACKET ps = list(pat, COMMA) RBRACKET
    { List.fold
        ~init:(mkpat ~pos:$loc (PCtor (mkloc ~pos:$loc (Lident "[]"), None)))
        ps ~f:(fun acc p -> conspat ~pos:$loc p acc) }
  | hd = pat_no_bar COLONCOLON tl = pat_no_bar
    { conspat ~pos:$loc hd tl }
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
  | QUOT x = as_loc(lident)
    { mktyp ~pos:$loc (Tvar (Some x, 0, Explicit)) }
  | t = decl_type_expr
    { t }
  | LPAREN x = type_expr RPAREN
    { x }
  | LPAREN xs = tuple(type_expr) RPAREN
    { mktyp ~pos:$loc (Ttuple (List.rev xs)) }

%inline type_arrow_label:
  | (* Empty *)
    { Asttypes.Nolabel }
  | name = LIDENT COLON
    { Asttypes.Labelled name }

type_expr:
  | x = simple_type_expr
    { x }
  | label = type_arrow_label x = simple_type_expr DASHGT y = type_expr
    { mktyp ~pos:$loc (Tarrow (x, y, Explicit, label)) }
  | QUESTION name = LIDENT COLON x = simple_type_expr DASHGT y = type_expr
    { mktyp ~pos:$loc (Tarrow (x, y, Explicit, Asttypes.Optional name)) }
  | label = type_arrow_label LBRACE x = simple_type_expr RBRACE DASHGT y = type_expr
    { mktyp ~pos:$loc (Tarrow (x, y, Implicit, label)) }

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
