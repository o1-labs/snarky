%{
module List = Core_kernel.List
module Loc = Ast_build.Loc
open Location
open Asttypes
open Ast_types
open Parsetypes
open Longident
open Parser_errors

let lid_last x = mkloc (last x.txt) x.loc

let mkloc ~pos x = mkloc x (Loc.of_pos pos)

let mktyp ~pos d = {type_desc= d; type_loc= Loc.of_pos pos}
let mkpat ~pos d = {pat_desc= d; pat_loc= Loc.of_pos pos}
let mkexp ~pos d = {exp_desc= d; exp_loc= Loc.of_pos pos}
let mkstmt ~pos d = {stmt_desc= d; stmt_loc= Loc.of_pos pos}
let mksig ~pos d = {sig_desc= d; sig_loc= Loc.of_pos pos}
let mkmod ~pos d = {mod_desc= d; mod_loc= Loc.of_pos pos}
let mkmty ~pos d = {msig_desc= d; msig_loc= Loc.of_pos pos}

let conspat ~pos hd tl =
  mkpat ~pos (Ppat_ctor
    ( mkloc ~pos (Lident "::"), Some (mkpat ~pos (Ppat_tuple [hd; tl]))))

let consexp ~pos hd tl =
  mkexp ~pos (Pexp_ctor
    ( mkloc ~pos (Lident "::"), Some (mkexp ~pos (Pexp_tuple [hd; tl]))))
%}
%token <string> FIELD
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token <string> LIDENT
%token <string> UIDENT
%token PROVER
%token FUN
%token LET
%token INSTANCE
%token AND
%token TRUE
%token FALSE
%token SWITCH
%token TYPE
%token CONVERTIBLE
%token BY
%token TO
%token LPROVER
%token REC
%token MODULE
%token OPEN
%token REQUEST
%token WITH
%token HANDLER
%token IF
%token ELSE
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
%token DASHDASHGT
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
%token MINUS
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
%left     MINUS INFIXOP2 PLUSEQUAL
%left     INFIXOP3
%right    INFIXOP4
%nonassoc above_infix
%right    DASHGT
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
  | LPROVER
    { "prover" }

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
    { mkstmt ~pos:$loc (Pstmt_value (x, e)) }
  | INSTANCE x = as_loc(val_ident) EQUAL e = expr
    { mkstmt ~pos:$loc (Pstmt_instance (x, e)) }
  | TYPE decl = type_decl
    { mkstmt ~pos:$loc (Pstmt_type decl) }
  | CONVERTIBLE TYPE decl = type_decl c = conv_type
    named = maybe (BY l = as_loc(val_ident) { l })
    { mkstmt ~pos:$loc (Pstmt_convtype (decl, c, named)) }
  | decls = type_decls
    { mkstmt ~pos:$loc (Pstmt_rectype decls) }
  | MODULE x = as_loc(UIDENT) EQUAL m = module_expr
    { mkstmt ~pos:$loc (Pstmt_module (x, m)) }
  | MODULE TYPE x = as_loc(UIDENT) EQUAL m = module_sig
    { mkstmt ~pos:$loc (Pstmt_modtype (x, m)) }
  | OPEN x = as_loc(longident(UIDENT, UIDENT))
    { mkstmt ~pos:$loc (Pstmt_open x) }
  | OPEN INSTANCE x = as_loc(longident(UIDENT, UIDENT))
    { mkstmt ~pos:$loc (Pstmt_open_instance x) }
  | TYPE x = decl_type(type_lident) PLUSEQUAL
    maybe(BAR) ctors = list(ctor_decl, BAR)
    { let (x, params) = x in
      mkstmt ~pos:$loc (Pstmt_typeext
        ( {var_ident= x; var_params= params}
        , ctors)) }
  | REQUEST LPAREN arg = type_expr RPAREN x = ctor_decl handler = maybe(default_request_handler)
    { mkstmt ~pos:$loc (Pstmt_request (arg, x, handler)) }
  | PROVER LBRACE stmts = structure RBRACE
    { mkstmt ~pos:$loc (Pstmt_prover stmts) }

signature_item:
  | LET x = as_loc(val_ident) COLON typ = type_expr
    { mksig ~pos:$loc (Psig_value (x, typ)) }
  | INSTANCE x = as_loc(val_ident) COLON typ = type_expr
    { mksig ~pos:$loc (Psig_instance (x, typ)) }
  | TYPE decl = type_decl
    { mksig ~pos:$loc (Psig_type decl) }
  | CONVERTIBLE TYPE decl = type_decl c = conv_type
    named = maybe (BY l = as_loc(val_ident) { l })
    { mksig ~pos:$loc (Psig_convtype (decl, c, named)) }
  | decls = type_decls
    { mksig ~pos:$loc (Psig_rectype decls) }
  | MODULE x = as_loc(UIDENT) COLON m = module_sig
    { mksig ~pos:$loc (Psig_module (x, m)) }
  | MODULE x = as_loc(UIDENT)
    { mksig ~pos:$loc (Psig_module (x, mkmty ~pos:$loc Pmty_abstract)) }
  | MODULE TYPE x = as_loc(UIDENT) EQUAL m = module_sig
    { mksig ~pos:$loc (Psig_modtype (x, m)) }
  | OPEN x = as_loc(longident(UIDENT, UIDENT))
    { mksig ~pos:$loc (Psig_open x) }
  | TYPE x = decl_type(type_lident) PLUSEQUAL
    maybe(BAR) ctors = list(ctor_decl, BAR)
    { let (x, params) = x in
      mksig ~pos:$loc (Psig_typeext
        ( {var_ident= x; var_params= params}
        , ctors)) }
  | REQUEST LPAREN arg = type_expr RPAREN x = ctor_decl
    { mksig ~pos:$loc (Psig_request (arg, x)) }
  | PROVER LBRACE sigs = signature RBRACE
    { mksig ~pos:$loc (Psig_prover sigs) }

type_decl:
  | x = decl_type(lident) k = type_kind
    { let (x, args) = x in
      { tdec_ident= x
      ; tdec_params= args
      ; tdec_desc= k
      ; tdec_loc= Loc.of_pos $loc } }

and_type_decls(type_keyword):
  | type_keyword decl = type_decl
    { [decl] }
  | type_keyword decl = type_decl decls = and_type_decls(AND)
    { decl :: decls }

type_decls:
  | decls = and_type_decls(TYPE REC {})
    { decls }

default_request_handler:
  | WITH HANDLER p = pat_ctor_args EQUALGT LBRACE body = block RBRACE
    { (p, body) }

conv_type:
  | WITH decl = type_decl
    { Ptconv_with (Checked, decl) }
  | WITH LPROVER decl = type_decl
    { Ptconv_with (Prover, decl) }
  | TO typ = type_expr
    { Ptconv_to typ }

module_expr:
  | LBRACE s = structure RBRACE
    { mkmod ~pos:$loc (Pmod_struct s) }
  | x = as_loc(longident(UIDENT, UIDENT))
    { mkmod ~pos:$loc (Pmod_name x) }

module_sig:
  | LBRACE s = signature RBRACE
    { mkmty ~pos:$loc (Pmty_sig s) }
  | x = as_loc(longident(UIDENT, UIDENT))
    { mkmty ~pos:$loc (Pmty_name x) }

%inline decl_type(X):
  | x = as_loc(X)
    { (x, []) }
  | x = as_loc(X) LPAREN args = list(type_expr, COMMA) RPAREN
    { (x, List.rev args) }

decl_type_expr:
  | x = decl_type(longident(lident, UIDENT))
    { let (x, params) = x in
      mktyp ~pos:$loc
        (Ptyp_ctor {var_ident= x; var_params= params}) }

record_field(ID, EXP):
  | id = as_loc(ID) COLON t = EXP
    { (id, t) }

field_decl:
  | x = record_field(lident, type_expr)
    { let (fld_ident, fld_type) = x in
      { fld_ident; fld_type; fld_loc= Loc.of_pos $loc } }

type_kind:
  | (* empty *)
    { Pdec_abstract }
  | EQUAL k = type_kind_body
    { k }

type_kind_body:
  | t = type_expr
    { Pdec_alias t }
  | LBRACE fields = list(field_decl, COMMA) RBRACE
    { Pdec_record (List.rev fields) }
  | maybe(BAR) ctors = list(ctor_decl, BAR)
    { Pdec_variant (List.rev ctors) }
  | DOTDOT
    { Pdec_open }

ctor_decl_args:
  | (* empty *)
    { Ctor_tuple [] }
  | LPAREN rev_args = list(type_expr, COMMA) RPAREN
    { Ctor_tuple (List.rev rev_args) }
  | LBRACE fields = list(field_decl, COMMA) RBRACE
    { Ctor_record (List.rev fields) }

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
  | MINUS         { "-" }
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
      ; ctor_loc= Loc.of_pos $loc } }

expr_field:
  | x = record_field(longident(lident, UIDENT), expr)
    { x }
  | x = as_loc(longident(lident, UIDENT))
    { (x, mkexp ~pos:$loc (Pexp_variable (mk_lid (lid_last x)))) }

simpl_expr:
  | x = as_loc(val_longident)
    { mkexp ~pos:$loc (Pexp_variable x) }
  | x = INT
    { mkexp ~pos:$loc (Pexp_literal (Int x)) }
  | x = BOOL
    { mkexp ~pos:$loc (Pexp_literal (Bool x)) }
  | x = FIELD
    { mkexp ~pos:$loc (Pexp_literal (Field x)) }
  | LPAREN e = expr_or_bare_tuple RPAREN
    { e }
  | LBRACKET es = list(expr, COMMA) RBRACKET
    { List.fold
        ~init:(mkexp ~pos:$loc (Pexp_ctor (mkloc ~pos:$loc (Lident "[]"), None)))
        es ~f:(fun acc e -> consexp ~pos:$loc e acc) }
  | LBRACE es = block RBRACE
    { es }
  | e = expr_record
    { e }
  | e = simpl_expr DOT field = as_loc(longident(lident, UIDENT))
    { mkexp ~pos:$loc (Pexp_field (e, field)) }
  | s = STRING
    { mkexp ~pos:$loc (Pexp_literal (String s)) }
  | PROVER LBRACE e = block RBRACE
    { mkexp ~pos:$loc (Pexp_prover e) }

expr:
  | x = simpl_expr
    { x }
  | LPAREN x = simpl_expr COLON typ = type_expr RPAREN
    { mkexp ~pos:$loc (Pexp_constraint (x, typ)) }
  | FUN LPAREN RPAREN EQUALGT LBRACE body = block RBRACE
    { let unit_pat =
        mkpat ~pos:$loc (Ppat_ctor (mkloc (Lident "()") ~pos:$loc, None))
      in
      mkexp ~pos:$loc (Pexp_fun (Nolabel, unit_pat, body, Explicit)) }
  | FUN LPAREN f = function_from_args
    { f }
  | FUN LBRACE f = function_from_implicit_args
    { f }
  | f = expr LPAREN es = expr_arg_list RPAREN
    { mkexp ~pos:$loc (Pexp_apply (f, List.rev es)) }
  | hd = expr COLONCOLON tl = expr
    { consexp ~pos:$loc hd tl }
  | e1 = expr op = infix_operator e2 = expr %prec above_infix
    { let op = mkloc (Lident op) ~pos:$loc(op) in
      mkexp ~pos:$loc
        (Pexp_apply (mkexp ~pos:$loc (Pexp_variable op), [Nolabel, e1; Nolabel, e2])) }
  | op = PREFIXOP e = expr
    { let op = mkloc (Lident op) ~pos:$loc(op) in
      mkexp ~pos:$loc (Pexp_apply (mkexp ~pos:$loc (Pexp_variable op), [Nolabel, e])) }
  | _op = MINUS e = expr
    { let op = mkloc (Lident "~-") ~pos:$loc(_op) in
      mkexp ~pos:$loc (Pexp_apply (mkexp ~pos:$loc (Pexp_variable op), [Nolabel, e])) }
  | SWITCH LPAREN e = expr_or_bare_tuple RPAREN LBRACE rev_cases = list(match_case, {}) RBRACE
    { mkexp ~pos:$loc (Pexp_match (e, List.rev rev_cases)) }
  | id = as_loc(longident(ctor_ident, UIDENT)) args = expr_ctor_args
    { mkexp ~pos:$loc (Pexp_ctor (id, args)) }
  | e = if_expr
    { e }

if_expr:
  | IF e1 = expr LBRACE e2 = block RBRACE
    { mkexp ~pos:$loc (Pexp_if (e1, e2, None)) }
  | IF e1 = expr LBRACE e2 = block RBRACE ELSE e3 = if_expr_or_block
    { mkexp ~pos:$loc (Pexp_if (e1, e2, Some e3)) }

if_expr_or_block:
  | e = if_expr
    { e }
  | LBRACE e = block RBRACE
    { e }

expr_record:
  | LBRACE fields = list(expr_field, COMMA) RBRACE
    { mkexp ~pos:$loc (Pexp_record(List.rev fields, None)) }
  | LBRACE DOTDOTDOT e = expr COMMA fields = list(expr_field, COMMA) RBRACE
    { mkexp ~pos:$loc (Pexp_record(List.rev fields, Some e)) }

expr_or_bare_tuple:
  | x = expr
    { x }
  | es = tuple(expr)
    { mkexp ~pos:$loc (Pexp_tuple (List.rev es)) }

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
  | TILDE name = lident
    { (Asttypes.Labelled name, mkexp ~pos:$loc
        (Pexp_variable (mkloc ~pos:$loc (Lident name)))) }
  | TILDE name = lident EQUAL e = expr
    { (Asttypes.Labelled name, e) }
  | QUESTION name = lident
    { (Asttypes.Optional name, mkexp ~pos:$loc
        (Pexp_variable (mkloc ~pos:$loc (Lident name)))) }
  | QUESTION name = lident EQUAL e = expr
    { (Asttypes.Optional name, e) }

expr_arg_list:
  | (* empty *)
    { [Nolabel, mkexp ~pos:$loc (Pexp_ctor (mkloc ~pos:$loc (Lident "()"), None))] }
  | e = expr_arg
    { [e] }
  | es = expr_arg_list COMMA e = expr_arg
    { e :: es }

pat_arg:
  | p = pat
    { (Asttypes.Nolabel, p) }
  | TILDE name = as_loc(lident)
    { (Asttypes.Labelled name.txt, mkpat ~pos:$loc (Ppat_variable name)) }
  | TILDE name = as_loc(lident) COLON typ = type_expr
    { ( Asttypes.Labelled name.txt
      , mkpat ~pos:$loc
          (Ppat_constraint (mkpat ~pos:$loc(name) (Ppat_variable name), typ)) ) }

pat_arg_opt:
  | p = pat_arg
    { p }
  | QUESTION name = as_loc(lident)
    { (Asttypes.Optional name.txt, mkpat ~pos:$loc (Ppat_variable name)) }
  | QUESTION name = as_loc(lident) COLON typ = type_expr
    { ( Asttypes.Optional name.txt
      , mkpat ~pos:$loc
          (Ppat_constraint (mkpat ~pos:$loc(name) (Ppat_variable name), typ)) ) }

function_body:
 | EQUALGT LBRACE body = block RBRACE
   { body }
 | err = err
   { raise (Error (err, Fun_no_fat_arrow)) }

function_from_args:
  | p = pat_arg_opt RPAREN body = function_body
    { let (label, p) = p in
      mkexp ~pos:$loc (Pexp_fun (label, p, body, Explicit)) }
  | p = pat_arg_opt RPAREN COLON typ = type_expr body = function_body
    { let (label, p) = p in
      mkexp ~pos:$loc (Pexp_fun (label, p, mkexp ~pos:$loc(typ)
        (Pexp_constraint (body, typ)), Explicit)) }
  | p = pat_arg_opt COMMA f = function_from_args
    { let (label, p) = p in
      mkexp ~pos:$loc (Pexp_fun (label, p, f, Explicit)) }
  | TYPE t = as_loc(lident) RPAREN body = function_body
    { mkexp ~pos:$loc (Pexp_newtype (t, body)) }
  | TYPE t = as_loc(lident) COMMA f = function_from_args
    { mkexp ~pos:$loc (Pexp_newtype (t, f)) }
  | pat_arg_opt RPAREN err = err
    { raise (Error (err, Fun_no_fat_arrow)) }

function_from_implicit_args:
  | p = pat_arg RBRACE LPAREN f = function_from_args
    { let (label, p) = p in
      mkexp ~pos:$loc (Pexp_fun (label, p, f, Implicit)) }
  | p = pat_arg RBRACE EQUALGT LBRACE body = block RBRACE
    { let (label, p) = p in
      mkexp ~pos:$loc (Pexp_fun (label, p, body, Implicit)) }
  | p = pat_arg RBRACE COLON typ = type_expr EQUALGT LBRACE body = block RBRACE
    { let (label, p) = p in
      mkexp ~pos:$loc (Pexp_fun (label, p, mkexp ~pos:$loc(typ)
        (Pexp_constraint (body, typ)), Implicit)) }
  | p = pat_arg COMMA f = function_from_implicit_args
    { let (label, p) = p in
      mkexp ~pos:$loc (Pexp_fun (label, p, f, Implicit)) }
  | pat_arg RBRACE err = err
    { raise (Error (err, Fun_no_fat_arrow)) }

block:
  | e = expr SEMI
    { e }
  | e1 = expr SEMI rest = block
    { mkexp ~pos:$loc (Pexp_seq (e1, rest)) }
  | LET x = pat EQUAL lhs = expr SEMI rhs = block
    { mkexp ~pos:$loc (Pexp_let (x, lhs, rhs)) }
  | INSTANCE x = as_loc(lident) EQUAL lhs = expr SEMI rhs = block
    { mkexp ~pos:$loc (Pexp_instance (x, lhs, rhs)) }
  | LET pat EQUAL expr err = err
    { raise (Error (err, Missing_semi)) }
  | expr err = err
    { raise (Error (err, Missing_semi)) }

pat_field:
  | x = record_field(longident(lident, UIDENT), pat)
    { x }
  | x = as_loc(longident(lident, UIDENT))
    { (x, mkpat ~pos:$loc (Ppat_variable (lid_last x))) }

pat_record:
  | LBRACE fields = list(pat_field, COMMA) RBRACE
    { mkpat ~pos:$loc (Ppat_record (List.rev fields)) }

pat_ctor_args:
  | (* empty *)
    { None }
  | LPAREN p = pat_or_bare_tuple RPAREN
    { Some p }
  | p = pat_record
    { Some p }

pat_no_bar:
  | UNDERSCORE
    { mkpat ~pos:$loc Ppat_any }
  | LPAREN p = pat_or_bare_tuple RPAREN
    { p }
  | LBRACKET ps = list(pat, COMMA) RBRACKET
    { List.fold
        ~init:(mkpat ~pos:$loc (Ppat_ctor (mkloc ~pos:$loc (Lident "[]"), None)))
        ps ~f:(fun acc p -> conspat ~pos:$loc p acc) }
  | hd = pat_no_bar COLONCOLON tl = pat_no_bar
    { conspat ~pos:$loc hd tl }
  | p = pat_no_bar COLON typ = type_expr
    { mkpat ~pos:$loc (Ppat_constraint (p, typ)) }
  | x = as_loc(val_ident)
    { mkpat ~pos:$loc (Ppat_variable x) }
  | i = INT
    { mkpat ~pos:$loc (Ppat_int i) }
  | p = pat_record
    { p }
  | id = as_loc(longident(ctor_ident, UIDENT)) args = pat_ctor_args
    { mkpat ~pos:$loc (Ppat_ctor (id, args)) }

pat:
  | p = pat_no_bar
    { p }
  | p1 = pat_no_bar BAR p2 = pat
    { mkpat ~pos:$loc (Ppat_or (p1, p2)) }

pat_or_bare_tuple:
  | x = pat
    { x }
  | ps = tuple(pat)
    { mkpat ~pos:$loc (Ppat_tuple (List.rev ps)) }

simple_type_expr:
  | UNDERSCORE
    { mktyp ~pos:$loc (Ptyp_var None) }
  | QUOT x = as_loc(lident)
    { mktyp ~pos:$loc (Ptyp_var (Some x)) }
  | t = decl_type_expr
    { t }
  | LPAREN x = type_expr RPAREN
    { x }
  | LPAREN xs = tuple(type_expr) RPAREN
    { mktyp ~pos:$loc (Ptyp_tuple (List.rev xs)) }
  | PROVER LPAREN x = type_expr RPAREN
    { mktyp ~pos:$loc (Ptyp_prover x) }
  | PROVER LBRACE x = type_expr RBRACE
    { mktyp ~pos:$loc (Ptyp_prover x) }

%inline type_arrow_label:
  | (* Empty *)
    { Asttypes.Nolabel }
  | name = lident COLON
    { Asttypes.Labelled name }

type_expr:
  | x = simple_type_expr
    { x }
  | label = type_arrow_label x = simple_type_expr DASHGT y = type_expr
    { mktyp ~pos:$loc (Ptyp_arrow (x, y, Explicit, label)) }
  | QUESTION name = lident COLON x = simple_type_expr DASHGT y = type_expr
    { mktyp ~pos:$loc (Ptyp_arrow (x, y, Explicit, Asttypes.Optional name)) }
  | label = type_arrow_label LBRACE x = simple_type_expr RBRACE DASHGT y = type_expr
    { mktyp ~pos:$loc (Ptyp_arrow (x, y, Implicit, label)) }
  | x = simple_type_expr DASHDASHGT y = type_expr
    { mktyp ~pos:$loc (Ptyp_conv (x, y)) }

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
  { Loc.of_pos ($symbolstartpos, $endpos) }
