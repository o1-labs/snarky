%{
open Location
open Parsetypes

let mklocation (loc_start, loc_end) = {loc_start; loc_end; loc_ghost= false}

let mkrhs rhs pos = mkloc rhs (mklocation pos)

let pos_to_loc ~pos (f : ?loc:Location.t -> 'b) = f ~loc:(mklocation pos)

let mktyp = pos_to_loc Type.mk
let mktypvar = pos_to_loc Type.mk_var
let mktypconstr = pos_to_loc Type.mk_constr

let mktypdecl =pos_to_loc TypeDecl.mk
let mkpat = pos_to_loc Pattern.mk
let mkexp = pos_to_loc Expression.mk
let mkstr = pos_to_loc Statement.mk
%}
%token <int> INT
%token <string> LIDENT
%token <string> UIDENT
%token FUN
%token LET
%token TYPE
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
%token DOT
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
  | EOL rest = file
    { rest }
  | s = structure_item EOF
    { [s] }
  | s = structure_item SEMI rest = file
    { s :: rest }

structure_item:
  | bind = let_binding
    { let (x, e) = bind in
      mkstr ~pos:$loc (Value (x, e)) }
  | TYPE x = LIDENT k = type_kind
    { mkstr ~pos:$loc (Type (mkrhs x $loc(x), k)) }

type_kind:
  | (* empty *)
    { mktypdecl ~pos:$loc Abstract }
  | EQUAL t = type_expr
    { mktypdecl ~pos:$loc (Alias t) }
  | EQUAL LBRACE rev_fields = record_fields RBRACE
    { mktypdecl ~pos:$loc (Record (List.rev rev_fields)) }

record_field:
  | id = LIDENT COLON t = type_expr
    { { field_ident= mkrhs id $loc(id);
        field_type= t;
        field_loc= mklocation $loc } }

record_fields:
  | field = record_field
    { [field] }
  | fields = record_fields COMMA field = record_field
    { field :: fields }

simple_expr:
  | x = LIDENT
    { mkexp ~pos:$loc (Variable (mkrhs x $loc(x))) }
  | x = INT
    { mkexp ~pos:$loc (Int x) }
  | e = simple_expr DOT field = LIDENT
    { mkexp ~pos:$loc (Field (e, mkrhs field $loc(field))) }
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
  | LBRACE r = exp_record_fields RBRACE
    { let (fields, values) = List.fold_left (fun (fields, values) (id, e) ->
      let field =
        { field_ident= id
        ; field_type= Type.mk_var ~loc:id.loc None
        ; field_loc= id.loc }
      in
      (field :: fields, e :: values)) ([], []) r in
      mkexp ~pos:$loc (Record_literal
        { record_values= values
        ; record_fields= fields }) }

expr:
  | x = simple_expr %prec below_EXP
    { x }
  | f = simple_expr xs = simple_expr_list
    { mkexp ~pos:$loc (Apply (f, List.rev xs)) }
  | rev = expr_comma_list %prec below_COMMA
    { mkexp ~pos:$loc (Tuple (List.rev rev)) }

exp_record_field:
  | id = LIDENT COLON e = simple_expr
    { (mkrhs id $loc(id), e) }

exp_record_fields:
  | field = exp_record_field
    { [field] }
  | fields = exp_record_fields COMMA field = exp_record_field
    { field :: fields }

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
    { mktypconstr ~pos:$loc (mkrhs x $loc(x)) }
  | LBRACKET x = type_expr RBRACKET
    { x }

type_expr:
  | x = simple_type_expr
    { x }
  | x = simple_type_expr DASHGT y = type_expr
    { mktyp ~pos:$loc (Tarrow (x, y)) }
