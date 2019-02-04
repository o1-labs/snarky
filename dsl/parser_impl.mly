%{
open Location
open Parsetypes

let mklocation (loc_start, loc_end) = {loc_start; loc_end; loc_ghost= false}

let mkrhs rhs pos = mkloc rhs (mklocation pos)

let pos_to_loc ~pos (f : ?loc:Location.t -> 'b) = f ~loc:(mklocation pos)

let mktyp = pos_to_loc Type.mk
let mktypvar = pos_to_loc (Type.mk_var ~depth:0)
let mktypconstr = pos_to_loc Type.mk_constr

let mktypdecl =pos_to_loc TypeDecl.mk
let mkpat = pos_to_loc Pattern.mk
let mkexp = pos_to_loc Expression.mk
let mkstr = pos_to_loc Statement.mk

let make_fields ~pos r = 
  let (fields, values) = List.fold_left (fun (fields, values) (id, e) ->
    let field =
      { field_ident= id
      ; field_type= Type.mk_var ~loc:id.loc ~depth:0 None
      ; field_loc= id.loc }
    in
    (field :: fields, e :: values)) ([], []) r in
  mkexp ~pos (Record_literal
      { record_values= values
      ; record_fields= fields })
%}
%token <string> INT
%token <string> LIDENT
%token <string> UIDENT
%token FUN
%token LET
%token TYPE
%token SWITCH
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
%token QUOT
%token UNDERSCORE
%token DOT
%token BAR
%token EOF

%token EOL

%nonassoc below_COMMA
%left COMMA

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
  | EQUAL ctors = variant_list
    { mktypdecl ~pos:$loc (Variant (List.rev ctors)) }

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

variant_args:
  | (* empty *)
    { Constr_tuple [] }
  | LBRACKET rev_args = type_expr_list RBRACKET
    { Constr_tuple (List.rev rev_args) }
  | LBRACKET LBRACE fields = record_fields RBRACE RBRACKET
    { Constr_record (List.rev fields) }

variant_return_type:
  | (* empty *)
    { None }
  | COLON t = simple_type_expr
    { Some t }

constructor:
  | id = UIDENT
    { id }
  | LBRACKET RBRACKET
    { "()" }
  | TRUE
    { "true" }
  | FALSE
    { "false" }

variant:
  | id = constructor args = variant_args return_typ = variant_return_type
    { { constr_decl_ident= mkrhs id $loc(id)
      ; constr_decl_args= args
      ; constr_decl_return= return_typ
      ; constr_decl_loc= mklocation $loc } }

variant_list:
  | x = variant
    { [x] }
  | xs = variant_list BAR x = variant
    { x :: xs }

simple_expr:
  | x = LIDENT
    { mkexp ~pos:$loc (Variable (mkrhs x $loc(x))) }
  | x = INT
    { mkexp ~pos:$loc (Constant (Pconst_integer (x, None))) }
  | e = simple_expr DOT field = LIDENT
    { mkexp ~pos:$loc (Field (e, mkrhs field $loc(field))) }
  | FUN LBRACKET f = function_from_args
    { f }
  | LBRACKET es = expr_with_tuple RBRACKET
    { es }
  | LBRACE es = block RBRACE
    { es }
  | LBRACE r = exp_record_fields RBRACE
    { make_fields ~pos:$loc r }

arg_expr:
  | x = simple_expr
    { x }
  | id = constructor
    { mkexp ~pos:$loc (Constructor (mkrhs id $loc(id), None)) }

expr:
  | x = arg_expr
    { x }
  | id = constructor LBRACKET rev = expr_comma_list RBRACKET
    { mkexp ~pos:$loc (Constructor (mkrhs id $loc(id),
        Some (mkexp ~pos:$loc(rev) (Tuple (List.rev rev))))) }
  | id = constructor LBRACE r = exp_record_fields RBRACE
    { mkexp ~pos:$loc (Constructor (mkrhs id $loc(id), Some (make_fields ~pos:$loc(r) r))) }
  | id = constructor LBRACKET e = expr RBRACKET
    { mkexp ~pos:$loc (Constructor (mkrhs id $loc(id), Some e)) }
  | f = simple_expr xs = arg_expr_list
    { mkexp ~pos:$loc (Apply (f, List.rev xs)) }
  | SWITCH LBRACKET e = expr_with_tuple RBRACKET LBRACE rev_cases = match_cases RBRACE
    { mkexp ~pos:$loc (Match (e, List.rev rev_cases)) }

expr_with_tuple:
  | x = expr
    { x }
  | rev = expr_comma_list %prec below_COMMA
    { mkexp ~pos:$loc (Tuple (List.rev rev)) }

expr_with_bind:
  | x = expr_with_tuple
    { x }
  | bind = let_binding SEMI rhs = expr_with_bind
    { let (x, lhs) = bind in
      mkexp ~pos:$loc (Let (x, lhs, rhs)) }

exp_record_field:
  | id = LIDENT COLON e = simple_expr
    { (mkrhs id $loc(id), e) }

exp_record_fields:
  | field = exp_record_field
    { [field] }
  | fields = exp_record_fields COMMA field = exp_record_field
    { field :: fields }

match_case:
  | BAR p = pat EQUALGT e = expr_with_tuple
    { (p, e) }

match_cases:
  | c = match_case
    { [c] }
  | cs = match_cases c = match_case
    { c :: cs }

let_binding:
  | LET x = pat EQUAL e = expr_with_tuple
    { (x, e) }

arg_expr_list:
  | x = arg_expr
    { [x] }
  | l = arg_expr_list x = arg_expr
    { x :: l }

expr_comma_list:
  | l = expr_comma_list COMMA e = expr_with_tuple
    { e :: l }
  | e1 = expr_with_tuple COMMA e2 = expr_with_tuple
    { [e2; e1] }

function_from_args:
  | p = pat RBRACKET EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, body)) }
  | p = pat RBRACKET typ = type_expr EQUALGT LBRACE body = block RBRACE
    { mkexp ~pos:$loc (Fun (p, mkexp ~pos:$loc (Constraint {econ_exp= body; econ_typ= typ}))) }
  | p = pat COMMA f = function_from_args
    { mkexp ~pos:$loc (Fun (p, f)) }

opt_semi:
  | (* empty *)
    { }
  | SEMI
    { }

block:
  | e = expr_with_bind opt_semi
    { e }
  | bind = let_binding opt_semi
    { let (x, lhs) = bind in
      mkexp ~pos:$loc (Let (x, lhs, mkexp ~pos:$loc (Tuple []))) }
  | e1 = expr_with_bind SEMI rest = block
    { mkexp ~pos:$loc (Seq (e1, rest)) }

pat_without_or:
  | UNDERSCORE
    { mkpat ~pos:$loc PAny }
  | x = INT
    { mkpat ~pos:$loc (PConstant (Pconst_integer (x, None))) }
  | LBRACKET p = pat RBRACKET
    { p }
  | p = pat_without_or COLON typ = type_expr
    { mkpat ~pos:$loc (PConstraint {pcon_pat= p; pcon_typ= typ}) }
  | x = LIDENT
    { mkpat ~pos:$loc (PVariable (mkrhs x $loc(x))) }
  | LBRACE p = field_pats RBRACE
    { let (fields, closed) = p in
      mkpat ~pos:$loc (PRecord (fields, closed)) }
  | LBRACKET ps = pat_comma_list RBRACKET
    { mkpat ~pos:$loc (PTuple (List.rev ps)) }

pat:
  | p = pat_without_or
    { p }
  | p1 = pat_without_or BAR p2 = pat
    { mkpat ~pos:$loc (POr (p1, p2)) }

pat_comma_list:
  | ps = pat_comma_list COMMA p = pat
    {p :: ps }
  | p1 = pat COMMA p2 = pat
    { [p1; p2] }

field_pat:
  | label = LIDENT COLON p = pat
    { (mkrhs label $loc(label), p) }
  | label = LIDENT
    { (mkrhs label $loc(label), mkpat ~pos:$loc (PVariable (mkrhs label $loc(label)))) }

field_pats:
  | p = field_pat
    { ([p], Asttypes.Closed) }
  | p = field_pat SEMI UNDERSCORE
    { ([p], Asttypes.Open) }
  | p = field_pat SEMI ps = field_pats
    { let (fields, closed) = ps in (p :: fields, closed) }

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

type_expr_list:
  | x = type_expr
    { [x] }
  | xs = type_expr_list COMMA x = type_expr
    { x :: xs }
