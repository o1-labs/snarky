{
open Location
open Parser_impl
open Parser_errors

let new_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1
  ; pos_bol = pos.pos_cnum
  }

let mklocation (loc_start, loc_end) = {loc_start; loc_end; loc_ghost= false}

let lexeme_loc lexbuf =
  mklocation (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
}

let newline = ('\r'* '\n')
let whitespace = [' ' '\t']+
let number = ['0'-'9']+
let lowercase_alpha = ['a'-'z']
let uppercase_alpha = ['A'-'Z']
let ident = ['a'-'z' 'A'-'Z' '_' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']

rule token = parse
    whitespace
  { token lexbuf }
  | newline
    { new_line lexbuf; token lexbuf }
  | number
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "fun" { FUN }
  | "let" { LET }
  | "instance" { INSTANCE }
  | "type" { TYPE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "switch" { SWITCH }
  | "type" { TYPE }
  | "module" { MODULE }
  | "open" { OPEN }
  | ';' { SEMI }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LBRACKET }
  | ')' { RBRACKET }
  | "->" { DASHGT }
  | "=>" { EQUALGT }
  | "+=" { PLUSEQUAL }
  | '=' { EQUAL }
  | ':' { COLON }
  | ',' { COMMA }
  | '_' { UNDERSCORE }
  | '|' { BAR }
  | ''' { QUOT }
  | "..." { DOTDOTDOT }
  | ".." { DOTDOT }
  | '.' { DOT }

  | "!" symbolchar * as op { PREFIXOP op }
  | ['~' '?'] symbolchar + as op { PREFIXOP op }
  | ['=' '<' '>' '|' '&' '$'] symbolchar * as op { INFIXOP0 op }
  | ['@' '^'] symbolchar * as op { INFIXOP1 op }
  | ['+' '-'] symbolchar * as op { INFIXOP2 op }
  | "**" symbolchar * as op { INFIXOP4 op }
  | ['*' '/' '%'] symbolchar * as op { INFIXOP3 op }
  | lowercase_alpha ident* { LIDENT(Lexing.lexeme lexbuf) }
  | uppercase_alpha ident* { UIDENT(Lexing.lexeme lexbuf) }
  | _ { raise (Error (lexeme_loc lexbuf, Unexpected_character (Lexing.lexeme lexbuf))) }
  | eof { EOF }
