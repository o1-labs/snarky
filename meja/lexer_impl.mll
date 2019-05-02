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

let comment_buffer = Buffer.create 256
let store_lexeme lexbuf =
  Buffer.add_string comment_buffer (Lexing.lexeme lexbuf)

let get_comment comment lexbuf =
  Buffer.reset comment_buffer;
  comment lexbuf;
  let s = Buffer.contents comment_buffer in
  Buffer.reset comment_buffer;
  s
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
  | "request" { REQUEST }
  | "with" { WITH }
  | "handler" { HANDLER }
  | ';' { SEMI }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "->" { DASHGT }
  | "=>" { EQUALGT }
  | "~" { TILDE }
  | "?" { QUESTION }
  | "+=" { PLUSEQUAL }
  | '=' { EQUAL }
  | ':' { COLON }
  | "::" { COLONCOLON }
  | ',' { COMMA }
  | '_' { UNDERSCORE }
  | '|' { BAR }
  | ''' { QUOT }
  | "..." { DOTDOTDOT }
  | ".." { DOTDOT }
  | '.' { DOT }
  | "//" ([^'\n']* as comment) newline
    { new_line lexbuf; COMMENT (comment) }
  | "//" ([^'\n']* as comment) eof
    { new_line lexbuf; COMMENT (comment) }
  | "/*"
    { let comment = get_comment comment lexbuf in
      COMMENT (comment) }

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

and comment = parse
  | "*/"
    { store_lexeme lexbuf }
  | newline
    { new_line lexbuf; store_lexeme lexbuf; comment lexbuf }
  | _
    { store_lexeme lexbuf; comment lexbuf }

{
  let token lexbuf =
    let rec go () =
      match token lexbuf with
      | COMMENT _ -> go ()
      | tok -> tok
    in
    go ()
}
