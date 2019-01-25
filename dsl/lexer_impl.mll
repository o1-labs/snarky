{
open Parser_impl

let new_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1
  ; pos_bol = pos.pos_cnum
  }

}

rule token = parse
    [' ' '\t']+
  { token lexbuf }
  | ('\r'* '\n')
    { new_line lexbuf; token lexbuf }
  | ['0'-'9']+
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "fun" { FUN }
  | "let" { LET }
  | ';' { SEMI }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LBRACKET }
  | ')' { RBRACKET }
  | "->" { DASHGT }
  | "=>" { EQUALGT }
  | '=' { EQUAL }
  | ':' { COLON }
  | ',' { COMMA }
  | '_' { UNDERSCORE }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
    { VAR (Lexing.lexeme lexbuf) }
  | _ { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
  | eof { EOF }
