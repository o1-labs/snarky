{
open Lexing
open Parser_impl

}

let newline = ('\r'* '\n')
let whitespace = [' ' '\t']+
let number = ['0'-'9']+
let lowercase_alpha = ['a'-'z']
let uppercase_alpha = ['A'-'Z']
let ident = ['a'-'z' 'A'-'Z' '_' '\'' '0'-'9']

rule token = parse
    whitespace
  { token lexbuf }
  | newline
    { new_line lexbuf; token lexbuf }
  | number
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "fun" { FUN }
  | "let" { LET }
  | "type" { TYPE }
  | "switch" { SWITCH }
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
  | ''' { QUOT }
  | '.' { DOT }
  | '|' { BAR }
  | lowercase_alpha ident* { LIDENT(Lexing.lexeme lexbuf) }
  | uppercase_alpha ident* { UIDENT(Lexing.lexeme lexbuf) }
  | _ { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }
  | eof { EOF }
