open Core_kernel

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error parse lexbuf =
  try parse lexbuf with Parser_impl.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf ;
    exit (-1)

let read_file parse filename =
  let file = In_channel.create filename in
  let lex = Lexing.from_channel file in
  (* Set filename in lex_curr_p. *)
  lex.Lexing.lex_curr_p
  <- {lex.Lexing.lex_curr_p with Lexing.pos_fname= filename} ;
  let ast = parse_with_error parse lex in
  In_channel.close file ; ast

let main =
  let files = ref [] in
  Arg.parse [] (fun filename -> files := filename :: !files) "" ;
  let files = List.rev !files in
  let asts =
    List.map files ~f:(read_file (Parser_impl.file Lexer_impl.token))
  in
  let ocaml_asts = List.map asts ~f:To_ocaml.of_file in
  ignore @@ List.map ocaml_asts ~f:(Pprintast.structure Format.std_formatter)
