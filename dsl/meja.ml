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

let typecheck asts = ignore @@ List.map asts ~f:Typechecker.check

let main =
  let file = ref None in
  let ocaml_file = ref None in
  let ast_file = ref None in
  let default = ref true in
  Arg.parse
    [ ( "ml"
      , String
          (fun name ->
            default := false ;
            ocaml_file := Some name )
      , "output OCaml code" )
    ; ( "ast"
      , String
          (fun name ->
            default := false ;
            ast_file := Some name )
      , "output OCaml ast" ) ]
    (fun filename -> file := Some filename)
    "" ;
  let file =
    Option.value_exn !file
      ~error:(Error.of_string "Please pass a file as an argument.")
  in
  let ast = read_file (Parser_impl.file Lexer_impl.token) file in
  ignore (Typechecker.check ast) ;
  let ocaml_ast = To_ocaml.of_file ast in
  let ocaml_formatter =
    match (!ocaml_file, !default) with
    | Some filename, _ ->
        Some (Format.formatter_of_out_channel (Out_channel.create filename))
    | None, true -> Some Format.std_formatter
    | None, false -> None
  in
  ( match !ast_file with
  | Some filename ->
      let output =
        Format.formatter_of_out_channel (Out_channel.create filename)
      in
      Printast.structure 2 output ocaml_ast ;
      Format.pp_print_newline output ()
  | None -> () ) ;
  match ocaml_formatter with
  | Some output ->
      Pprintast.structure output ocaml_ast ;
      Format.pp_print_newline output ()
  | None -> ()
