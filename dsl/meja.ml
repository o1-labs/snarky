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

let do_output filename f =
  match filename with
  | Some filename ->
      let output =
        Format.formatter_of_out_channel (Out_channel.create filename)
      in
      f output
  | None -> ()

let main =
  let file = ref None in
  let ocaml_file = ref None in
  let ast_file = ref None in
  let struct_file = ref None in
  let env_file = ref None in
  let ocaml_env_file = ref None in
  let default = ref true in
  Arg.parse
    [ ( "--ml"
      , String
          (fun name ->
            default := false ;
            ocaml_file := Some name )
      , "output OCaml code" )
    ; ( "--ast"
      , String
          (fun name ->
            default := false ;
            ast_file := Some name )
      , "output OCaml ast" )
    ; ( "--struct"
      , String
          (fun name ->
            default := false ;
            struct_file := Some name )
      , "output internal ast" )
    ; ( "--env"
      , String
          (fun name ->
            default := false ;
            env_file := Some name )
      , "output environment after evaluation" )
    ; ( "--ocaml-env"
      , String
          (fun name ->
            default := false ;
            ocaml_env_file := Some name )
      , "output environment in OCaml format after evaluation" )
    ; ( "--stderr"
      , String
          (fun name ->
            Format.pp_set_formatter_out_channel Format.err_formatter
              (Out_channel.create name) )
      , "redirect stderr to the given filename" ) ]
    (fun filename -> file := Some filename)
    "" ;
  let file =
    Option.value_exn !file
      ~error:(Error.of_string "Please pass a file as an argument.")
  in
  try
    let ast = read_file (Parser_impl.file Lexer_impl.token) file in
    let env = Typechecker.check ast in
    let ocaml_ast = To_ocaml.of_file ast in
    let ocaml_formatter =
      match (!ocaml_file, !default) with
      | Some filename, _ ->
          Some (Format.formatter_of_out_channel (Out_channel.create filename))
      | None, true -> Some Format.std_formatter
      | None, false -> None
    in
    do_output !ast_file (fun output ->
        Printast.structure 2 output ocaml_ast ;
        Format.pp_print_newline output () ) ;
    ( match ocaml_formatter with
    | Some output ->
        Pprintast.structure output ocaml_ast ;
        Format.pp_print_newline output ()
    | None -> () ) ;
    do_output !struct_file (fun output ->
        List.iter ast ~f:(fun stri ->
            Parsetypes.pp_statement output stri ;
            Format.pp_print_newline output () ) ) ;
    do_output !env_file (fun output -> Environ.pp output env) ;
    do_output !ocaml_env_file (fun output -> Environ.pp_ocaml output env) ;
    exit 0
  with exn ->
    Location.report_exception Format.err_formatter exn ;
    exit 1
