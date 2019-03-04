open Core_kernel

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Format.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error parse lexbuf =
  let open Format in
  try parse lexbuf with Parser_impl.Error ->
    fprintf err_formatter "%a: syntax error\n" print_position lexbuf ;
    pp_print_flush err_formatter () ;
    exit 1

let read_file parse filename =
  let file = In_channel.create filename in
  let lex = Lexing.from_channel file in
  (* Set filename in lex_curr_p. *)
  lex.Lexing.lex_curr_p
  <- {lex.Lexing.lex_curr_p with Lexing.pos_fname= filename} ;
  let ast = parse_with_error parse lex in
  In_channel.close file ; ast

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
  let binml_file = ref None in
  let default = ref true in
  let set_and_clear_default opt name =
    default := false ;
    opt := Some name
  in
  let arg_spec =
    [ ( "--ml"
      , Arg.String (set_and_clear_default ocaml_file)
      , "output OCaml code" )
    ; ("--ast", Arg.String (set_and_clear_default ast_file), "output OCaml ast")
    ; ( "--stderr"
      , Arg.String
          (fun name ->
            Format.pp_set_formatter_out_channel Format.err_formatter
              (Out_channel.create name) )
      , "redirect stderr to the given filename" )
    ; ( "--binml"
      , Arg.String (set_and_clear_default binml_file)
      , "output a binary ml file" ) ]
  in
  Arg.parse arg_spec (fun filename -> file := Some filename) "" ;
  let env = Envi.Core.env in
  try
    let file =
      Option.value_exn !file
        ~error:(Error.of_string "Please pass a file as an argument.")
    in
    let parse_ast = read_file (Parser_impl.file Lexer_impl.token) file in
    let _env, ast = Typechecker.check parse_ast env in
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
    ( match !binml_file with
    | Some file -> Pparse.write_ast Pparse.Structure file ocaml_ast
    | None -> () ) ;
    exit 0
  with exn ->
    Location.report_exception Format.err_formatter exn ;
    exit 1
