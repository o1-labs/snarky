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

let add_preamble impl_mod curve proofs ast =
  let open Parsetypes in
  let open Longident in
  let mkloc x = Location.(mkloc x none) in
  let dot y x = Ldot (x, y) in
  let snarky_make = Lident "Snarky" |> dot "Snark" |> dot "Make" in
  let backend_path =
    Lident "Snarky" |> dot "Backends" |> dot curve |> dot proofs
  in
  let snarky_impl_path = mkloc (Lapply (snarky_make, backend_path)) in
  let snarky_impl =
    Module
      ( mkloc impl_mod
      , {mod_desc= ModName snarky_impl_path; mod_loc= Location.none} )
  in
  let snarky_open = Open (mkloc (Lident impl_mod)) in
  let mk_stmt x = {stmt_desc= x; stmt_loc= Location.none} in
  mk_stmt snarky_impl :: mk_stmt snarky_open :: ast

let main =
  let file = ref None in
  let ocaml_file = ref None in
  let ast_file = ref None in
  let binml_file = ref None in
  let default = ref true in
  let snarky_preamble = ref true in
  let curve = ref "Mnt4" in
  let proofs = ref "Default" in
  let impl_mod = ref "Impl" in
  let set_and_clear_default opt name =
    default := false ;
    opt := Some name
  in
  let cmi_files = ref [] in
  let cmi_dirs = ref [] in
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
      , "output a binary ml file" )
    ; ( "--load-cmi"
      , Arg.String (fun filename -> cmi_files := filename :: !cmi_files)
      , "load a .cmi file" )
    ; ( "-I"
      , Arg.String (fun dirname -> cmi_dirs := dirname :: !cmi_dirs)
      , "add a directory to the list of paths to search for .cmi files" )
    ; ("--preamble", Arg.Set snarky_preamble, "output the snarky preamble")
    ; ( "--no-preamble"
      , Arg.Clear snarky_preamble
      , "do not output snarky preamble" )
    ; ( "--curve"
      , Arg.Set_string curve
      , "set the elliptic curve to use \x1B[4mdefault: Mnt4\x1B[24m" )
    ; ("--proofs", Arg.Set_string proofs, "set the snarky proof system to use")
    ; ( "--impl-name"
      , Arg.Set_string impl_mod
      , "set the name to give to the snarky implementation module \
         \x1B[4mdefault: Impl\x1B[24m" ) ]
  in
  let usage_text =
    Format.sprintf "Usage:@.@[%s [options] file@]@.@.OPTIONS:"
      (Filename.basename Sys.executable_name)
  in
  Arg.parse arg_spec (fun filename -> file := Some filename) usage_text ;
  let env = Envi.Core.env in
  let env =
    if true then (
      match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
      | Some opam_path ->
          let lib_path = Filename.concat opam_path "lib" in
          let snarky_build_path =
            Filename.(
              Sys.executable_name |> dirname
              |> Fn.flip concat
                   (concat parent_dir_name "src/.snarky.objs/byte/"))
          in
          Loader.load_directory env (Filename.concat lib_path "snarky") ;
          Loader.load_directory env snarky_build_path ;
          let m, env =
            let loc = Location.none in
            let mkloc s = Location.mkloc s loc in
            let env = Envi.open_module env in
            let env = Envi.open_module env in
            let m =
              try
                Envi.find_module ~loc
                  (mkloc (Longident.Lident "Snarky__Request"))
                  env
              with _ ->
                Format.(
                  fprintf err_formatter
                    "Could not find the compiled inteface files for Snarky.") ;
                exit 1
            in
            let env = Envi.add_module (mkloc "Request") m env in
            let m, env = Envi.pop_module ~loc env in
            let env = Envi.add_module (mkloc "Snarky") m env in
            Envi.pop_module ~loc env
          in
          Envi.open_namespace_scope m env
      | None ->
          Format.(
            fprintf err_formatter
              "Warning: OPAM_SWITCH_PREFIX environment variable is not set. \
               Not loading the standard library.") ;
          env )
    else env
  in
  List.iter !cmi_dirs ~f:(Loader.load_directory env) ;
  try
    let cmi_files = List.rev !cmi_files in
    let cmi_scopes =
      List.map cmi_files ~f:(fun filename ->
          Loader.load ~loc:Location.none
            ~name:(Loader.modname_of_filename filename)
            env.Envi.resolve_env filename )
    in
    let env =
      List.fold ~init:env cmi_scopes ~f:(fun env scope ->
          Envi.open_namespace_scope scope env )
    in
    let file =
      match !file with
      | Some file -> file
      | None ->
          Arg.usage arg_spec usage_text ;
          exit 1
    in
    let parse_ast = read_file (Parser_impl.file Lexer_impl.token) file in
    let _env, ast = Typechecker.check parse_ast env in
    let ast =
      if !snarky_preamble then add_preamble !impl_mod !curve !proofs ast
      else ast
    in
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
