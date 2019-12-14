open Core_kernel
open Meja_lib
open Meja_ocaml

type config =
  { file: string
  ; ocaml_file: string option
  ; ast_file: string option
  ; binml_file: string option
  ; default: bool
  ; stdlib: bool
  ; snarky_preamble: bool
  ; curve: string
  ; proofs: string
  ; impl_mod: string
  ; meji_files: string list
  ; cmi_files: string list
  ; cmi_dirs: string list
  ; exn_backtraces: bool }

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Format.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error parse lexbuf =
  let open Format in
  try parse lexbuf
  with Parser_impl.Error ->
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
  | None ->
      ()

let make_preamble impl_mod curve proofs =
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
    Pstmt_module
      ( mkloc impl_mod
      , {mod_desc= Pmod_name snarky_impl_path; mod_loc= Location.none} )
  in
  let snarky_open = Pstmt_open (mkloc (Lident impl_mod)) in
  let mk_stmt x = {stmt_desc= x; stmt_loc= Location.none} in
  [mk_stmt snarky_impl; mk_stmt snarky_open]

let add_preamble impl_mod curve proofs ast =
  make_preamble impl_mod curve proofs @ ast

let run
    { file
    ; ocaml_file
    ; ast_file
    ; binml_file
    ; default
    ; stdlib
    ; snarky_preamble
    ; curve
    ; proofs
    ; impl_mod
    ; meji_files
    ; cmi_files
    ; cmi_dirs
    ; exn_backtraces } =
  let env = Initial_env.env in
  Printexc.record_backtrace exn_backtraces ;
  try
    let env =
      if stdlib then (
        let stdlib_path = Findlib.ocaml_stdlib () in
        (* Load OCaml stdlib *)
        Loader.load_directory env stdlib_path ;
        let stdlib = Ident.create ~mode:Checked "Stdlib" in
        let stdlib_scope =
          Loader.load ~loc:Location.none ~name:"Stdlib" env.Envi.resolve_env
            (Filename.concat stdlib_path "stdlib.cmi")
        in
        Envi.register_external_module stdlib (Immediate stdlib_scope) env ;
        let env = Envi.open_namespace_scope (Pident stdlib) stdlib_scope env in
        (* Load Snarky *)
        let snarky_build_path =
          Filename.(
            Sys.executable_name |> dirname
            |> Fn.flip concat (concat parent_dir_name "src/.snarky.objs/"))
        in
        ( try Loader.load_directory env (Findlib.package_directory "snarky")
          with _ -> () ) ;
        Loader.load_directory env (Filename.concat snarky_build_path "byte") ;
        Loader.load_directory env (Filename.concat snarky_build_path "native") ;
        Loader.load_directory env snarky_build_path ;
        env )
      else env
    in
    List.iter cmi_dirs ~f:(Loader.load_directory env) ;
    let cmi_files = List.rev cmi_files in
    let cmi_scopes =
      List.map cmi_files ~f:(fun filename ->
          let modname = Loader.modname_of_filename filename in
          let modident = Ident.create ~mode:Checked modname in
          ( modident
          , Loader.load ~loc:Location.none ~name:modname env.Envi.resolve_env
              filename ) )
    in
    let env =
      List.fold ~init:env cmi_scopes ~f:(fun env (name, scope) ->
          Envi.open_namespace_scope (Path.Pident name) scope env )
    in
    let env =
      List.fold ~init:env meji_files ~f:(fun env file ->
          let parse_ast =
            read_file (Parser_impl.interface Lexer_impl.token) file
          in
          let module_name = Loader.modname_of_filename file in
          let env = Envi.open_module env in
          let env, _typed_ast = Typechecker.check_signature env parse_ast in
          let m, env = Envi.pop_module ~loc:Location.none env in
          let name = Ident.create ~mode:Checked module_name in
          Envi.add_module name m env )
    in
    let parse_ast =
      read_file (Parser_impl.implementation Lexer_impl.token) file
    in
    let _env, ast = Typechecker.check parse_ast env in
    let preamble =
      if snarky_preamble then
        Some (To_ocaml.of_file (make_preamble impl_mod curve proofs))
      else None
    in
    let ocaml_ast = Of_typedast.of_file ast in
    let ocaml_formatter =
      match (ocaml_file, default) with
      | Some filename, _ ->
          Some (Format.formatter_of_out_channel (Out_channel.create filename))
      | None, true ->
          Some Format.std_formatter
      | None, false ->
          None
    in
    do_output ast_file (fun output ->
        Option.iter ~f:(Printast.structure 2 output) preamble ;
        Format.pp_print_newline output () ;
        Printast.structure 2 output ocaml_ast ;
        Format.pp_print_newline output () ) ;
    ( match ocaml_formatter with
    | Some output ->
        Option.iter ~f:(Pprintast.structure output) preamble ;
        Format.pp_print_newline output () ;
        Pprintast.structure output ocaml_ast ;
        Format.pp_print_newline output ()
    | None ->
        () ) ;
    ( match binml_file with
    | Some file ->
        Pparse.write_ast Pparse.Structure file ocaml_ast
    | None ->
        () ) ;
    exit 0
  with exn ->
    ( if exn_backtraces then
      Format.(pp_print_string err_formatter (Printexc.get_backtrace ())) ) ;
    Location.report_exception Format.err_formatter exn ;
    exit 1
