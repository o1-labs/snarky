open Core_kernel
open Meja_lib

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
  ; exn_backtraces: bool
  ; generate_cli: bool
  ; load_extlib: bool }

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

let read_string ?at_line parse filename contents =
  let lex = Lexing.from_string contents in
  (* Set filename and current line in lex_curr_p. *)
  let pos = {lex.Lexing.lex_curr_p with Lexing.pos_fname= filename} in
  let pos =
    match at_line with
    | Some line ->
        {pos with Lexing.pos_lnum= line}
    | None ->
        pos
  in
  lex.Lexing.lex_curr_p <- pos ;
  parse_with_error parse lex

let do_output filename f =
  match filename with
  | Some filename ->
      let output =
        Format.formatter_of_out_channel (Out_channel.create filename)
      in
      f output
  | None ->
      ()

let add_preamble impl_mod curve proofs ast =
  let open Parsetypes in
  let open Longident in
  let mkloc x = Location.(mkloc x none) in
  let dot y x = Ldot (x, y) in
  let snarky_make =
    Lident "Snarky" |> dot "Snark" |> dot "Run" |> dot "Make"
  in
  let backend_path =
    Lident "Snarky" |> dot "Backends" |> dot curve |> dot proofs
  in
  let unit_module = Lident "Core_kernel" |> dot "Unit" in
  let snarky_impl_path =
    mkloc (Lapply (Lapply (snarky_make, backend_path), unit_module))
  in
  let snarky_impl =
    Module
      ( mkloc impl_mod
      , {mod_desc= ModName snarky_impl_path; mod_loc= Location.none} )
  in
  let impl_open = Open (mkloc (Lident impl_mod)) in
  let snarky_open = Open (mkloc (Lident "Snarky")) in
  let snarky_snark_open = Open (mkloc (Ldot (Lident "Snarky", "Snark"))) in
  let mk_stmt x = {stmt_desc= x; stmt_loc= Location.none} in
  mk_stmt snarky_open :: mk_stmt snarky_snark_open :: mk_stmt snarky_impl
  :: mk_stmt impl_open :: ast

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
    ; exn_backtraces
    ; generate_cli
    ; load_extlib } =
  let env = Initial_env.env in
  Printexc.record_backtrace exn_backtraces ;
  try
    let env =
      if stdlib then (
        match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
        | Some opam_path ->
            let lib_path = Filename.concat opam_path "lib" in
            (* Load OCaml stdlib *)
            Loader.load_directory env (Filename.concat lib_path "ocaml") ;
            let stdlib_scope =
              Loader.load ~loc:Location.none ~name:"Stdlib"
                env.Envi.resolve_env
                (Filename.concat lib_path "ocaml/stdlib.cmi")
            in
            Envi.open_namespace_scope stdlib_scope Checked env
        | None ->
            Format.(
              fprintf err_formatter
                "Warning: OPAM_SWITCH_PREFIX environment variable is not set. \
                 Not loading the standard library.") ;
            env )
      else env
    in
    List.iter cmi_dirs ~f:(Loader.load_directory env) ;
    let cmi_files = List.rev cmi_files in
    let cmi_scopes =
      List.map cmi_files ~f:(fun filename ->
          Loader.load ~loc:Location.none
            ~name:(Loader.modname_of_filename filename)
            env.Envi.resolve_env filename )
    in
    let env =
      List.fold ~init:env cmi_scopes ~f:(fun env scope ->
          Envi.open_namespace_scope scope Checked env )
    in
    let env =
      let open Meja_stdlib in
      (* Load stdlib. *)
      let snark0_line, snark0 = Snark0.ocaml in
      let snark0_ast =
        read_string ~at_line:snark0_line
          (Parser_impl.interface Lexer_impl.token)
          "snark0" snark0
      in
      let env = Envi.open_continue_module Checked env in
      let env = Typechecker.check_signature' OCaml env snark0_ast in
      (* Localise stdlib to checked. *)
      let snark0_checked_line, snark0_checked = Snark0.checked in
      let snark0_checked_alias =
        read_string ~at_line:snark0_checked_line
          (Parser_impl.alias_interface Lexer_impl.token)
          "snark0_checked" snark0_checked
      in
      let env =
        Typechecker.import_alias ~in_mode:OCaml ~out_mode:Checked
          snark0_checked_alias env
      in
      (* Localise stdlib to prover. *)
      let snark0_prover_line, snark0_prover = Snark0.prover in
      let snark0_prover_alias =
        read_string ~at_line:snark0_prover_line
          (Parser_impl.alias_interface Lexer_impl.token)
          "snark0_prover" snark0_prover
      in
      let env =
        Typechecker.import_alias ~in_mode:OCaml ~out_mode:Prover
          snark0_prover_alias env
      in
      env
    in
    let meji_files =
      (*"meji/field.meji" :: "meji/boolean.meji" :: "meji/typ.meji" :: *)
      List.rev meji_files
    in
    let env =
      List.fold ~init:env meji_files ~f:(fun env file ->
          let parse_ast =
            read_file (Parser_impl.interface Lexer_impl.token) file
          in
          let module_name = Loader.modname_of_filename file in
          let env =
            Envi.open_absolute_module (Some (Longident.Lident module_name))
              Checked env
          in
          let env = Typechecker.check_signature env parse_ast in
          let m, env = Envi.pop_module ~loc:Location.none env in
          let name = Location.(mkloc module_name none) in
          Envi.add_module Checked name m env )
    in
    let env, extended_lib_ast =
      if load_extlib then
        (* TODO: This is a hack to get the "extended library" to be
         available. *)
        let parse_ast =
          read_string ~at_line:Meja_stdlib.Extended_lib.line
            (Parser_impl.implementation Lexer_impl.token)
            "extended_lib" Meja_stdlib.Extended_lib.t
        in
        Typechecker.check parse_ast env
      else (env, [])
    in
    let parse_ast =
      read_file (Parser_impl.implementation Lexer_impl.token) file
    in
    let env, ast = Typechecker.check parse_ast env in
    let ast = extended_lib_ast @ ast in
    let ast =
      if snarky_preamble then add_preamble impl_mod curve proofs ast else ast
    in
    let ast =
      if generate_cli then (
        (* Get the type of main. *)
        let main_typ, _ =
          try
            Envi.find_name ~loc:Location.none Checked
              Ast_build.(Loc.mk (Lid.of_name "main"))
              env
          with _ ->
            Format.(
              fprintf err_formatter "Error: This file has no function main.@.") ;
            exit 1
        in
        let main_typ = Envi.Type.flatten main_typ env in
        let rev_args, _ = Envi.Type.get_rev_args main_typ in
        (* Check return type of main. *)
        match rev_args with
        | [] ->
            Format.(
              fprintf err_formatter
                "Error: main must be a function.@ Try inserting fun () => { \
                 .. } around your main.@.") ;
            exit 1
        | hd :: tl ->
            ( try
                Typechecker.check_type Checked ~loc:Location.none env
                  Initial_env.Type.unit hd
              with _ ->
                Format.(
                  fprintf err_formatter
                    "Error: The final argument to main must be unit.@.") ;
                exit 1 ) ;
            let with_unit_typ =
              List.fold
                ~init:
                  (Envi.Type.mk Checked
                     (Tarrow
                        ( Initial_env.Type.unit
                        , Initial_env.Type.unit
                        , Explicit
                        , Nolabel ))
                     env)
                tl
                ~f:(fun typ _ ->
                  Envi.Type.mk Checked
                    (Tarrow
                       (Initial_env.Type.field Checked, typ, Explicit, Nolabel))
                    env )
            in
            ( try
                Typechecker.check_type Checked ~loc:Location.none env
                  with_unit_typ main_typ
              with err ->
                Format.(
                  fprintf err_formatter "Error: main has the wrong type.@.") ;
                raise err ) ;
            let public_input =
              List.fold ~init:Initial_env.Type.unit tl ~f:(fun typ _ ->
                  Envi.Type.mk Prover
                    (Tarrow
                       (Initial_env.Type.field Prover, typ, Explicit, Nolabel))
                    env )
            in
            let toplevel_param_module =
              Ast_build.(
                Stmt.module_ "Toplevel_param_module__"
                  (ModExp.struct_
                     [ Stmt.type_decl
                         (Type_decl.alias "result"
                            (Type.constr (Lid.of_name "unit")))
                     ; Stmt.type_decl
                         (Type_decl.alias "computation"
                            (Untype_ast.type_expr
                               (Envi.Type.normalise_constr_names OCaml env
                                  with_unit_typ)))
                     ; Stmt.type_decl
                         (Type_decl.alias "public_input"
                            (Untype_ast.type_expr
                               (Envi.Type.normalise_constr_names OCaml env
                                  public_input)))
                     ; Stmt.value (Pat.var "compute")
                         (Exp.var (Lid.of_name "main"))
                     ; Stmt.value (Pat.var "public_input")
                         (Exp.open_ (Lid.of_name "Data_spec")
                            (List.fold
                               ~init:(Exp.ctor (Lid.of_name "[]"))
                               tl
                               ~f:(fun exp _ ->
                                 Exp.ctor (Lid.of_name "::")
                                   ~args:
                                     (Exp.tuple
                                        [ Exp.var (Lid.of_list ["Field"; "typ"])
                                        ; exp ]) )))
                     ; Stmt.value (Pat.var "read_input")
                         (Exp.fun_ (Pat.var "a")
                            (Exp.match_
                               (Exp.var (Lid.of_name "a"))
                               [ ( List.foldi tl
                                     ~init:(Pat.ctor (Lid.of_name "[]"))
                                     ~f:(fun i pat _ ->
                                       Pat.ctor (Lid.of_name "::")
                                         ~args:
                                           (Pat.tuple
                                              [ Pat.var ("f_" ^ string_of_int i)
                                              ; pat ]) )
                                 , Exp.open_ (Lid.of_name "H_list")
                                     (List.foldi tl
                                        ~init:(Exp.ctor (Lid.of_name "[]"))
                                        ~f:(fun i exp _ ->
                                          Exp.ctor (Lid.of_name "::")
                                            ~args:
                                              (Exp.tuple
                                                 [ Exp.apply
                                                     (Exp.var
                                                        (Lid.of_list
                                                           [ "Field"
                                                           ; "Constant"
                                                           ; "of_string" ]))
                                                     [ ( Nolabel
                                                       , Exp.var
                                                           (Lid.of_name
                                                              ( "f_"
                                                              ^ string_of_int i
                                                              )) ) ]
                                                 ; exp ]) )) )
                               ; ( Pat.any ()
                                 , Exp.apply
                                     (Exp.var (Lid.of_name "failwith"))
                                     [ ( Nolabel
                                       , Exp.literal
                                           (String
                                              ( "Wrong number of arguments: \
                                                 expected "
                                              ^ string_of_int (List.length tl)
                                              )) ) ] ) ])) ]))
            in
            let toplevel_module =
              Ast_build.(
                Stmt.module_ "Toplevel_CLI_module__"
                  (ModExp.name
                     (Lid.apply
                        (Lid.apply
                           (Lid.of_list ["Snarky"; "Toplevel"; "Make"])
                           (Lid.of_name impl_mod))
                        (Lid.of_name "Toplevel_param_module__"))))
            in
            let call_main =
              Ast_build.(
                Stmt.value
                  (Pat.ctor (Lid.of_name "()"))
                  (Exp.apply
                     (Exp.var (Lid.of_list ["Toplevel_CLI_module__"; "main"]))
                     [(Nolabel, Exp.ctor (Lid.of_name "()"))]))
            in
            ast @ [toplevel_param_module; toplevel_module; call_main] )
      else ast
    in
    let ocaml_ast = To_ocaml.of_file ast in
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
        Printast.structure 2 output ocaml_ast ;
        Format.pp_print_newline output () ) ;
    ( match ocaml_formatter with
    | Some output ->
        Pprintast.structure output ocaml_ast ;
        Format.pp_print_newline output ()
    | None ->
        () ) ;
    match binml_file with
    | Some file ->
        Pparse.write_ast Pparse.Structure file ocaml_ast
    | None ->
        ()
  with exn ->
    ( if exn_backtraces then
      Format.(pp_print_string err_formatter (Printexc.get_backtrace ())) ) ;
    Location.report_exception Format.err_formatter exn ;
    exit 1
