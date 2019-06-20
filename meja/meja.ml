open Core_kernel

let main =
  let file = ref None in
  let ocaml_file = ref None in
  let ast_file = ref None in
  let binml_file = ref None in
  let default = ref true in
  let stdlib = ref true in
  let snarky_preamble = ref true in
  let curve = ref "Mnt4" in
  let proofs = ref "Default" in
  let impl_mod = ref "Impl" in
  let set_and_clear_default opt name =
    default := false ;
    opt := Some name
  in
  let meji_files = ref [] in
  let cmi_files = ref [] in
  let cmi_dirs = ref [] in
  let exn_backtraces = ref false in
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
    ; ( "--meji"
      , Arg.String (fun filename -> meji_files := filename :: !meji_files)
      , "load a .meji interface file" )
    ; ( "--load-cmi"
      , Arg.String (fun filename -> cmi_files := filename :: !cmi_files)
      , "load a .cmi file" )
    ; ( "-I"
      , Arg.String (fun dirname -> cmi_dirs := dirname :: !cmi_dirs)
      , "add a directory to the list of paths to search for .cmi files" )
    ; ( "--stdlib"
      , Arg.Set stdlib
      , "load the OCaml standard library \x1B[4mdefault\x1B[24m" )
    ; ( "--no-stdlib"
      , Arg.Clear stdlib
      , "do not load the OCaml standard library" )
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
         \x1B[4mdefault: Impl\x1B[24m" )
    ; ( "--compiler-backtraces"
      , Arg.Set exn_backtraces
      , "show a backtrace through the compiler when an error is encountered" )
    ]
  in
  let usage_text =
    Format.sprintf "Usage:@.@[%s [options] file@]@.@.OPTIONS:"
      (Filename.basename Sys.executable_name)
  in
  Arg.parse arg_spec
    (fun filename ->
      match !file with
      | Some _ ->
          Arg.usage arg_spec usage_text ;
          exit 1
      | None ->
          file := Some filename )
    usage_text ;
  let file =
    match !file with
    | Some file ->
        file
    | None ->
        Arg.usage arg_spec usage_text ;
        exit 1
  in
  Meja_toplevel.Meja.run
    { file
    ; ocaml_file= !ocaml_file
    ; ast_file= !ast_file
    ; binml_file= !binml_file
    ; default= !default
    ; stdlib= !stdlib
    ; snarky_preamble= !snarky_preamble
    ; curve= !curve
    ; proofs= !proofs
    ; impl_mod= !impl_mod
    ; meji_files= !meji_files
    ; cmi_files= !cmi_files
    ; cmi_dirs= !cmi_dirs
    ; exn_backtraces= !exn_backtraces
    ; generate_cli= false
    ; load_extlib= false } ;
  exit 0
