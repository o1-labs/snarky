open Meja_toplevel

let bare_name filename = Filename.(basename (chop_extension filename))

let generated_name filename = bare_name filename ^ "_gen"

let build_meja generate_cli dir curve filename =
  let ocaml_file = Filename.(concat dir (generated_name filename ^ ".ml")) in
  Meja.run
    { file= filename
    ; ocaml_file= Some ocaml_file
    ; ast_file= None
    ; binml_file= None
    ; default= false
    ; stdlib= true
    ; snarky_preamble= true
    ; curve
    ; proofs= "Default"
    ; impl_mod= "Impl"
    ; meji_files= []
    ; cmi_files= []
    ; cmi_dirs= ["."]
    ; exn_backtraces= false
    ; generate_cli
    ; load_extlib= true } ;
  ocaml_file

let build_dune_file dir filename =
  let dunefile = open_out Filename.(concat dir "dune") in
  output_string dunefile
    ( {|
(executable
  (name |} ^ filename
    ^ {|)
  (libraries snarky core_kernel)
  (flags (:standard -short-paths -w -A)))
|}
    ) ;
  close_out dunefile

let complain = Format.(fprintf err_formatter)

let main () =
  let config = Snarky_args.parse () in
  let get_usage () = Snarky_args.usage_of_toplevel_config config in
  let filename, curve, run_cmd, args =
    match config.mode with
    | None ->
        complain "%s@." (get_usage ()) ;
        exit 2
    | Some (Build config) ->
        let filename =
          match !config.filename with
          | Some filename ->
              filename
          | None ->
              complain "Error: No filename was given.@.%s@." (get_usage ()) ;
              exit 2
        in
        let curve =
          match !config.curve with
          | Some curve ->
              curve
          | None ->
              complain "Error: The --curve argument is required.@.%s@."
                (get_usage ()) ;
              exit 2
        in
        (filename, Some curve, false, [])
    | Some (Keys config) ->
        let filename =
          match !config.filename with
          | Some filename ->
              filename
          | None ->
              complain "Error: No filename was given.@.%s@." (get_usage ()) ;
              exit 2
        in
        let curve =
          match !config.curve with
          | Some curve ->
              curve
          | None ->
              complain "Error: The --curve argument is required.@.%s@."
                (get_usage ()) ;
              exit 2
        in
        let pk_arg =
          match !config.pk with
          | Some pk ->
              ["--proving-key=" ^ pk]
          | None ->
              ["--proving-key=" ^ bare_name filename ^ ".pk"]
        in
        let vk_arg =
          match !config.vk with
          | Some vk ->
              ["--verification-key=" ^ vk]
          | None ->
              ["--verification-key=" ^ bare_name filename ^ ".vk"]
        in
        (filename, Some curve, true, ["generate-keys"] @ pk_arg @ vk_arg)
    | Some (Prove config) ->
        let filename =
          match !config.filename with
          | Some filename ->
              filename
          | None ->
              complain "Error: No filename was given.@.%s@." (get_usage ()) ;
              exit 2
        in
        let pk_arg =
          match !config.pk with
          | Some pk ->
              ["--proving-key=" ^ pk]
          | None ->
              ["--proving-key=" ^ bare_name filename ^ ".pk"]
        in
        (* TODO: witness. *)
        let public_input = List.rev !config.public_input_rev in
        (filename, None, true, ["prove"] @ pk_arg @ public_input)
    | Some (Verify config) ->
        let filename =
          match !config.filename with
          | Some filename ->
              filename
          | None ->
              complain "Error: No filename was given.@.%s@." (get_usage ()) ;
              exit 2
        in
        let proof_filename =
          match !config.proof_filename with
          | Some filename ->
              [filename]
          | None ->
              [bare_name filename ^ ".zkp"]
        in
        let vk_arg =
          match !config.vk with
          | Some vk ->
              ["--verification-key=" ^ vk]
          | None ->
              ["--verification-key=" ^ bare_name filename ^ ".vk"]
        in
        let public_input = List.rev !config.public_input_rev in
        ( filename
        , None
        , true
        , ["verify"] @ proof_filename @ vk_arg @ public_input )
  in
  (* Make filename.snarky.build directory *)
  let dirname = "." ^ bare_name filename ^ ".snarky.build" in
  ( try Unix.mkdir dirname 0o777
    with Unix.Unix_error (Unix.EEXIST, _, _) -> () ) ;
  (* Generate the .ml file *)
  let bare_name =
    match curve with
    | Some curve ->
        let ocaml_name = build_meja run_cmd dirname curve filename in
        let bare_name = bare_name ocaml_name in
        (* Generate the dune file *)
        build_dune_file dirname bare_name ;
        bare_name
    | None ->
        let ocaml_name = generated_name filename ^ ".ml" in
        bare_name ocaml_name
  in
  if run_cmd then (
    let dune_args =
      ["dune"; "exec"; "--root"; dirname; "./" ^ bare_name ^ ".exe"; "--"]
      @ args
    in
    Format.(
      fprintf err_formatter "%a@."
        (pp_print_list
           ~pp_sep:(fun fmt () -> pp_print_string fmt " ")
           pp_print_string))
      dune_args ;
    Unix.execvp "dune" (Array.of_list dune_args) )
  else
    Unix.execvp "dune"
      (Array.of_list
         ["dune"; "build"; "--root"; dirname; "./" ^ bare_name ^ ".exe"])

let () = main ()
