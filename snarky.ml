open Meja_toplevel

let generated_name filename = Filename.chop_extension filename ^ "_gen"

let build_meja curve filename =
  let ocaml_file = generated_name filename ^ ".ml" in
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
    ; exn_backtraces= false } ;
  ocaml_file

let build_ocaml filename =
  let exe_name = Filename.chop_extension filename ^ ".exe" in
  (* TODO: OCaml build. *)
  exe_name

let complain = Format.(fprintf err_formatter)

let main () =
  let config = Snarky_args.parse () in
  let get_usage () = Snarky_args.usage_of_toplevel_config config in
  let filename, curve, args =
  match config.mode with
  | None ->
      complain "%s@." (get_usage ()) ;
      exit 2
  | Some (Keys config) ->
      let filename = match !config.filename with
      | Some filename -> filename
      | None ->
        complain "Error: No filename was given.@.%s@." (get_usage ());
        exit 2
      in
      let curve = match !config.curve with
      | Some curve -> curve
      | None -> complain "Error: The --curve argument is required.@.%s@." (get_usage ());
      exit 2
      in
      let curve_arg = "--curve=" ^ curve in
      let pk_arg = match !config.pk with
      | Some pk -> ["--proving-key=" ^ pk]
      | None -> []
      in
      let vk_arg = match !config.vk with
      | Some vk -> ["--verification-key=" ^ vk]
      | None -> []
      in
      filename, Some curve, ([curve_arg] @ pk_arg @ vk_arg)
  | Some (Prove config) ->
      let filename = match !config.filename with
      | Some filename -> filename
      | None ->
        complain "Error: No filename was given.@.%s@." (get_usage ());
        exit 2
      in
      let pk_arg = match !config.pk with
      | Some pk -> ["--proving-key=" ^ pk]
      | None -> []
      in
      (* TODO: witness. *)
      let public_input = List.rev !config.public_input_rev in
      filename, None, ([filename] @ pk_arg @ public_input)
  | Some (Verify config) ->
      let filename = match !config.filename with
      | Some filename -> filename
      | None ->
        complain "Error: No filename was given.@.%s@." (get_usage ());
        exit 2
      in
      let proof_filename = match !config.proof_filename with
      | Some filename -> [filename]
      | None -> []
      in
      let vk_arg = match !config.vk with
      | Some vk -> ["--verification-key=" ^ vk]
      | None -> []
      in
      let public_input = List.rev !config.public_input_rev in
      filename, None, ([filename] @ proof_filename @ vk_arg @ public_input)
  in
  let ocaml_filename = match curve with
    | Some curve -> build_meja curve filename
    | None -> generated_name filename ^ ".ml"
  in
  let exe_filename = build_ocaml ocaml_filename in
  Unix.execv exe_filename (Array.of_list args)

let () = main ()
