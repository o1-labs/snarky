open Meja_toplevel

let generated_name filename = Filename.chop_extension filename ^ "_gen"

let build curve filename =
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
    ; proofs= "default"
    ; impl_mod= "Impl"
    ; meji_files= []
    ; cmi_files= []
    ; cmi_dirs= ["."]
    ; exn_backtraces= false }

let main () =
  let config = Snarky_args.parse () in
  ignore config

let () = main ()
