open Core_kernel
open Meja_lib

let do_output filename f =
  let output = Format.formatter_of_out_channel (Out_channel.create filename) in
  f output

let main =
  let outdir = ref (Sys.getcwd ()) in
  let files = ref [] in
  let cmi_dirs = ref [] in
  let arg_spec =
    [ ( "-o"
      , Arg.Set_string outdir
      , "output directory. \x1B[4mdefault: current directory\x1B[24m" )
    ; ( "-I"
      , Arg.String (fun dirname -> cmi_dirs := dirname :: !cmi_dirs)
      , "add a directory to the list of paths to search for .cmi files" ) ]
  in
  let usage_text =
    Format.sprintf "Usage:@.@[%s [options] files..@]@.@.OPTIONS:"
      (Filename.basename Sys.executable_name)
  in
  Arg.parse arg_spec (fun filename -> files := filename :: !files) usage_text ;
  let env = Initial_env.env in
  List.iter !cmi_dirs ~f:(Meja_ocaml.Loader.load_directory env) ;
  let files = List.rev !files in
  List.iter files ~f:(fun filename ->
      let cmi_info = Cmt_format.read_cmi filename in
      let signature = Meja_of_ocaml.to_signature cmi_info.cmi_sign in
      let out_file =
        Filename.(
          concat !outdir (chop_extension (basename filename) ^ ".meji"))
      in
      do_output out_file (fun out -> Pprint.signature out signature) )
