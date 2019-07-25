open Core_kernel
open Cmt_format
open Meja_lib

let load ~loc ~name resolve_env filename =
  (*Format.(fprintf err_formatter "Loading %s from %s...@." name filename) ;*)
  let cmi_info = read_cmi filename in
  let signature = Of_ocaml.to_signature cmi_info.cmi_sign in
  let env = {Initial_env.env with resolve_env} in
  let env = Envi.open_absolute_module (Some (Longident.Lident name)) env in
  let env, _ = Typechecker.check_signature env signature in
  (*Format.(fprintf err_formatter "Loaded@.") ;*)
  let m, _ = Envi.pop_module ~loc env in
  m

let () = Envi.Scope.load_module := load

let modname_of_filename file =
  String.capitalize (Filename.chop_extension (Filename.basename file))

let load_directory env dirname =
  let files = try Sys.readdir dirname with Sys_error _ -> [||] in
  Array.iter files ~f:(fun file ->
      match Filename.split_extension file with
      | _, Some ("cmi" | "cmti") ->
          let filename = Filename.concat dirname file in
          let module_name = Ident.create (modname_of_filename file) in
          Envi.register_external_module module_name (Envi.Deferred filename)
            env
      | _ ->
          () )
