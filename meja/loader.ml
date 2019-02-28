open Core_kernel
open Cmi_format

let load ~loc ~name env filename =
  Format.(fprintf err_formatter "Loading %s from %s...@." name filename) ;
  let cmi_info = read_cmi filename in
  let signature = Of_ocaml.to_signature cmi_info.cmi_sign in
  let scopes = env.Envi.scope_stack in
  let env =
    Envi.
      { env with
        scope_stack= Scope.empty Scope.Module :: Envi.Core.env.scope_stack }
  in
  let env = Typechecker.check_signature env signature in
  Format.(fprintf err_formatter "Loaded@.") ;
  let m, env = Envi.pop_module ~loc env in
  let env =
    { env with
      scope_stack= scopes
    ; external_modules=
        Map.set env.external_modules ~key:name ~data:(Envi.Immediate m) }
  in
  (m, env)

let () = Envi.load_module := load

let modname_of_filename file = String.capitalize (Filename.chop_extension file)

let load_directory env dirname =
  let files = try Sys.readdir dirname with Sys_error _ -> [||] in
  Array.fold ~init:env files ~f:(fun env file ->
      if String.equal (Filename.extension file) ".cmi" then
        let filename = Filename.concat dirname file in
        let module_name = modname_of_filename file in
        Envi.register_external_module module_name (Envi.Deferred filename) env
      else env )
