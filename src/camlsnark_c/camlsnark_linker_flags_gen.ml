open Printf
module C = Configurator.V1

let () =
  let uname_chan = Unix.open_process_in "uname" in
  let l = input_line uname_chan in
  C.Flags.write_sexp "flags.sexp"
    ( match l with
    | "Darwin" ->
        [ sprintf "-lcamlsnark_c_stubs"
        ; "-L/usr/local/opt/openssl/lib"
        ; "-lssl"
        ; "-lcrypto"
        ; "-lgmp"
        ; "-lgmpxx"
        ; "-lomp"
        ; "-lstdc++" ]
    | "Linux" ->
        [ "-Wl,-E"
        ; "-g"
        ; "-lcamlsnark_c_stubs"
        ; "-lgomp"
        ; "-lssl"
        ; "-lcrypto"
        ; "-lprocps"
        ; "-lgmp"
        ; "-lgmpxx"
        ; "-lstdc++" ]
    | s ->
        failwith (sprintf "don't know how to link on %s yet" s) )
