open Core_kernel
open Snarky_libsnark_bindings

let path = Filename.chop_extension (Filename.basename Sys.executable_name)

module Commands = struct
  module type Command = sig
    type config

    val empty_config : config

    val name : Arg.key

    val description : Arg.doc

    val spec : config ref -> (Arg.key * Arg.spec * Arg.doc) list

    val usage : Arg.usage_msg

    val anon_fun : config ref -> Arg.anon_fun
  end

  module Generate_keys = struct
    type config = {pk: string option; vk: string option}

    let empty_config = {pk= None; vk= None}

    let name = "generate-keys"

    let description = "generate a keypair"

    let usage = "[options..]"

    let spec config =
      let pk pk = config := {!config with pk= Some pk} in
      let vk vk = config := {!config with vk= Some vk} in
      [ ( "--proving-key"
        , Arg.String pk
        , Printf.sprintf
            "specify a filename for the proving key. Default is %s.pk" path )
      ; ( "--verification-key"
        , Arg.String vk
        , Printf.sprintf
            "specify a filename for the verification key. Default is %s.vk"
            path )
      ; ("-pk", Arg.String pk, "alias of --proving-key")
      ; ("-vk", Arg.String vk, "alias of --verification-key") ]

    let anon_fun _config filename =
      raise (Arg.Bad (Printf.sprintf "Invalid argument %s" filename))
  end

  module Prove = struct
    type config =
      { public_input: string option
      ; output_path: string option
      ; pk: string option }

    let empty_config = {public_input= None; output_path= None; pk= None}

    let name = "prove"

    let description = "generate a zkSNARK proof"

    let usage = "prove [options..] PUBLIC_INPUT"

    let spec config =
      let pk pk = config := {!config with pk= Some pk} in
      let out out = config := {!config with output_path= Some out} in
      [ ( "--proving-key"
        , Arg.String pk
        , Printf.sprintf
            "specify a filename for the proving key. Default is %s.pk" path )
      ; ("-pk", Arg.String pk, "alias of --proving-key")
      ; ( "--output"
        , Arg.String out
        , Printf.sprintf "specify a filename for the proof. Default is %s.zkp"
            path )
      ; ("-o", Arg.String out, "alias of --output") ]

    let anon_fun config data =
      match !config.public_input with
      | Some public_input ->
          config :=
            {!config with public_input= Some (public_input ^ " " ^ data)}
      | None ->
          config := {!config with public_input= Some data}
  end

  module Verify = struct
    type config =
      {filename: string option; public_input: string option; vk: string option}

    let empty_config = {filename= None; public_input= None; vk= None}

    let name = "verify"

    let description = "verify a zkSNARK proof"

    let usage = "verify [options..] filename.zkp PUBLIC_INPUT"

    let spec config =
      let vk vk = config := {!config with vk= Some vk} in
      [ ( "--verification-key"
        , Arg.String vk
        , Printf.sprintf
            "specify a filename for the verification key. Default is %s.vk"
            path )
      ; ("-vk", Arg.String vk, "alias of --verification-key") ]

    let anon_fun config data =
      match !config.filename with
      | Some _ -> (
        match !config.public_input with
        | Some public_input ->
            config :=
              {!config with public_input= Some (public_input ^ " " ^ data)}
        | None ->
            config := {!config with public_input= Some data} )
      | None ->
          config := {!config with filename= Some data}
  end

  module Toplevel = struct
    type mode =
      | Keys of Generate_keys.config ref
      | Prove of Prove.config ref
      | Verify of Verify.config ref

    type config =
      { path: string
      ; spec: (Arg.key * Arg.spec * Arg.doc) list ref
      ; usage: Arg.usage_msg
      ; anon_fun: Arg.anon_fun
      ; mode: mode option }

    let select_command (type cmd_config)
        ~cmd:(module Command : Command with type config = cmd_config)
        (config : config ref) ~(store_config : cmd_config ref -> unit)
        (arg : string) =
      if String.equal arg Command.name then (
        let cmd_config = ref Command.empty_config in
        store_config cmd_config ;
        !config.spec := Command.spec cmd_config ;
        config :=
          { !config with
            anon_fun= Command.anon_fun cmd_config
          ; usage= Command.usage
          ; path= !config.path ^ " " ^ Command.name } ;
        true )
      else false

    let usage =
      {|[generate-keys | prove | verify] [options..]

  generate-keys  |}
      ^ Generate_keys.description ^ {|
  prove  |} ^ Prove.description
      ^ {|
  verify  |} ^ Verify.description

    let spec = []

    let anon_fun config data =
      if
        select_command config data
          ~cmd:(module Generate_keys)
          ~store_config:(fun mode_config ->
            config := {!config with mode= Some (Keys mode_config)} )
      then ()
      else if
        select_command config data
          ~cmd:(module Prove)
          ~store_config:(fun mode_config ->
            config := {!config with mode= Some (Prove mode_config)} )
      then ()
      else if
        select_command config data
          ~cmd:(module Verify)
          ~store_config:(fun mode_config ->
            config := {!config with mode= Some (Verify mode_config)} )
      then ()
      else
        let of_key (module M : Command) =
          (M.name, Arg.Unit (fun _ -> ()), M.description)
        in
        (* Set the arguments so that we generate a full spec. *)
        !config.spec :=
          [ of_key (module Generate_keys)
          ; of_key (module Prove)
          ; of_key (module Verify) ] ;
        raise (Arg.Bad (Printf.sprintf "Unknown argument %s" data))

    let config () =
      let config =
        ref
          { path= Filename.basename Sys.executable_name
          ; spec= ref spec
          ; usage
          ; anon_fun= (fun _ -> ())
          ; mode= None }
      in
      config := {!config with anon_fun= anon_fun config} ;
      config
  end
end

let parse () =
  let config = Commands.Toplevel.config () in
  try
    Arg.parse_argv_dynamic Sys.argv !config.spec
      (fun str -> !config.anon_fun str)
      "" ;
    !config
  with
  | Arg.Bad err ->
      Format.(pp_print_string err_formatter) err ;
      exit 2
  | Arg.Help _ ->
      Format.(pp_print_string err_formatter)
        (Arg.usage_string !(!config.spec) (!config.path ^ " " ^ !config.usage)) ;
      exit 0

module Make_basic (Intf : Snark_intf.Run_basic with type prover_state = unit) =
struct
  open Intf
  open Bin_prot_io

  let main proof_system read_input =
    let config = parse () in
    ( match config.mode with
    | Some (Keys conf) ->
        let keypair = Proof_system.generate_keypair proof_system in
        let pk_path = Option.value ~default:(path ^ ".pk") !conf.pk in
        write (module Proving_key) pk_path ~data:(Keypair.pk keypair) ;
        let vk_path = Option.value ~default:(path ^ ".vk") !conf.vk in
        write (module Verification_key) vk_path ~data:(Keypair.vk keypair)
    | Some (Prove conf) ->
        let public_input =
          read_input (Option.value ~default:"" !conf.public_input)
        in
        let pk_path = Option.value ~default:(path ^ ".pk") !conf.pk in
        let proving_key = read (module Proving_key) pk_path in
        let proof =
          Proof_system.prove ~public_input ~proving_key proof_system ()
        in
        let output_path =
          Option.value ~default:(path ^ ".zkp") !conf.output_path
        in
        write (module Proof) output_path ~data:proof
    | Some (Verify conf) ->
        let public_input =
          read_input (Option.value ~default:"" !conf.public_input)
        in
        let vk_path = Option.value ~default:(path ^ ".vk") !conf.vk in
        let verification_key = read (module Verification_key) vk_path in
        let filename = Option.value ~default:(path ^ ".zkp") !conf.filename in
        let proof = read (module Proof) filename in
        if
          Proof_system.verify ~public_input ~verification_key proof_system
            proof
        then Format.(fprintf std_formatter "Proof is valid.@.")
        else (
          Format.(fprintf std_formatter "Proof is invalid.@.") ;
          exit 1 )
    | None ->
        Format.(pp_print_string err_formatter)
          (Arg.usage_string Commands.Toplevel.spec
             ( Filename.basename Sys.executable_name
             ^ " " ^ Commands.Toplevel.usage )) ;
        exit 2 ) ;
    exit 0
end

module type Toplevel = sig
  val main : unit -> unit
end

module Make
    (Intf : Snark_intf.Run_basic with type prover_state = unit) (M : sig
        type result

        type computation

        type public_input

        val compute : computation

        val public_input :
          (unit -> result, unit, computation, public_input) Intf.Data_spec.t

        val read_input : string -> (unit, public_input) H_list.t
    end) =
struct
  open Intf
  module Basic = Make_basic (Intf)

  let main () =
    let proof_system =
      Proof_system.create ~public_input:M.public_input M.compute
    in
    Basic.main proof_system M.read_input
end

let%test_unit "toplevel_functor" =
  let (module M : Toplevel) =
    ( module struct
      module Intf =
        Snark.Run.Make
          (Backends.Bn128.Default)
          (struct
            type t = unit
          end)

      include Make
                (Intf)
                (struct
                  open Intf

                  type result = unit

                  type computation = Field.t -> Field.t -> unit -> unit

                  type public_input =
                    Field.Constant.t -> Field.Constant.t -> unit

                  let compute x y () =
                    let open Field in
                    let z = (x + y) * (x - y) in
                    let x2 = x * x in
                    let y2 = y * y in
                    Field.Assert.equal z (x2 - y2)

                  let public_input = Data_spec.[Field.typ; Field.typ]

                  let read_input str =
                    let strs = String.split str ~on:' ' in
                    match strs with
                    | [x; y] ->
                        H_list.
                          [ Field.Constant.of_string x
                          ; Field.Constant.of_string y ]
                    | _ ->
                        failwith "Bad input. Expected 2 field elements."
                end)
    end )
  in
  ()

module Make_json
    (Intf : Snark_intf.Run_basic with type prover_state = unit) (M : sig
        type arg0

        type computation0

        type computation = arg0 -> computation0

        type public_input

        val public_input :
          (unit -> unit, unit, computation, public_input) Intf.Data_spec.t

        val read_input : Yojson.Safe.t -> (unit, public_input) H_list.t

        module Witness : sig
          type t

          module Constant : sig
            type t

            val of_yojson : Yojson.Safe.t -> (t, string) Result.t
          end

          val typ : (t, Constant.t) Intf.Typ.t
        end

        val main : Witness.t -> computation
    end) =
struct
  open Intf

  let bad_object ~backtrace:bt str =
    `Assoc
      [ ("name", `String "error")
      ; ("message", `String str)
      ; ("backtrace", `String bt) ]

  let print_key fmt str = Format.(fprintf fmt "'%s'" str)

  let report_error ~json ~full =
    Format.(fprintf err_formatter "Could not interpret JSON message: %s@.")
      full ;
    failwith json

  let report_no_key keys =
    let open Format in
    let full =
      fprintf str_formatter "there is no key matching any of %a"
        (pp_print_list
           ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
           print_key)
        keys ;
      flush_str_formatter ()
    in
    let json = sprintf "Missing key '%s'" (List.hd_exn keys) in
    report_error ~full ~json

  let find_key ~keys l =
    List.find_map l ~f:(fun (key, value) ->
        if List.mem ~equal:String.equal keys key then Some value else None )

  let main () =
    (* We're communicating over stdout, don't log to it! *)
    Libsnark.set_printing_off () ;
    let module W = struct
      type _ Request.t += Witness : M.Witness.Constant.t Request.t
    end in
    let main =
      (* TODO: Really big hack, kill this ASAP. *)
      Intf.Internal_Basic.conv_never_use
        (fun () -> Intf.exists ~request:(fun () -> W.Witness) M.Witness.typ)
        M.public_input M.main
    in
    let proof_system =
      Proof_system.create ~proving_key_path:"proving_key.pk"
        ~verification_key_path:"verification_key.vk"
        ~public_input:M.public_input main
    in
    let public_input_keys = ["statement"; "public_input"; "data"] in
    let prover_state_keys =
      ["witness"; "prover_state"; "auxiliary_input"; "auxiliary"]
    in
    let proof_keys = ["proof"] in
    Stream.iter
      (fun json ->
        try
          let l =
            match json with
            | `Assoc l ->
                l
            | _ ->
                report_error ~full:"expected an object." ~json:"Not an object"
          in
          match List.Assoc.find ~equal:String.equal l "command" with
          | Some (`String "prove") -> (
              let public_input =
                match find_key ~keys:public_input_keys l with
                | Some public_input ->
                    M.read_input public_input
                | None ->
                    report_no_key public_input_keys
              in
              match
                M.Witness.Constant.of_yojson
                  (Option.value
                     (find_key ~keys:prover_state_keys l)
                     ~default:(`Assoc []))
              with
              | Error e ->
                  report_error ~json:"Could not parse witness."
                    ~full:(sprintf "Could not parse witness: %s" e)
              | Ok witness ->
                  let proof =
                    Proof_system.prove ~public_input proof_system ()
                      ~handlers:
                        [ (fun (With {request; respond}) ->
                            match request with
                            | W.Witness ->
                                respond (Provide witness)
                            | _ ->
                                failwith "Unhandled" ) ]
                  in
                  let proof_string =
                    let size = Proof.bin_size_t proof in
                    let buf = Bigstring.create size in
                    ignore (Proof.bin_write_t buf ~pos:0 proof) ;
                    Base64.encode_string (Bigstring.to_string buf)
                  in
                  Yojson.Safe.pretty_to_channel stdout
                    (`Assoc
                      [ ("name", `String "proof")
                      ; ("proof", `String proof_string) ]) )
          | Some (`String "verify") ->
              let public_input =
                match find_key ~keys:public_input_keys l with
                | Some public_input ->
                    M.read_input public_input
                | None ->
                    report_no_key public_input_keys
              in
              let proof =
                match find_key ~keys:proof_keys l with
                | Some (`String str) ->
                    let buf = Bigstring.of_string (Base64.decode_exn str) in
                    Proof.bin_read_t buf ~pos_ref:(ref 0)
                | Some _ ->
                    report_error
                      ~full:"expected value for key 'proof' to be a string."
                      ~json:"Bad proof value"
                | None ->
                    report_no_key public_input_keys
              in
              let verified =
                Proof_system.verify ~public_input proof_system proof
              in
              Yojson.Safe.pretty_to_channel stdout
                (`Assoc
                  [("name", `String "verified"); ("verified", `Bool verified)])
          | Some (`String "generate_keys") | Some (`String "generate-keys") ->
              ignore (Proof_system.generate_keypair proof_system) ;
              Yojson.Safe.pretty_to_channel stdout
                (`Assoc
                  [ ("name", `String "keys_generated")
                  ; ("proving_key_path", `String "proving_key.pk")
                  ; ("verification_key_path", `String "verification_key.vk") ])
          | Some _ ->
              report_error ~full:"unknown command." ~json:"Unknown command"
          | None ->
              report_error ~full:"there is no key 'command' in the object."
                ~json:"Missing key 'command'"
        with
        | Failure str ->
            let backtrace = Printexc.get_backtrace () in
            Yojson.Safe.pretty_to_channel stdout (bad_object ~backtrace str)
        | Yojson.Json_error str ->
            let backtrace = Printexc.get_backtrace () in
            Format.(fprintf err_formatter "JSON error: %s@." str) ;
            Yojson.Safe.pretty_to_channel stdout
              (bad_object ~backtrace (sprintf "Parse error: %s" str))
        | exn ->
            let backtrace = Printexc.get_backtrace () in
            let exn = Exn.to_string exn in
            Format.(fprintf err_formatter "Unknown error:@.%s@." exn) ;
            Yojson.Safe.pretty_to_channel stdout
              (bad_object ~backtrace (sprintf "Unknown error:\n%s" exn)) )
      (Yojson.Safe.stream_from_channel In_channel.stdin)
end
