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
    type config =
      { curve: string option
      ; pk: string option
      ; vk: string option
      ; filename: string option }

    let empty_config = {curve= None; pk= None; vk= None; filename= None}

    let name = "generate-keys"

    let description = "generate a keypair"

    let usage = "[options..] filename.zk"

    let spec config =
      let pk pk = config := {!config with pk= Some pk} in
      let vk vk = config := {!config with vk= Some vk} in
      [ ( "--curve"
        , Arg.String
            (fun curve ->
              match curve with
              | "Mnt4" | "Mnt4753" | "Mnt6" | "Mnt6753" | "Bn128" ->
                  config := {!config with curve= Some curve}
              | _ ->
                  raise (Arg.Bad (Printf.sprintf "Invalid curve %s" curve)) )
        , "select the curve to prove over. One of Mnt4, Mnt4753, Mnt6, \
           Mnt6753, BN128." )
      ; ( "--proving-key"
        , Arg.String pk
        , "specify a filename for the proving key. Default is filename.pk" )
      ; ( "--verification-key"
        , Arg.String vk
        , "specify a filename for the verification key. Default is filename.vk"
        )
      ; ("-pk", Arg.String pk, "alias of --proving-key")
      ; ("-vk", Arg.String vk, "alias of --verification-key") ]

    let anon_fun config filename =
      match !config.filename with
      | Some _ ->
          raise
            (Arg.Bad
               (Printf.sprintf
                  "Invalid argument %s: another filename was not expected"
                  filename))
      | None ->
          config := {!config with filename= Some filename}
  end

  module Prove = struct
    type config =
      { filename: string option
      ; public_input: string option
      ; witness_path: string option
      ; witness_data: string option
      ; output_path: string option
      ; pk: string option }

    let empty_config =
      { filename= None
      ; public_input= None
      ; witness_path= None
      ; witness_data= None
      ; output_path= None
      ; pk= None }

    let name = "prove"

    let description = "generate a zkSNARK proof"

    let usage =
      "prove [--witness-path path | --witness-data data] [options..] \
       filename.zk PUBLIC_INPUT"

    let spec config =
      let pk pk = config := {!config with pk= Some pk} in
      let out out = config := {!config with output_path= Some out} in
      [ ( "--witness-path"
        , Arg.String
            (fun witness_path ->
              config :=
                { !config with
                  witness_path= Some witness_path
                ; witness_data= None } )
        , "specify a filename to read the witness data from" )
      ; ( "--witness-data"
        , Arg.String
            (fun witness_data ->
              config :=
                { !config with
                  witness_data= Some witness_data
                ; witness_path= None } )
        , "specify the witness data directly" )
      ; ( "--proving-key"
        , Arg.String pk
        , "specify a filename for the proving key. Default is filename.pk" )
      ; ("-pk", Arg.String pk, "alias of --proving-key")
      ; ( "--output"
        , Arg.String out
        , "specify a filename for the proof. Default is filename.zkp" )
      ; ("-o", Arg.String out, "alias of --output") ]

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
        , "specify a filename for the verification key. Default is filename.vk"
        )
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
