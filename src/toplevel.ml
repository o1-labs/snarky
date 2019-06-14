open Core_kernel

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

    let empty_config =
      { public_input= None
      ; output_path= None
      ; pk= None }

    let name = "prove"

    let description = "generate a zkSNARK proof"

    let usage =
      "prove [options..] \
       PUBLIC_INPUT"

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
  open Bin_prot_io

  let main () =
    let config = parse () in
    let proof_system =
      Proof_system.create ~public_input:M.public_input M.compute
    in
    ( match config.mode with
    | Some (Keys conf) ->
        let keypair = Proof_system.generate_keypair proof_system in
        let pk_path = Option.value ~default:(path ^ ".pk") !conf.pk in
        write (module Proving_key) pk_path ~data:(Keypair.pk keypair) ;
        let vk_path = Option.value ~default:(path ^ ".vk") !conf.vk in
        write (module Verification_key) vk_path ~data:(Keypair.vk keypair)
    | Some (Prove conf) ->
        let public_input =
          M.read_input (Option.value ~default:"" !conf.public_input)
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
          M.read_input (Option.value ~default:"" !conf.public_input)
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

let%test_unit "toplevel_functor" =
  let (module M : Toplevel) = (module struct
    module Intf = Snark.Run.Make (Backends.Bn128.Default) (struct type t = unit end)
    include Make (Intf) (struct
      open Intf
      type result = unit

      type computation = Field.t -> Field.t -> unit -> unit

      type public_input = Field.Constant.t -> Field.Constant.t -> unit

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
            H_list.[Field.Constant.of_string x; Field.Constant.of_string y]
        | _ ->
            failwith "Bad input. Expected 2 field elements."
    end)
  end) in
  ()
