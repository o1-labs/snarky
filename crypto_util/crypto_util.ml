open Core

type curve = Bn128 | Mnt4 | Mnt6

let curve_of_string s =
  let s = String.lowercase s in
  List.find_map_exn [("bn128", Bn128); ("mnt4", Mnt4); ("mnt6", Mnt6)]
    ~f:(fun (m, mode) -> if String.equal s m then Some mode else None)

module type S = Pedersen.Inputs_intf

type curve_module = (module S)

let curve_module : curve -> curve_module = function
  | Bn128 ->
      ( module struct
        module Bn128 = Snarky.Snark.Make (Snarky.Backends.Bn128.Default)
        module Field = Bn128.Field

        module Curve =
          Affine.Make
            (Bn128.Field)
            (struct
              open Bn128

              let a =
                Field.of_string
                  "7296080957279758407415468581752425029516121466805344781232734728849116493472"

              (*         let b = Field.of_string "16213513238399463127589930181672055621146936592900766180517188641980520820846" *)
            end)
      end )
  | Mnt4 ->
      ( module struct
        module Mnt4 = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
        module Field = Mnt4.Field

        module Curve = struct
          let sexp_of_t = sexp_of_opaque

          let t_of_sexp = opaque_of_sexp

          include Snarky.Backends.Mnt6.G1
        end
      end )
  | Mnt6 ->
      ( module struct
        module Mnt6 = Snarky.Snark.Make (Snarky.Backends.Mnt6.Default)
        module Field = Mnt6.Field

        module Curve = struct
          let sexp_of_t = sexp_of_opaque

          let t_of_sexp = opaque_of_sexp

          include Snarky.Backends.Mnt4.G1
        end
      end )

let curve_to_string = function Bn128 -> "bn128" | _ -> assert false

let cmd =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map curve =
    flag "input"
      (optional_with_default Bn128 (Arg_type.create curve_of_string))
      ~doc:"Curve to use"
  and padded_length =
    flag "padded-length"
      (optional_with_default 256 int)
      ~doc:"Length in bits to pad input to"
  and params = flag "params" (optional string) ~doc:"Path to params file"
  and str = anon ("INPUT" %: string) in
  fun () ->
    let module Inputs = (val curve_module curve) in
    let open Inputs in
    let module P = Pedersen.Make (Inputs) in
    let params =
      Option.value
        ~default:(sprintf "%s-params" (curve_to_string Bn128))
        params
      |> P.Params.load
    in
    assert (padded_length >= String.length str) ;
    P.digest_fold (P.State.create params)
      Fold_lib.Fold.(
        group3 ~default:false
          ( string_bits str
          +> init (padded_length - (8 * String.length str)) ~f:(fun _ -> false)
          ))
    |> Field.to_string |> print_endline

let () = Command.run (Command.basic cmd ~summary:"Compute pedersen hashes")
