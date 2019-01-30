open Core_kernel
open Parsetypes

type t =
  { names:
      (string, [`Copy | `NoCopy] * type_expr, String.comparator_witness) Map.t
  ; typ_vars:
      ( string
      , [`User | `Generated] * type_expr
      , String.comparator_witness )
      Map.t
  ; vars_size: int
  ; depth: int }

let pp (output : Format.formatter) (env : t) =
  Format.pp_print_string output "Names:\n";
  Map.iteri env.names ~f:(fun ~key ~data:(_, typ) ->
    Format.fprintf output "%s : %a\n" key pp_type_expr typ)

let pp_ocaml (output : Format.formatter) (env : t) =
  Format.pp_print_string output "Names:\n";
  Map.iteri env.names ~f:(fun ~key ~data:(_, typ) ->
    Format.fprintf output "%s : %a\n" key Pprintast.core_type (To_ocaml.of_typ typ))

let empty () =
  { names= Map.empty (module String)
  ; typ_vars= Map.empty (module String)
  ; vars_size= 0
  ; depth= 0 }

let add_name name typ env =
  {env with names= Map.update env.names name.txt ~f:(fun _ -> typ)}

let find_name name {names; _} = Map.find names name.txt

let add_type_var ~user ~name var env =
  { env with
    typ_vars=
      Map.update env.typ_vars name.txt ~f:(fun _ ->
          if user then (`User, var) else (`Generated, var) ) }

let find_type_var name {typ_vars; _} = Map.find typ_vars name.txt
