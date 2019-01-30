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
  
let empty () =
  { names= Map.empty (module String)
  ; typ_vars= Map.empty (module String)
  ; vars_size= 0
  ; depth= 0 }

let add_name {Location.txt= name; _} typ env =
  {env with names= Map.update env.names name ~f:(fun _ -> typ)}

let find_name {Location.txt= name; _} {names; _} =
  Map.find names name
