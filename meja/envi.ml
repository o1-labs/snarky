open Core_kernel
open Parsetypes

type 'a ready = Final of 'a | In_progress of 'a

type t = (string, type_expr ready, String.comparator_witness) Base.Map.t

let empty = Map.empty (module String)

let add_final {Location.txt= name; _} typ map =
  Map.update map name ~f:(fun _ -> Final typ)

let add_in_progress {Location.txt= name; _} typ map =
  Map.update map name ~f:(fun _ -> In_progress typ)
