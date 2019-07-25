open Core_kernel

type t = Pident of Ident.t | Pdot of t * string | Papply of t * t
[@@deriving sexp]

let rec pp ppf lid =
  let open Format in
  match lid with
  | Pident name ->
      Ident.pprint ppf name
  | Pdot (path, name) ->
      fprintf ppf "%a.%s" pp path name
  | Papply (path1, path2) ->
      fprintf ppf "%a(%a)" pp path1 pp path2

let rec add_outer_module name path =
  match path with
  | Pident name2 ->
      Pdot (Pident name, Ident.name name2)
  | Pdot (path, name2) ->
      Pdot (add_outer_module name path, name2)
  | Papply _ ->
      failwith "Unhandled Papply in add_outer_module"
