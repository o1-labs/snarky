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

let rec compare lid1 lid2 =
  let nonzero_or x f = if Int.equal x 0 then f () else x in
  match (lid1, lid2) with
  | Pident name1, Pident name2 ->
      Ident.compare name1 name2
  | Pdot (lid1, name1), Pdot (lid2, name2) ->
      nonzero_or (String.compare name1 name2) (fun () -> compare lid1 lid2)
  | Papply (lid1a, lid1b), Papply (lid2a, lid2b) ->
      nonzero_or (compare lid1a lid2a) (fun () -> compare lid1b lid2b)
  | Pident _, _ ->
      -1
  | _, Pident _ ->
      1
  | Pdot _, _ ->
      -1
  | _, Pdot _ ->
      1
