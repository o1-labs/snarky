open Core_kernel
open Ast_types

(** Paths formed from unique identifiers. *)
type t = Pident of Ident.t | Pdot of t * mode * string | Papply of t * t
[@@deriving sexp]

(** Pretty print. Identifiers that do not begin with a letter or underscore
    will be surrounded by parentheses.
*)
let rec pp ppf path =
  let open Format in
  match path with
  | Pident name ->
      Ident.pprint ppf name
  | Pdot (path, _mode, name) ->
      fprintf ppf "%a.%s" pp path name
  | Papply (path1, path2) ->
      fprintf ppf "%a(%a)" pp path1 pp path2

(** Debug print. Prints the path with its internal modes and IDs. *)
let rec debug_print ppf path =
  let open Format in
  match path with
  | Pident name ->
      Ident.debug_print ppf name
  | Pdot (path, mode, name) ->
      fprintf ppf "(%a).%s/%a" debug_print path name pp_mode mode
  | Papply (path1, path2) ->
      fprintf ppf "(%a)(%a)" debug_print path1 debug_print path2

(** Create a new path by prefixing the path with [name]. *)
let rec add_outer_module name path =
  match path with
  | Pident name2 ->
      Pdot (Pident name, Ident.mode name2, Ident.name name2)
  | Pdot (path, mode, name2) ->
      Pdot (add_outer_module name path, mode, name2)
  | Papply _ ->
      failwith "Unhandled Papply in add_outer_module"

let dot (path : t) (ident : Ident.t) =
  Pdot (path, Ident.mode ident, Ident.name ident)

(** Compare two paths. This can be 0 only when the two values' [Ident.t]
    children were created in the same call to [Ident.create].
*)
let rec compare lid1 lid2 =
  let nonzero_or x f = if Int.equal x 0 then f () else x in
  match (lid1, lid2) with
  | Pident name1, Pident name2 ->
      Ident.compare name1 name2
  | Pdot (lid1, mode1, name1), Pdot (lid2, mode2, name2) ->
      nonzero_or (String.compare name1 name2) (fun () ->
          nonzero_or (compare lid1 lid2) (fun () -> compare_mode mode1 mode2)
      )
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

let rec to_longident = function
  | Pident name ->
      Longident.Lident (Ident.name name)
  | Pdot (path, _, name) ->
      Longident.Ldot (to_longident path, name)
  | Papply (path1, path2) ->
      Longident.Lapply (to_longident path1, to_longident path2)
