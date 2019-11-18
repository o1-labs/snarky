open Core_kernel
open Ast_types

(** Paths formed from unique identifiers. *)
type t = Pident of Ident.t | Pdot of t * mode * string | Papply of t * t
[@@deriving sexp, compare]

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
      fprintf ppf "(%a).%s/%a" debug_print path name mode_debug_print mode
  | Papply (path1, path2) ->
      fprintf ppf "(%a)(%a)" debug_print path1 debug_print path2

let dot (path : t) (ident : Ident.t) =
  Pdot (path, Ident.mode ident, Ident.name ident)

(** Create a path from a list of [Ident.t]s. *)
let of_idents idents =
  match idents with
  | [] ->
      failwith "Path.of_idents: Empty list."
  | ident :: idents ->
      List.fold ~f:dot ~init:(Pident ident) idents

(** Create a new path by prefixing the path with [name]. *)
let rec add_outer_module name path =
  match path with
  | Pident name2 ->
      Pdot (Pident name, Ident.mode name2, Ident.name name2)
  | Pdot (path, mode, name2) ->
      Pdot (add_outer_module name path, mode, name2)
  | Papply _ ->
      failwith "Path.add_outer_module: Unhandled Papply."

include Comparable.Make (struct
  type nonrec t = t

  let compare = compare

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

let rec to_longident = function
  | Pident name ->
      Longident.Lident (Ident.name name)
  | Pdot (path, _, name) ->
      Longident.Ldot (to_longident path, name)
  | Papply (path1, path2) ->
      Longident.Lapply (to_longident path1, to_longident path2)

let mode = function
  | Pident name ->
      Ident.mode name
  | Pdot (_path, mode, _name) ->
      mode
  | Papply _ ->
      failwith "Path.mode: Unhandled Papply."
