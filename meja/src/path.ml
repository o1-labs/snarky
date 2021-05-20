open Core_kernel
open Ast_types

(** Paths formed from unique identifiers. *)
type t =
  | Pident of Ident.t
  | Pdot of t * mode * string
  | Pocamldot of t * mode * string * string ref
  | Papply of t * t
[@@deriving sexp, compare]

(** Pretty print. Identifiers that do not begin with a letter or underscore
    will be surrounded by parentheses.
*)
let rec pp ppf path =
  let open Format in
  match path with
  | Pident name ->
      Ident.pprint ppf name
  | Pdot (path, _mode, name) | Pocamldot (path, _mode, name, _) ->
      fprintf ppf "%a.%s" pp path name
  | Papply (path1, path2) ->
      fprintf ppf "%a(%a)" pp path1 pp path2

(** Debug print. Prints the path with its internal modes and IDs. *)
let rec debug_print ppf path =
  let open Format in
  match path with
  | Pident name ->
      Ident.debug_print ppf name
  | Pocamldot (path, mode, name, ocamlname)
    when not (String.equal name !ocamlname) ->
      fprintf ppf "(%a).(%s=%s)/%a" debug_print path name !ocamlname
        mode_debug_print mode
  | Pdot (path, mode, name) | Pocamldot (path, mode, name, _) ->
      fprintf ppf "(%a).%s/%a" debug_print path name mode_debug_print mode
  | Papply (path1, path2) ->
      fprintf ppf "(%a)(%a)" debug_print path1 debug_print path2

let dot (path : t) (ident : Ident.t) =
  match Ident.ocaml_name_ref ident with
  | Some ocaml_name ->
      Pocamldot (path, Ident.mode ident, Ident.name ident, ocaml_name)
  | None ->
      Pdot (path, Ident.mode ident, Ident.name ident)

(** Deconstruct a path to an [Ident.t] and a list of (mode, name, ocaml name)
    triples, from innermost to outermost.

    For example,
    [deconstruct(A.B.C.d) = (A, [(_, "B", _); (_, "C", _); (_, "d", _)])].
*)
let deconstruct path =
  let rec go l = function
    | Pident ident ->
        (ident, l)
    | Pdot (path, mode, name) ->
        go ((mode, name, None) :: l) path
    | Pocamldot (path, mode, name, ocaml_name) ->
        go ((mode, name, Some ocaml_name) :: l) path
    | Papply _ ->
        failwith "Path.deconstruct: Unhandled Papply."
  in
  go [] path

let pdot (root_path : t) (path : t) =
  let ident, mode_names = deconstruct path in
  List.fold ~init:(dot root_path ident) mode_names
    ~f:(fun path (mode, name, ocaml_name) ->
      match ocaml_name with
      | None ->
          Pdot (path, mode, name)
      | Some ocaml_name ->
          Pocamldot (path, mode, name, ocaml_name) )

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
  | Pident name2 -> (
    match Ident.ocaml_name_ref name2 with
    | None ->
        Pdot (Pident name, Ident.mode name2, Ident.name name2)
    | Some ocaml_name ->
        Pocamldot (Pident name, Ident.mode name2, Ident.name name2, ocaml_name)
    )
  | Pdot (path, mode, name2) ->
      Pdot (add_outer_module name path, mode, name2)
  | Pocamldot (path, mode, name2, ocaml_name) ->
      Pocamldot (add_outer_module name path, mode, name2, ocaml_name)
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
  | Pdot (path, _, name) | Pocamldot (path, _, name, _) ->
      Longident.Ldot (to_longident path, name)
  | Papply (path1, path2) ->
      Longident.Lapply (to_longident path1, to_longident path2)

let rec to_ocaml_longident = function
  | Pident name ->
      Longident.Lident (Ident.ocaml_name name)
  | Pocamldot (path, _, _, name) ->
      Longident.Ldot (to_ocaml_longident path, !name)
  | Pdot (path, _, name) ->
      Longident.Ldot (to_ocaml_longident path, name)
  | Papply (path1, path2) ->
      Longident.Lapply (to_ocaml_longident path1, to_ocaml_longident path2)

let mode = function
  | Pident name ->
      Ident.mode name
  | Pdot (_path, mode, _name) | Pocamldot (_path, mode, _name, _) ->
      mode
  | Papply _ ->
      failwith "Path.mode: Unhandled Papply."
