open Core_kernel

let pp_name ppf name =
  let c = name.[0] in
  if
    (Char.compare c 'a' >= 0 && Char.compare c 'z' <= 0)
    || (Char.compare c 'A' >= 0 && Char.compare c 'z' <= 0)
    || Char.equal c '_'
  then Format.pp_print_string ppf name
  else Format.fprintf ppf "(%s)" name

module Lexing = struct
  type position = Lexing.position =
    {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}
  [@@deriving ord, sexp]

  include (Lexing : module type of Lexing with type position := position)
end

module Location = struct
  type t = Location.t =
    {loc_start: Lexing.position; loc_end: Lexing.position; loc_ghost: bool}
  [@@deriving ord, sexp]

  type 'a loc = 'a Location.loc = {txt: 'a; loc: t} [@@deriving ord, sexp]

  include (
    Location :
      module type of Location with type t := t and type 'a loc := 'a loc )
end

module Longident = struct
  type t = Longident.t =
    | Lident of string
    | Ldot of t * string
    | Lapply of t * t
  [@@deriving ord, sexp]

  include (Longident : module type of Longident with type t := t)

  include Comparator.Make (struct
    type nonrec t = t

    let compare = compare

    let sexp_of_t = sexp_of_t
  end)

  let rec pp ppf lid =
    let open Format in
    match lid with
    | Lident name ->
        pp_name ppf name
    | Ldot (lid, name) ->
        fprintf ppf "%a.%s" pp lid name
    | Lapply (lid1, lid2) ->
        fprintf ppf "%a(%a)" pp lid1 pp lid2

  let rec add_outer_module name lid =
    match lid with
    | Lident name2 ->
        Ldot (Lident name, name2)
    | Ldot (lid, name2) ->
        Ldot (add_outer_module name lid, name2)
    | Lapply _ ->
        failwith "Unhandled Lapply in add_outer_module"
end

type str = string Location.loc [@@deriving ord, sexp]

type lid = Longident.t Location.loc [@@deriving ord, sexp]

type explicitness = Implicit | Explicit [@@deriving ord, sexp]

type arg_label = Asttypes.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
[@@deriving ord, sexp]

let map_loc x ~f = Location.mkloc (f x.Location.txt) x.loc

let mk_lid (str : str) = map_loc str ~f:(fun x -> Longident.Lident x)
