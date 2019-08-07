open Core_kernel

let pp_name ppf name =
  let c = name.[0] in
  if
    (Char.compare c 'a' >= 0 && Char.compare c 'z' <= 0)
    || (Char.compare c 'A' >= 0 && Char.compare c 'z' <= 0)
    || Char.equal c '_' || String.equal name "()"
  then Format.pp_print_string ppf name
  else Format.fprintf ppf "(%s)" name

module Longident = struct
  type t = (Longident.t[@sexp.opaque]) =
    | Lident of string
    | Ldot of t * string
    | Lapply of t * t
  [@@deriving sexp]

  include (Longident : module type of Longident with type t := t)

  let rec compare lid1 lid2 =
    let nonzero_or x f = if Int.equal x 0 then f () else x in
    match (lid1, lid2) with
    | Lident name1, Lident name2 ->
        String.compare name1 name2
    | Ldot (lid1, name1), Ldot (lid2, name2) ->
        nonzero_or (String.compare name1 name2) (fun () -> compare lid1 lid2)
    | Lapply (lid1a, lid1b), Lapply (lid2a, lid2b) ->
        nonzero_or (compare lid1a lid2a) (fun () -> compare lid1b lid2b)
    | Lident _, _ ->
        -1
    | _, Lident _ ->
        1
    | Ldot _, _ ->
        -1
    | _, Ldot _ ->
        1

  let rec sexp_of_t lid =
    match lid with
    | Lident name ->
        Sexp.Atom name
    | Ldot (lid, name) ->
        Sexp.List [sexp_of_t lid; Atom name]
    | Lapply (lid1, lid2) ->
        Sexp.List [sexp_of_t lid1; sexp_of_t lid2]

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

  let rec join lid1 lid2 =
    match lid2 with
    | Lident name ->
        Ldot (lid1, name)
    | Ldot (lid2, name) ->
        Ldot (join lid1 lid2, name)
    | Lapply (lid2, lid_apply) ->
        Lapply (join lid1 lid2, lid_apply)

  let join_name lid name =
    match lid with Some lid -> Ldot (lid, name) | None -> Lident name

  let join_path lid1 lid2 =
    match lid1 with Some lid1 -> join lid1 lid2 | None -> lid2
end

type arg_label = Asttypes.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
[@@deriving sexp]

type str = string Location.loc

type lid = Longident.t Location.loc

type explicitness = Implicit | Explicit [@@deriving sexp]

let map_loc x ~f = Location.mkloc (f x.Location.txt) x.loc

let mk_lid (str : str) = map_loc str ~f:(fun x -> Longident.Lident x)

type mode = Checked | Prover [@@deriving sexp, compare]

let string_of_mode = function Checked -> "Checked" | Prover -> "Prover"

let pp_mode ppf mode = Format.pp_print_string ppf (string_of_mode mode)

let modes_of_mode = function
  | Checked -> (
      function Checked -> true | Prover -> false )
  | Prover -> (
      function Checked -> true | Prover -> true )

let weakest_mode mode1 mode2 =
  match (mode1, mode2) with
  | Checked, _ | _, Checked ->
      Checked
  | Prover, Prover ->
      Prover
