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
[@@deriving sexp, equal, compare]

type closed_flag = Asttypes.closed_flag = Closed | Open
[@@deriving sexp, equal, compare]

let closed_flag_debug_print fmt = function
  | Closed ->
      Format.pp_print_string fmt "Cl"
  | Open ->
      Format.pp_print_string fmt "Op"

type str = string Location.loc

type lid = Longident.t Location.loc

type explicitness = Implicit | Explicit [@@deriving sexp, equal]

let map_loc x ~f = Location.mkloc (f x.Location.txt) x.loc

let mk_lid (str : str) = map_loc str ~f:(fun x -> Longident.Lident x)

(** Convert the OCaml primitive [__POS__] into a Lexing.position *)
let loc_of_prim (file, lnum, cnum, enum) =
  (* Note: We use a fake value for [pos_bol], since we can't get the true
             value from [__POS__]. *)
  { Location.loc_start=
      {Lexing.pos_fname= file; pos_lnum= lnum; pos_cnum= cnum; pos_bol= 0}
  ; loc_end=
      {Lexing.pos_fname= file; pos_lnum= lnum; pos_cnum= enum; pos_bol= 0}
  ; loc_ghost= false }

type mode = Checked | Prover [@@deriving sexp, equal, compare]

let other_mode = function Checked -> Prover | Prover -> Checked

let string_of_mode = function Checked -> "Checked" | Prover -> "Prover"

let pp_mode ppf mode = Format.pp_print_string ppf (string_of_mode mode)

let mode_debug_print ppf = function
  | Checked ->
      Format.pp_print_string ppf "C"
  | Prover ->
      Format.pp_print_string ppf "P"

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

type literal =
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Float of float
  | Bool of bool
  | Field of string
  | Char of char
  | String of string

(** Remove a minus sign if one is present, or add one if there is none.
    If the string is empty, raises an [Invalid_argument] exception.
*)
let neg_string s =
  match s.[0] with
  | '-' ->
      String.sub s ~pos:1 ~len:(String.length s - 1)
  | _ ->
      "-" ^ s

let neg_literal = function
  | Int i ->
      Int (-i)
  | Int32 i ->
      Int32 (Int32.neg i)
  | Int64 i ->
      Int64 (Int64.neg i)
  | Nativeint i ->
      Nativeint (Nativeint.neg i)
  | Float f ->
      Float (-.f)
  | Field f ->
      Field (neg_string f)
  | _ ->
      assert false
