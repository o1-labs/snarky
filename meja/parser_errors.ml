type error = Fun_no_fat_arrow | Missing_semi | Unexpected_character of string

exception Error of Location.t * error

open Format

let pp_typ ppf typ = Pprintast.core_type ppf (To_ocaml.of_type_expr typ)

let report_error ppf = function
  | Fun_no_fat_arrow -> fprintf ppf "Expected => before {@."
  | Missing_semi -> fprintf ppf "Missing semicolon."
  | Unexpected_character x -> fprintf ppf "Unexpected character '%s'" x

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None )
