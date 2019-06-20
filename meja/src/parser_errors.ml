type error =
  | Fun_no_fat_arrow
  | Missing_semi
  | Unexpected_character of string
  | Expecting of string
  | Dangling_handler

exception Error of Location.t * error

open Format

let report_error ppf = function
  | Fun_no_fat_arrow ->
      fprintf ppf "Expected => before {@."
  | Missing_semi ->
      (* TODO: make other errors not emit this. *)
      fprintf ppf "Syntax error"
  | Unexpected_character x ->
      fprintf ppf "Unexpected character '%s'" x
  | Expecting desc ->
      fprintf ppf "Syntax error: %s expected" desc
  | Dangling_handler ->
      fprintf ppf "A handler expression cannot finish a block."

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
    | _ ->
        None )
