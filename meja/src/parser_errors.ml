open Meja_compiler_internals

type error =
  | Fun_no_fat_arrow
  | Missing_semi
  | Unexpected_character of string
  | Expecting of string

exception Error of Location.t * error

open Format

let report_error ppf = function
  | Fun_no_fat_arrow ->
      fprintf ppf "Expected => before {@."
  | Missing_semi ->
      fprintf ppf "Missing semicolon."
  | Unexpected_character x ->
      fprintf ppf "Unexpected character '%s'" x
  | Expecting desc ->
      fprintf ppf "Syntax error: %s expected" desc

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
    | _ ->
        None)
