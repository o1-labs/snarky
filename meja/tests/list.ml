module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x =
  [ Field.constant (Field.Constant.of_string "1")
  ; Field.constant (Field.Constant.of_string "2")
  ; Field.constant (Field.Constant.of_string "3") ]

let y =
  [ ( Field.constant (Field.Constant.of_string "1")
    , Field.constant (Field.Constant.of_string "2")
    , Field.constant (Field.Constant.of_string "3") )
  ; ( Field.constant (Field.Constant.of_string "4")
    , Field.constant (Field.Constant.of_string "5")
    , Field.constant (Field.Constant.of_string "6") )
  ; ( Field.constant (Field.Constant.of_string "7")
    , Field.constant (Field.Constant.of_string "8")
    , Field.constant (Field.Constant.of_string "9") ) ]

let f (l : bool list) : bool * bool * bool =
  match l with
  | [] ->
      (false, false, false)
  | [a] ->
      (a, false, false)
  | [a; b] ->
      (a, b, false)
  | a :: b :: c :: _ ->
      (a, b, c)

let g = f [true; false; true]

let ( ! ) (x : bool) = match x with true -> false | false -> true

let ( + ) x y = match (x, y) with false, false -> false | _ -> true

let h = [!true; !false; true + !false]

let i = [true + false; !false]
