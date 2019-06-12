module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let ( + ) = Field.constant (Field.Constant.of_string "15")

let ( - ) = Field.constant (Field.Constant.of_string "20")

let ( ! ) _ = Field.constant (Field.Constant.of_string "30")

let ( ~- ) _ = Field.constant (Field.Constant.of_string "80")

let ( || ) x y = match x with true -> x | false -> y

let a = true || false

let b = true || false

let c = !a

let d = -Field.constant (Field.Constant.of_string "35")

let e = -d

let (f : (int -> int -> int) -> int) =
 fun (check : 'a -> 'a -> 'a) ->
  let ( + ) l _ = l in
  let ( - ) _ r = r in
  let ( * ) _ r = r in
  let ( / ) l _ = l in
  let x = (((() * true) - 1) / ((), ())) + (1, 1) in
  let y = (() * true) - (1 / ((), ())) + (1, 1) in
  check x y
