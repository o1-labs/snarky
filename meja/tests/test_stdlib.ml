module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x (x : Field.t) : Field.t =
  let one = Field.constant (Field.Constant.of_string "1") in
  let two = Field.constant (Field.Constant.of_string "2") in
  let three = Field.constant (Field.Constant.of_string "3") in
  Field.mul one (Field.mul two (Field.mul three x))
