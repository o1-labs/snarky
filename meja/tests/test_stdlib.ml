open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let x (x : Field.t) : Field.t =
  let one = Field.constant (Field.Constant.of_string "1") in
  let two = Field.constant (Field.Constant.of_string "2") in
  let three = Field.constant (Field.Constant.of_string "3") in
  Field.mul one (Field.mul two (Field.mul three x))

open Field

let y (x : Field.t) : Field.t =
  let a = Field.constant (Field.Constant.of_string "15") in
  let b = Field.constant (Field.Constant.of_string "20") in
  let c = Field.constant (Field.Constant.of_string "25") in
  a * (b + c) * x

open Boolean

let z (a : Boolean.var) (b : Boolean.var) : Boolean.var =
  (Boolean.false_ || Boolean.true_) && (a || b)
