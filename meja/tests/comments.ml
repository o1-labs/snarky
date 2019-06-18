open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let x =
  ( Field.constant (Field.Constant.of_string "1")
  , Field.constant (Field.Constant.of_string "2")
  , Field.constant (Field.Constant.of_string "7") )

let y =
  let i, j, k = x in
  (i, j)

let f = 1

let g = f + 2

let h = ()

let i = ()
