module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let ( + ) = 15

let ( - ) = 20

let ( ! ) _ = 30

let ( ~- ) _ = 80

let ( || ) x y = match x with true -> x | false -> y

let a = true || false

let b = true || false

let c = !a

let d = -35

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
