module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let f ?x ?y ?z i =
  let i = match x with Some x -> i + x | None -> i in
  let i = match y with Some y -> i + y | None -> i in
  let i = match z with Some z -> i + z | None -> i in
  i

let _ = f 1

let _ = f ~x:1 1

let _ = f ~y:1 1

let _ = f ~z:1 1

let _ = f ~x:1 ~y:1 1

let _ = f ~y:1 ~z:1 1

let _ = f ~x:1 ~z:1 1

let _ = f ~x:1 ~y:1 ~z:1 1
