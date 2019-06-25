module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x = if true then true else false

let y = if true then ()

let z x y z = if x then y else if true then z else y

let a x y z = if x then y else if true then z else y

let b () = if true then () else if false then ()
