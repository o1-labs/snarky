module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let (f : ('a -> 'a) -> 'a -> 'a) =
  fun (type t) (x : t -> t) (y : t) -> (x y : t)

let (g : ('a -> 'a) -> 'a -> 'a) =
  fun (type u) (x : u -> u) (y : u) -> (x y : u)

let id x = x

let a = f id 1

let d = g id true
