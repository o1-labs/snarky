module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let (f : ('a -> 'a) -> 'a -> 'a) =
  fun (type t) (x : t -> t) (y : t) -> (x y : t)

let (g : ('a -> 'a) -> 'a -> 'a) =
  fun (type u) (x : u -> u) (y : u) -> (x y : u)

type (_, _) t =
  | A : (unit, unit) t
  | B : 'a * (_, 'rest) t -> ('a, 'a -> 'rest) t

let h (type rest) (x : ('a, rest) t) : rest =
  match x with A -> () | B (x, _) -> x
