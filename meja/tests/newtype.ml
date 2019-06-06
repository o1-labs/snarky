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
  match x with A -> (() : _) | B (x, _) -> x

type _ u = Int : int u | Bool : bool u

let destruct_with (type kind) (x : kind u) (y : kind) : kind =
  match (x, y) with Int, i -> i + 1 | Bool, b -> not b

let destruct_with2 (type kind) (x : kind u) (y : kind) : kind =
  match (y, x) with i, Int -> i + 1 | b, Bool -> not b
