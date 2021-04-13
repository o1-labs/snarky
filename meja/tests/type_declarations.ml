module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type nonrec t = int

type nonrec u = int * bool

type nonrec 'a v = 'a

type _ w = A of int | B : 'a -> 'a w

type nonrec 'a x = {a: 'a; b: int; c: bool}

type nonrec y = unit x

type nonrec z = bool x

type nonrec a = t x w x x x

type ('a, 'b, 'c) b =
  | First : 'a -> ('a, 'a, 'a) b
  | Second : 'b * ('a, _, _) b -> ('a, 'b, 'b) b
  | Third : 'c * ('a, 'b, _) b -> ('a, 'b, 'c) b
  | Rotate : ('a, 'b, 'c) b -> ('b, 'c, 'a) b

type nonrec unit = ()

type polycary = A : 'a -> polycary

type 'a c = A of {x: 'a} | B of 'a | C : {x: 'b} -> 'b c

type nonrec d
