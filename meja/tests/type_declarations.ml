type t = int

type u = int * bool

type 'a v = 'a

type _ w = A of int | B : 'a -> 'a w

type 'a x = {a: 'a; b: int; c: bool}

type y = unit x

type z = bool x

type a = t x w x x x

type ('a, 'b, 'c) b =
  | First : 'a -> ('a, 'a, 'a) b
  | Second : 'b * ('a, _, _) b -> ('a, 'b, 'b) b
  | Third : 'c * ('a, 'b, _) b -> ('a, 'b, 'c) b
  | Rotate : ('a, 'b, 'c) b -> ('b, 'c, 'a) b

type unit = ()

type polycary = A : 'a -> polycary

type 'a c = A of {x: 'a} | B of 'a | C : {x: 'b} -> 'b c

type d
