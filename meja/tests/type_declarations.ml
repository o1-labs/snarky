open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

include struct
  type t = int

  type var = int
end

include struct
  type u = int * bool

  type u_var = int * bool
end

include struct
  type 'a v = 'a

  type 'a v_var = 'a
end

type _ w = A of int | B : 'a -> 'a w

type 'a x = {a: 'a; b: int; c: bool}

include struct
  type y = unit x

  type y_var = unit x
end

include struct
  type z = bool x

  type z_var = bool x
end

include struct
  type a = t x w x x x

  type a_var = var x w x x x
end

type ('a, 'b, 'c) b =
  | First : 'a -> ('a, 'a, 'a) b
  | Second : 'b * ('a, _, _) b -> ('a, 'b, 'b) b
  | Third : 'c * ('a, 'b, _) b -> ('a, 'b, 'c) b
  | Rotate : ('a, 'b, 'c) b -> ('b, 'c, 'a) b

type unit = ()

type polycary = A : 'a -> polycary

type 'a c = A of {x: 'a} | B of 'a | C : {x: 'b} -> 'b c

type d
