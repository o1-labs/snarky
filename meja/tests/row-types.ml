module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type t = ..

type t += C | B | A

type u = unit

type 'a v = unit

type ('a, 'b, 'c) w = unit
