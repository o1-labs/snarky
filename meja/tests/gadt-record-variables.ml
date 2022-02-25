module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type t = A : { x : 'a } -> t

let x = A { x = 15 }

let y = A { x = true }

type _ u = Int : int -> int u | Bool : bool -> bool u | Unit : unit u

type v = B : { x : 'a u } -> v

let a = B { x = Int 15 }

let b = B { x = Bool true }

let c = B { x = Unit }
