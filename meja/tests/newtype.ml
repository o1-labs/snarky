open Snarky
open Snarky.Snark
module Impl =
  Snarky.Snark.Run.Make (Snarky.Backends.Mnt4.Default) (Core_kernel.Unit)
open Impl

let (f : ('a -> 'a) -> 'a -> 'a) =
  fun (type t) (x : t -> t) (y : t) -> (x y : t)

let (g : ('a -> 'a) -> 'a -> 'a) =
  fun (type u) (x : u -> u) (y : u) -> (x y : u)
