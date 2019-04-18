module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type t = ..

type t += A

type t += B

let (a : t) = A

let (b : t) = B

module T = struct
  type t = ..

  type t += C
end

let (c : T.t) = T.C

type T.t += D

let (d : T.t) = D

type ('a, 'b) u = ..

type ('a, 'b) u += F of 'a | E of 'a * 'b

let (e : (int, bool) u) = E (1, true)

let (f : (int, int) u) = E (1, 1)

let (g : (unit, bool) u) = F ()
