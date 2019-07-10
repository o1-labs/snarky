module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module A = struct
  type t = int
end

let (x : A.t) = 15

open A

let (y : t) = 20
