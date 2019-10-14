module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module type T = sig
  type ('a, 'b) t = U of ('a, 'b) u | A of 'a

  and ('a, 'b) u = T of ('a, 'b) t | B of 'b
end
