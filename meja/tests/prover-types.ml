module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type t

include struct
  type t

  type s
end

let f (f : t -> t -> s) x y = f x y
