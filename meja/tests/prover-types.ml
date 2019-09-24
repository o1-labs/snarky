module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type t

include struct
  type t1

  type t2
end

let f (f : t -> t1 -> t2) x y = f x y

include struct
  let f (f : t -> t1 -> t2) x y = f x y
end
