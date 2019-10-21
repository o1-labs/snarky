module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type nonrec t

include struct
  type nonrec t1

  type nonrec t2
end

let f (f : t -> t1 -> t2) x y = f x y

include struct
  let f (f : t -> t1 -> t2) x y = f x y
end
