module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module X = struct
  let x = 15
end

let f (x : int) = x

let a = f X.x

let y = 15

let b = f y

;;
()

let c = f X.x
