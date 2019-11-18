module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module A = struct
  let x = 15

  let y = 20

  include struct
    let a = true

    let b = false
  end
end

let z = A.x + A.y

include struct
  let z = A.a && A.b
end

let (z : int) = if A.a && A.b then A.x else A.y
