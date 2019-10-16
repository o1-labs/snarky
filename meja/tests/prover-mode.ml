module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module A = struct
  let x = 15

  let y = 20

  include struct
    let x = true

    let y = false
  end
end

let z = A.x + A.y

include struct
  let z = A.x && A.y
end

let (z : int) = if A.x && A.y then A.x else A.y
