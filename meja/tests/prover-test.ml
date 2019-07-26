module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

module type P = sig
  include
    sig
      val x : int

      val f : bool -> bool
  end
end

module P = struct
  include struct
    let x = 15

    let f b = b
  end
end

let f x = x

include struct
  let g x = f x
end

let h x = g x

let i () =
  let f x = x in
  let i = f 15 in
  i
