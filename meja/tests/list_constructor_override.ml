module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type nil

type 'a t = [] : nil t | ( :: ) : 'hd * 'tl t -> ('hd -> 'tl) t

let x = []

let y = [12]

let z = [1; Boolean.true_; ()]

let z = [1; Boolean.true_; ()]

module A = struct
  type ('a, 'b) u = [] of 'a * 'b

  let x = []
end
