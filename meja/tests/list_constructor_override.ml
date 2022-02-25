module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

type nonrec nil

type 'a t = [] : nil t | ( :: ) : 'hd * 'tl t -> ('hd -> 'tl) t

let x = []

let y = [ 12 ]

let z = [ 1; true; () ]

let z = [ 1; true; () ]

module A = struct
  type nonrec ('a, 'b) u = [] of 'a * 'b

  let x = []
end
