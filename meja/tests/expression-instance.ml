module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let f (x : int) = x

let g () =
  let y = 15 in
  f y

let a (f : int -> 'a -> 'a) x = f 12 x

let h _ x = x

let b () =
  (let f _ x = x in
   let g i j = i + j in
   ignore (a g 1) ;
   ignore (a f true)) ;
  ignore (a h 1) ;
  ignore (a h true)
