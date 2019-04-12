module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let x = 15

let y = x

let z = x

let a = 15

let b =
  let y = x in
  y

let c (ignore : int -> unit) = ignore x ; y

let d x = x

let e =
  let e =
    let e = x in
    e
  in
  e

let f =
  let f x = x in
  let g = d f f in
  (g f f) f

let (g : _ -> _) =
  let (f : _ -> _) = fun x -> x in
  f

let h (ignore : int -> unit) =
  let a = 1 in
  let b = 2 in
  ignore a ;
  ignore b ;
  let c = 3 in
  let d = 4 in
  ignore c ;
  ignore d ;
  let e = 5 in
  ignore e ;
  let f x y = x in
  f a (f b (f c (f d e)))

let i () = ()

let j = i ()
