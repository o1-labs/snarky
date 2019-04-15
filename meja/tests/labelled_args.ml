module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let a ~a = a

let b ~(a : int) = a + a

let c ?a = match a with Some a -> a | None -> false

let d ~a ?(b : bool) = match b with Some a -> a | None -> a

let e () =
  let x1 = a ~a:[1; 2; 3] in
  let x2 = b ~a:2 in
  let a = 2 in
  let x3 = b ~a in
  let x4 = c ~a:true in
  let a = true in
  let x5 = c ~a in
  let x6 = c ?a:(Some a) in
  let x7 = d ~b:true ~a:true in
  let x8 = d ~a:true in
  (x1, x2, x3, x4, x5, x6, x7, x8)
