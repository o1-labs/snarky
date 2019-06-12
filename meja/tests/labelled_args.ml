module Impl = Snarky.Snark.Make (Snarky.Backends.Mnt4.Default)
open Impl

let a ~a = a

let b ~(a : int) = a + a

let c ?a = match a with Some a -> a | None -> false

let d ~a ?(b : bool) = match b with Some a -> a | None -> a

let e () =
  let x1 =
    a
      ~a:
        [ Field.constant (Field.Constant.of_string "1")
        ; Field.constant (Field.Constant.of_string "2")
        ; Field.constant (Field.Constant.of_string "3") ]
  in
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

let (f : a:int -> ?b:bool -> int -> int) =
 fun ~a ?b c -> match b with Some true -> a | Some false -> c | None -> a + c

let (g : a:int -> int -> int) = f ~b:true

let (h : a:int -> ?b:bool -> int) = f 15

let (i : ?b:bool -> int -> int) = f ~a:15

let (j : unit -> int) = fun () -> f 20 ~a:15

let (j : a:int -> b:int option -> int) =
 fun ~a ~b -> match b with Some a -> a | None -> a

let x = 0

let (k : int option -> int) = fun __implicit2__ -> j ~a:x ~b:__implicit2__

let y = Some 0

let (l : int) = j ~a:x ~b:y
