let a =
  let x = 1 in
  let f x = x in
  f x

let b (f : int -> '_a28) =
  let (i : '_a28) = f 15 in
  let (j : '_a28) = f i in
  j

let c =
  let (f : '_a51 -> '_a51) = fun (x : '_a51) -> x in
  f f f

let d (f : int -> int) (x : int) = f 15 ; f x
