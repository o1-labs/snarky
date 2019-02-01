let a =
  let x = 1 in
  let f x = x in
  f x

let b (f : int -> '_a27) =
  let (i : '_a27) = f 15 in
  let (j : '_a27) = f i in
  j

let c =
  let (f : '_a49 -> '_a49) = fun (x : '_a49) -> x in
  f f f

let d (f : int -> int) (x : int) = f 15 ; f x
