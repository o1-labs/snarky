let a =
  let x = 1 in
  let f x = x in
  f x

let b (f : int -> int) =
  let (i : int) = f 15 in
  let (j : int) = f i in
  j

let c =
  let (f : 'a -> '_a51) = fun (x : 'a -> 'a) -> x in
  f f f

let d (f : int -> int) (x : int) = f 15 ; f x
