let (a : ('b -> 'b) -> 'b -> 'b) = fun (f : 'b -> 'b) -> f

let (b : '_a53) =
  let (f : '_a31 -> '_a31) = fun (x : '_a31) -> x in
  f f f

let (c : (int -> int) -> int -> int) = fun (f : int -> int) (x : int) -> f x
