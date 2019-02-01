let (a : ('b -> 'b) -> 'b -> 'b) = fun (f : 'b -> 'b) -> f

let (b : '_a49) =
  let (f : '_a29 -> '_a29) = fun (x : '_a29) -> x in
  f f f

let (c : (int -> int) -> int -> int) = fun (f : int -> int) (x : int) -> f x
