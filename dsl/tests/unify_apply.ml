let (a : ('b -> 'b) -> 'b -> 'b) = fun (f : 'b -> 'b) -> f

let (b : '_a47) =
  let (f : '_a27 -> '_a27) = fun (x : '_a27) -> x in
  f f f

let (c : (int -> int) -> int -> int) = fun (f : int -> int) (x : int) -> f x
