let (a : ('b -> 'b) -> 'b -> 'b) = fun (f : 'b -> 'b) -> f

let (b : 'a -> '_a43) =
  let (f : 'a -> '_a39) = fun (x : 'a -> '_a43) -> x in
  f f f

let (c : (int -> int) -> int -> int) = fun (f : int -> int) (x : int) -> f x
