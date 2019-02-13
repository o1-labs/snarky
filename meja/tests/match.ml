let x (y : int) (z : bool) = match y with i -> z

let y (f : int -> int -> int -> _) = match (1, 2, 3) with i, j, k -> f i j k

type t = {a: int; b: bool}

type u = {f: int -> int}

let z x {f; _} = match x with {a= x; b; _} -> f x

let a (x : t) {f; _} = match x with {a= x; b; _} -> f x

let b = match {f= (fun x -> 12)} with {f; _} -> f
