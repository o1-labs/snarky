let x (y : int) (z : bool) = match y with i -> z

let y (f : int -> int -> int -> _) = match (1, 2, 3) with i, j, k -> f i j k
