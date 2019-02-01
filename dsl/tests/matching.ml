type int' = int

type record = {one: int; two: int'; three: int}

let a =
  let f x = x in
  match f with x -> x 15 | _ -> 12

let b ({one= one'; two} : record) =
  let not_one = match one' with 0 -> 1 | _ -> 0 in
  let two =
    match two with 1 | 2 | 3 | 4 -> 0 | 5 | 6 | 7 | 8 -> 1 | x -> x
  in
  (not_one, two)

let c record =
  match record with
  | {one= 1; _} -> record
  | {two= 2; three} -> {one= 1; two= 2; three}
  | _ -> {one= 1; two= 2; three= 3}

let d x =
  match x with
  | _, (1 | 2 | 3) -> 1
  | (f, ({one= 1; two} | {three= two})), x -> f two x
  | (f, _), _ -> f 94 84
